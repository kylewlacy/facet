use std::io::{self, Write};

use alloc::collections::VecDeque;
use facet_core::{Def, Facet, ScalarAffinity, StructKind};
use facet_reflect::{HasFields, Peek};

#[derive(Debug)]
enum SerializeOp<'mem, 'facet_lifetime> {
    Value(Peek<'mem, 'facet_lifetime>),
    Array {
        first: bool,
        items: VecDeque<Peek<'mem, 'facet_lifetime>>,
    },
    Object {
        first: bool,
        entries: VecDeque<(
            ObjectKey<'mem, 'facet_lifetime>,
            Peek<'mem, 'facet_lifetime>,
        )>,
    },
}

#[derive(Debug)]
enum ObjectKey<'mem, 'facet_lifetime> {
    String(&'static str),
    Value(Peek<'mem, 'facet_lifetime>),
}

/// Serializes a value to JSON
pub fn to_string<'a, T: Facet<'a>>(value: &T) -> String {
    peek_to_string(&Peek::new(value))
}

/// Serializes a Peek instance to JSON
pub fn peek_to_string(peek: &Peek<'_, '_>) -> String {
    let mut output = Vec::new();
    peek_to_writer(peek, &mut output).unwrap();
    String::from_utf8(output).unwrap()
}

/// Serializes a value to a writer in JSON format
pub fn to_writer<'a, T: Facet<'a>, W: Write>(value: &T, writer: &mut W) -> io::Result<()> {
    peek_to_writer(&Peek::new(value), writer)
}

/// Serializes a Peek instance to a writer in JSON format
pub fn peek_to_writer<W: Write>(peek: &Peek<'_, '_>, writer: &mut W) -> io::Result<()> {
    let mut queue = VecDeque::from_iter([SerializeOp::Value(*peek)]);

    while let Some(op) = queue.pop_front() {
        let mut value = match op {
            SerializeOp::Value(value) => value,
            SerializeOp::Array { first, mut items } => {
                if first {
                    write!(writer, "[").unwrap();
                }

                let Some(next_item) = items.pop_front() else {
                    // Finished writing list, go to the next op
                    write!(writer, "]").unwrap();
                    continue;
                };

                if !first {
                    write!(writer, ",").unwrap();
                }

                queue.push_front(SerializeOp::Array {
                    first: false,
                    items,
                });
                next_item
            }
            SerializeOp::Object { first, mut entries } => {
                if first {
                    write!(writer, "{{").unwrap();
                }

                let Some((key, entry)) = entries.pop_front() else {
                    write!(writer, "}}").unwrap();
                    continue;
                };

                if !first {
                    write!(writer, ",").unwrap();
                }

                match key {
                    ObjectKey::String(key) => {
                        write_json_string(writer, key).unwrap();
                    }
                    ObjectKey::Value(peek) => {
                        if let Some(s) = peek.as_str() {
                            write_json_string(writer, s).unwrap();
                        } else {
                            let s = peek.to_string();
                            write_json_string(writer, &s).unwrap();
                        }
                    }
                }

                write!(writer, ":").unwrap();

                queue.push_front(SerializeOp::Object {
                    first: false,
                    entries,
                });
                entry
            }
        };

        // Handle options and wrappers
        if let Some(inner) = innermost_option_peek(value) {
            value = inner;
        } else {
            // Got None value, so write "null" and go to the next op
            write!(writer, "null").unwrap();
            continue;
        }

        let shape = value.shape();
        if let Def::Scalar(scalar_def) = shape.def {
            match scalar_def.affinity {
                ScalarAffinity::Number(_) => {
                    // Write numbers directly.
                    // TODO: Figure out a better way to do this. Ideally, this
                    // should prevent invalid JSON numbers, but also allow
                    // things beyond floats
                    write!(writer, "{value}").unwrap();
                }
                ScalarAffinity::Boolean(_) => {
                    let Ok(&boolean) = value.get::<bool>() else {
                        panic!("shape {shape} has a boolean affinity, but could not get boolean");
                    };
                    if boolean {
                        write!(writer, "true").unwrap();
                    } else {
                        write!(writer, "false").unwrap();
                    }
                }
                ScalarAffinity::Empty(_) => {
                    write!(writer, "null").unwrap();
                }
                _ => {
                    if let Some(s) = value.as_str() {
                        write_json_string(writer, s).unwrap();
                    } else {
                        let s = value.to_string();
                        write_json_string(writer, &s).unwrap();
                    }
                }
            }
        } else if let Some(s) = value.as_str() {
            write_json_string(writer, s).unwrap();
        } else if let Ok(peek_tuple) = value.into_tuple() {
            let items = peek_tuple.fields().map(|(_, field)| field).collect();
            queue.push_front(SerializeOp::Array { first: true, items });
        } else if let Ok(peek_list) = value.into_list_like() {
            let items = peek_list.iter().collect();
            queue.push_front(SerializeOp::Array { first: true, items });
        } else if let Ok(peek_struct) = value.into_struct() {
            match peek_struct.ty().kind {
                StructKind::Unit => {
                    // Unit struct, serialize as null
                    write!(writer, "null").unwrap();
                    continue;
                }
                StructKind::TupleStruct => {
                    // Tuple struct, serialize as an array
                    let items = peek_struct
                        .fields_for_serialize()
                        .map(|(_, value)| value)
                        .collect();
                    queue.push_front(SerializeOp::Array { first: true, items });
                }
                StructKind::Struct => {
                    // Serialize struct as object
                    let entries = peek_struct
                        .fields_for_serialize()
                        .map(|(key, value)| (ObjectKey::String(key.name), value))
                        .collect();
                    queue.push_front(SerializeOp::Object {
                        first: true,
                        entries,
                    });
                }
                _ => unimplemented!("unsupported struct kind"),
            }
        } else if let Ok(peek_enum) = value.into_enum() {
            let variant = peek_enum.active_variant().unwrap();
            match variant.data.kind {
                StructKind::Unit => {
                    // Unit enum variant, serialize as a string based on the
                    // variant name
                    write_json_string(writer, variant.name).unwrap();
                }
                StructKind::Tuple if variant.data.fields.len() == 1 => {
                    // Single-element tuple variant, serialize the inner
                    // variant transparently

                    write!(writer, "{{").unwrap();
                    write_json_string(writer, variant.name).unwrap();
                    write!(writer, ":").unwrap();
                    queue.push_front(SerializeOp::Object {
                        first: false,
                        entries: VecDeque::new(),
                    });

                    let inner = peek_enum.field(0).unwrap().unwrap();
                    queue.push_front(SerializeOp::Value(inner));
                }
                StructKind::Tuple => {
                    // Normal tuple variant, serialize the variant as an array

                    write!(writer, "{{").unwrap();
                    write_json_string(writer, variant.name).unwrap();
                    write!(writer, ":").unwrap();
                    queue.push_front(SerializeOp::Object {
                        first: false,
                        entries: VecDeque::new(),
                    });

                    let items = peek_enum
                        .fields_for_serialize()
                        .map(|(_, item)| item)
                        .collect();
                    queue.push_front(SerializeOp::Array { first: true, items });
                }
                StructKind::Struct => {
                    // Struct variant, serialize as an object
                    // Normal tuple variant, serialize the variant as an array

                    write!(writer, "{{").unwrap();
                    write_json_string(writer, variant.name).unwrap();
                    write!(writer, ":").unwrap();
                    queue.push_front(SerializeOp::Object {
                        first: false,
                        entries: VecDeque::new(),
                    });

                    let entries = peek_enum
                        .fields_for_serialize()
                        .map(|(field, value)| (ObjectKey::String(field.name), value))
                        .collect();
                    queue.push_front(SerializeOp::Object {
                        first: true,
                        entries,
                    });
                }
                kind => unimplemented!("unhandled enum variant kind for shape {shape}: {kind:?}"),
            }
        } else if let Ok(map) = value.into_map() {
            let entries = map
                .iter()
                .map(|(key, value)| (ObjectKey::Value(key), value))
                .collect();
            queue.push_front(SerializeOp::Object {
                first: true,
                entries,
            });
        } else {
            todo!("unhandled shape {shape}: {:?}", shape.def);
        }
    }

    Ok(())
}

fn innermost_option_peek<'mem, 'facet_lifetime>(
    mut peek: Peek<'mem, 'facet_lifetime>,
) -> Option<Peek<'mem, 'facet_lifetime>> {
    loop {
        // Resolve the innermost value of any wrapper types (references, etc.)
        peek = peek.innermost_peek();

        let Ok(peek_option) = peek.into_option() else {
            // Value is not an option, so we're done
            return Some(peek);
        };

        // Try to get the Some value, then repeat
        peek = peek_option.value()?;
    }
}

/// Properly escapes and writes a JSON string
fn write_json_string<W: Write>(writer: &mut W, s: &str) -> io::Result<()> {
    writer.write_all(b"\"")?;

    for c in s.chars() {
        write_json_escaped_char(writer, c)?;
    }

    writer.write_all(b"\"")
}

/// Writes a single JSON escaped character
fn write_json_escaped_char<W: Write>(writer: &mut W, c: char) -> io::Result<()> {
    match c {
        '"' => writer.write_all(b"\\\""),
        '\\' => writer.write_all(b"\\\\"),
        '\n' => writer.write_all(b"\\n"),
        '\r' => writer.write_all(b"\\r"),
        '\t' => writer.write_all(b"\\t"),
        '\u{08}' => writer.write_all(b"\\b"),
        '\u{0C}' => writer.write_all(b"\\f"),
        c if c.is_control() => {
            let mut buf = [0; 6];
            let s = format!("{:04x}", c as u32);
            buf[0] = b'\\';
            buf[1] = b'u';
            buf[2] = s.as_bytes()[0];
            buf[3] = s.as_bytes()[1];
            buf[4] = s.as_bytes()[2];
            buf[5] = s.as_bytes()[3];
            writer.write_all(&buf)
        }
        c => {
            let mut buf = [0; 4];
            let len = c.encode_utf8(&mut buf).len();
            writer.write_all(&buf[..len])
        }
    }
}
