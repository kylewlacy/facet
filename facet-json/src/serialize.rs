use std::io::{self, Write};

use alloc::collections::VecDeque;
use facet_core::{Def, Facet, ScalarAffinity, StructKind};
use facet_reflect::{
    Peek, PeekEnum, PeekListLike, PeekListLikeIter, PeekMap, PeekMapIter, PeekStruct, PeekTuple,
};

enum SerializeOp<'mem, 'facet_lifetime> {
    Value(Peek<'mem, 'facet_lifetime>),
    Array {
        first: bool,
        items: ItemIter<'mem, 'facet_lifetime>,
    },
    Object {
        first: bool,
        entries: EntryIter<'mem, 'facet_lifetime>,
    },
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

                let Some(next_item) = items.next() else {
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

                let Some((key, entry)) = entries.next() else {
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
                    // Empty - write as null
                    write!(writer, "null").unwrap();
                }
                _ => {
                    // Otherwise, stringify the value
                    if let Some(s) = value.as_str() {
                        write_json_string(writer, s).unwrap();
                    } else {
                        let s = value.to_string();
                        write_json_string(writer, &s).unwrap();
                    }
                }
            }
        } else if let Some(s) = value.as_str() {
            // String value
            // TODO: Should strings be scalars? It feels like they should...
            write_json_string(writer, s).unwrap();
        } else if let Ok(peek_tuple) = value.into_tuple() {
            // Encode tuple as an array
            queue.push_front(SerializeOp::Array {
                first: true,
                items: ItemIter::new_tuple(peek_tuple),
            });
        } else if let Ok(peek_list) = value.into_list_like() {
            // Write any list-like value as an array
            queue.push_front(SerializeOp::Array {
                first: true,
                items: ItemIter::new_list_like(peek_list),
            });
        } else if let Ok(peek_struct) = value.into_struct() {
            match peek_struct.ty().kind {
                StructKind::Unit => {
                    // Unit struct, serialize as null
                    write!(writer, "null").unwrap();
                    continue;
                }
                StructKind::TupleStruct => {
                    // Tuple struct, serialize as an array
                    let items = ItemIter::new_struct(peek_struct);
                    queue.push_front(SerializeOp::Array { first: true, items });
                }
                StructKind::Struct => {
                    // Serialize struct as object
                    queue.push_front(SerializeOp::Object {
                        first: true,
                        entries: EntryIter::new_struct(peek_struct),
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
                        entries: EntryIter::Empty,
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
                        entries: EntryIter::Empty,
                    });

                    let items = ItemIter::new_enum(peek_enum);
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
                        entries: EntryIter::Empty,
                    });

                    queue.push_front(SerializeOp::Object {
                        first: true,
                        entries: EntryIter::new_enum(peek_enum),
                    });
                }
                kind => unimplemented!("unhandled enum variant kind for shape {shape}: {kind:?}"),
            }
        } else if let Ok(map) = value.into_map() {
            queue.push_front(SerializeOp::Object {
                first: true,
                entries: EntryIter::new_map(map),
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

enum ItemIter<'mem, 'facet_lifetime> {
    ListLike(PeekListLikeIter<'mem, 'facet_lifetime>),
    Tuple {
        tuple: PeekTuple<'mem, 'facet_lifetime>,
        next_field: usize,
    },
    Struct {
        struct_: PeekStruct<'mem, 'facet_lifetime>,
        next_field: usize,
    },
    Enum {
        enum_: PeekEnum<'mem, 'facet_lifetime>,
        next_field: usize,
    },
}

impl<'mem, 'facet_lifetime> ItemIter<'mem, 'facet_lifetime> {
    fn new_list_like(value: PeekListLike<'mem, 'facet_lifetime>) -> Self {
        Self::ListLike(value.iter())
    }

    fn new_tuple(value: PeekTuple<'mem, 'facet_lifetime>) -> Self {
        Self::Tuple {
            tuple: value,
            next_field: 0,
        }
    }

    fn new_struct(value: PeekStruct<'mem, 'facet_lifetime>) -> Self {
        Self::Struct {
            struct_: value,
            next_field: 0,
        }
    }

    fn new_enum(value: PeekEnum<'mem, 'facet_lifetime>) -> Self {
        Self::Enum {
            enum_: value,
            next_field: 0,
        }
    }
}

impl<'mem, 'facet_lifetime> Iterator for ItemIter<'mem, 'facet_lifetime> {
    type Item = Peek<'mem, 'facet_lifetime>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::ListLike(iter) => iter.next(),
            Self::Tuple { tuple, next_field } => {
                let value = tuple.field(*next_field)?;
                *next_field += 1;
                Some(value)
            }
            Self::Struct {
                struct_,
                next_field,
            } => {
                let value = struct_.field(*next_field).ok()?;
                *next_field += 1;
                Some(value)
            }
            Self::Enum { enum_, next_field } => {
                let value = enum_.field(*next_field).unwrap()?;
                *next_field += 1;
                Some(value)
            }
        }
    }
}

#[derive(Debug)]
enum ObjectKey<'mem, 'facet_lifetime> {
    String(&'static str),
    Value(Peek<'mem, 'facet_lifetime>),
}

enum EntryIter<'mem, 'facet_lifetime> {
    Empty,
    Map(PeekMapIter<'mem, 'facet_lifetime>),
    Struct {
        struct_: PeekStruct<'mem, 'facet_lifetime>,
        next_field: usize,
    },
    Enum {
        enum_: PeekEnum<'mem, 'facet_lifetime>,
        next_field: usize,
    },
}

impl<'mem, 'facet_lifetime> EntryIter<'mem, 'facet_lifetime> {
    fn new_map(value: PeekMap<'mem, 'facet_lifetime>) -> Self {
        Self::Map(value.iter())
    }

    fn new_struct(value: PeekStruct<'mem, 'facet_lifetime>) -> Self {
        Self::Struct {
            struct_: value,
            next_field: 0,
        }
    }

    fn new_enum(value: PeekEnum<'mem, 'facet_lifetime>) -> Self {
        Self::Enum {
            enum_: value,
            next_field: 0,
        }
    }
}

impl<'mem, 'facet_lifetime> Iterator for EntryIter<'mem, 'facet_lifetime> {
    type Item = (
        ObjectKey<'mem, 'facet_lifetime>,
        Peek<'mem, 'facet_lifetime>,
    );

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Empty => None,
            Self::Map(iter) => {
                let (key, value) = iter.next()?;
                Some((ObjectKey::Value(key), value))
            }
            Self::Struct {
                struct_,
                next_field,
            } => {
                let value = struct_.field(*next_field).ok()?;
                let field = struct_.ty().fields[*next_field];
                *next_field += 1;

                Some((ObjectKey::String(field.name), value))
            }
            Self::Enum { enum_, next_field } => {
                let value = enum_.field(*next_field).unwrap()?;
                let variant = enum_.active_variant().unwrap();
                let field = variant.data.fields[*next_field];
                *next_field += 1;

                Some((ObjectKey::String(field.name), value))
            }
        }
    }
}
