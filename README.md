# ini-doc

A Rust library for reading and writing INI files, built on top of [`ini-roundtrip`](https://crates.io/crates/ini-roundtrip). It preserves comments (doc info), supports ordered sections and keys by default, and provides a convenient API for editing and serialization.

## Features
- **Comment Preservation**: Retains and exposes all comments (doc info) in INI files.
- **Ordered Sections & Keys**: Uses `indexmap` by default to preserve the order of sections and keys.
- **Easy-to-use API**: Simple interface for reading, editing, and serializing INI files.
- **Doc Info Preservation**: Preserves as much doc (comment) information as possible.
- **No Inline Comment Support**: Inline comments are not supported.

## Installation
Add the following to your `Cargo.toml`:

```toml
[dependencies]
ini-doc = "0.1"
```


## Dependencies
- [ini-roundtrip](https://crates.io/crates/ini-roundtrip)
- [indexmap](https://crates.io/crates/indexmap)
- [thiserror](https://crates.io/crates/thiserror)

## License
MIT License

---

For more examples and API details, see the source code or documentation.
