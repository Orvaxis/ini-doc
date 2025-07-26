use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
    str::FromStr,
};

extern crate ini_roundtrip as ini_engine;
use indexmap::IndexMap as Map;

use thiserror::Error;

use ini_engine::Item;

/// Wraps section and property values after parsing
/// Records their preceding documentation content and line numbers
#[derive(Clone)]
pub struct ReadonlyDocument<'a, T> {
    // Each line of documentation content before the data line
    doc_texts: Vec<&'a str>,
    line_num: usize,
    data: T,
}

/// Wraps section and property values for editing
/// Extends to record their preceding documentation content
#[derive(Clone)]
pub struct EditableDocument<T> {
    // Each line of documentation content before the data line
    doc_texts: Vec<String>,
    data: T,
}

impl<T> EditableDocument<T> {
    pub fn new(data: T, doc_texts: Vec<String>) -> Self {
        Self { doc_texts, data }
    }
}

impl<S1, S2> From<ReadonlyDocument<'_, S1>> for EditableDocument<S2>
where
    S1: Into<S2>,
{
    fn from(value: ReadonlyDocument<'_, S1>) -> Self {
        EditableDocument {
            doc_texts: value.doc_texts.iter().map(|s| s.to_string()).collect(),
            data: value.data.into(),
        }
    }
}

/// For backward compatibility, keep the original Document type alias
pub type Document<T> = EditableDocument<T>;

impl<'a, T> ReadonlyDocument<'a, T> {
    pub fn new(data: T, line_num: usize, doc_texts: Vec<&'a str>) -> Self {
        Self {
            doc_texts,
            line_num,
            data,
        }
    }

    pub fn doc_texts(&self) -> &[&'a str] {
        &self.doc_texts
    }

    pub fn line_num(&self) -> usize {
        self.line_num
    }

    /// Convert to an editable structure by reference
    pub fn to_editable(&self) -> EditableDocument<T>
    where
        T: Clone,
    {
        EditableDocument::from(self.clone())
    }
}

impl<T> EditableDocument<T> {
    pub fn doc_texts(&self) -> &[String] {
        &self.doc_texts
    }

    pub fn doc_texts_mut(&mut self) -> &mut Vec<String> {
        &mut self.doc_texts
    }

    pub fn set_doc_texts(&mut self, doc_texts: Vec<String>) {
        self.doc_texts = doc_texts;
    }
}

impl<T: Display> Display for ReadonlyDocument<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for doc_line in &self.doc_texts {
            writeln!(f, "{}", doc_line)?;
        }
        write!(f, "{}", self.data)
    }
}

impl<T: Display> Display for EditableDocument<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for doc_line in &self.doc_texts {
            writeln!(f, "{}", doc_line)?;
        }
        write!(f, "{}", self.data)
    }
}

impl<T> Deref for ReadonlyDocument<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> DerefMut for EditableDocument<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl<T> Deref for EditableDocument<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<S> From<PropertyValue<S>> for EditableDocument<PropertyValue<S>> {
    fn from(property_value: PropertyValue<S>) -> Self {
        EditableDocument {
            data: property_value,
            doc_texts: vec![],
        }
    }
}

impl From<PropertyValue<&str>> for EditableDocument<PropertyValue<String>> {
    fn from(prop: PropertyValue<&str>) -> Self {
        EditableDocument {
            data: PropertyValue {
                value: prop.value.map(|s| s.to_string()),
            },
            doc_texts: vec![],
        }
    }
}
pub type ReadonlyPropertyDocument<'a> = ReadonlyDocument<'a, PropertyValue<&'a str>>;
pub type ReadonlySectionDocument<'a> = ReadonlyDocument<'a, ReadonlyProperties<'a>>;

pub type EditablePropertyDocument = EditableDocument<PropertyValue<String>>;
pub type EditableSectionDocument = EditableDocument<Properties>;

pub type PropertyDocument = EditablePropertyDocument;
pub type SectionDocument = EditableSectionDocument;

#[derive(Clone)]
pub struct PropertyValue<S> {
    pub value: Option<S>,
}

// Allow conversion from PropertyValue<&str> to PropertyValue<String>
impl From<PropertyValue<&str>> for PropertyValue<String> {
    fn from(prop: PropertyValue<&str>) -> Self {
        PropertyValue {
            value: prop.value.map(|s| s.to_string()),
        }
    }
}

impl<S: Display> Display for PropertyValue<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(value) = &self.value {
            write!(f, "{}", value)
        } else {
            // If there is no value, output nothing (for properties with only a key name)
            Ok(())
        }
    }
}

#[derive(Clone)]
pub struct Properties {
    inner: Map<String, PropertyDocument>,
}

impl Display for Properties {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (key, doc) in &self.inner {
            for doc_line in doc.doc_texts() {
                writeln!(f, "{}", doc_line)?;
            }
            if let Some(value) = &doc.value {
                writeln!(f, "{}={}", key, value)?;
            } else {
                writeln!(f, "{}", key)?;
            }
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct ReadonlyProperties<'a> {
    inner: Map<&'a str, ReadonlyPropertyDocument<'a>>,
}

impl<'a> Display for ReadonlyProperties<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (key, doc) in &self.inner {
            for doc_line in doc.doc_texts() {
                writeln!(f, "{}", doc_line)?;
            }
            if let Some(value) = &doc.value {
                writeln!(f, "{}={}", key, value)?;
            } else {
                writeln!(f, "{}", key)?;
            }
        }
        Ok(())
    }
}

impl<'a> ReadonlyProperties<'a> {
    pub fn new() -> Self {
        Self { inner: Map::new() }
    }

    pub fn get(&self, key: &str) -> Option<&ReadonlyPropertyDocument<'a>> {
        self.inner.get(key)
    }

    pub fn iter(
        &'a self,
    ) -> impl Iterator<Item = (&'a str, &'a ReadonlyPropertyDocument<'a>)> + 'a {
        self.inner.iter().map(|(k, v)| (*k, v))
    }

    pub fn into_iter(self) -> impl Iterator<Item = (&'a str, ReadonlyPropertyDocument<'a>)> {
        self.inner.into_iter()
    }
}

impl Properties {
    pub fn new() -> Self {
        Self { inner: Map::new() }
    }

    fn insert(&mut self, key: &str, value: PropertyDocument) -> Option<PropertyDocument> {
        self.inner.insert(key.to_owned(), value)
    }

    pub fn get(&self, key: &str) -> Option<&PropertyDocument> {
        self.inner.get(key)
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut PropertyDocument> {
        self.inner.get_mut(key)
    }

    pub fn get_value<T: FromStr>(&self, key: &str) -> Result<Option<T>, T::Err> {
        if let Some(property_doc) = self.get(key) {
            if let Some(value_str) = &property_doc.value {
                return Ok(Some(value_str.parse()?));
            } else {
                // Only key name, no value
                return Ok(None);
            }
        }
        Ok(None)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, &PropertyDocument)> {
        self.inner.iter()
    }

    pub fn into_iter(self) -> impl Iterator<Item = (String, PropertyDocument)> {
        self.inner.into_iter()
    }

    pub fn set(&mut self, key: &str, value: PropertyValue<String>) -> Option<PropertyDocument> {
        let value = EditableDocument::from(value);
        self.insert(key, value)
    }

    pub fn remove(&mut self, key: &str) -> Option<PropertyDocument> {
        self.inner.shift_remove(key)
    }

    pub fn remove_at(&mut self, idx: usize) -> Option<(String, PropertyDocument)> {
        self.inner.shift_remove_index(idx)
    }

    pub fn replace_at(
        &mut self,
        idx: usize,
        key: &str,
        value: PropertyDocument,
    ) -> Option<(String, PropertyDocument)> {
        let entry = self.inner.get_index_entry(idx);
        if let Some(mut entry) = entry {
            use indexmap::map::MutableEntryKey;
            let old_key = std::mem::replace(entry.key_mut(), key.to_string());
            let old_value = std::mem::replace(entry.get_mut(), value);
            return Some((old_key, old_value));
        } else {
            self.insert(key, value);
        }
        None
    }

    pub fn contains_key(&self, key: &str) -> bool {
        self.inner.contains_key(key)
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }
}

impl From<ReadonlyProperties<'_>> for Properties {
    fn from(readonly_properties: ReadonlyProperties) -> Self {
        let mut properties = Properties::new();

        for (prop_key, readonly_prop) in readonly_properties.into_iter() {
            let editable_prop = EditableDocument::from(readonly_prop);
            properties.inner.insert(prop_key.to_owned(), editable_prop);
        }

        properties
    }
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("parse error: {0}")]
    FailedParse(String),
}

#[derive(Error, Debug)]
pub enum ConfigError {
    #[error("section {section:?} not found")]
    SectionNotFound { section: Option<String> },
    #[error("property '{property}' not found in section {section:?}")]
    PropertyNotFound {
        section: Option<String>,
        property: String,
    },
}

#[derive(Error, Debug)]
pub enum IniError {
    #[error(transparent)]
    Parse(#[from] ParseError),
    #[error(transparent)]
    Config(#[from] ConfigError),
}

pub struct ReadonlyIni<'a> {
    sections: Map<Option<&'a str>, ReadonlyDocument<'a, ReadonlyProperties<'a>>>,
}

impl<'a> ReadonlyIni<'a> {
    pub fn section(
        &'a self,
        name: Option<&'a str>,
    ) -> Option<&'a ReadonlyDocument<'a, ReadonlyProperties<'a>>> {
        self.sections.get(&name)
    }

    pub fn sections(
        &'a self,
    ) -> &'a Map<Option<&'a str>, ReadonlyDocument<'a, ReadonlyProperties<'a>>> {
        &self.sections
    }

    pub fn get_property(
        &'a self,
        section_name: Option<&'a str>,
        key: &str,
    ) -> Option<&'a ReadonlyPropertyDocument<'a>> {
        self.section(section_name)
            .and_then(|section| section.get(key))
    }
}

impl<'a> From<ReadonlyIni<'a>> for Ini {
    fn from(readonly_ini: ReadonlyIni<'a>) -> Self {
        let editable_sections = readonly_ini
            .sections
            .into_iter()
            .map(|(section_key, readonly_section)| {
                let editable_section = EditableDocument::from(readonly_section);
                (section_key.map(|s| s.to_owned()), editable_section)
            })
            .collect();

        Ini {
            sections: editable_sections,
        }
    }
}

impl<'a> TryFrom<Vec<Item<'a>>> for ReadonlyIni<'a> {
    type Error = IniError;

    fn try_from(value: Vec<Item<'a>>) -> Result<Self, Self::Error> {
        let mut sections = Map::new();
        // Create initial structure for global section
        sections.insert(
            None,
            ReadonlyDocument::new(ReadonlyProperties::new(), 0, vec![]),
        );
        let mut current_section: Option<&'a str> = None;
        let mut pending_docs: Vec<&str> = Vec::new();
        let mut line_num = 0;
        for item in value {
            if !matches!(item, Item::SectionEnd) {
                line_num += 1;
            }
            match item {
                Item::Blank { raw } => {
                    pending_docs.push(raw);
                }
                Item::Comment { raw, .. } => {
                    pending_docs.push(raw);
                }
                Item::Section { name, raw: _ } => {
                    // Save current pending_docs for new section
                    let section_docs = if !pending_docs.is_empty() {
                        pending_docs.drain(..).collect()
                    } else {
                        vec![]
                    };

                    // Create new section, record current line number
                    let new_section =
                        ReadonlyDocument::new(ReadonlyProperties::new(), line_num, section_docs);
                    sections.insert(Some(name), new_section);
                    current_section = Some(name);
                }
                Item::Property { key, val, raw: _ } => {
                    let section_key = current_section.clone();

                    // Create PropertyValue and PropertyDocument with line number
                    let property_value = PropertyValue { value: val };

                    let docs = if !pending_docs.is_empty() {
                        pending_docs.drain(..).collect()
                    } else {
                        vec![]
                    };

                    let property_doc = ReadonlyDocument::new(property_value, line_num, docs);

                    // Insert property to corresponding section
                    if let Some(section_doc) = sections.get_mut(&section_key) {
                        section_doc.data.inner.insert(key, property_doc);
                    }
                }
                Item::SectionEnd => {
                    // SectionEnd does not need special handling
                }
                Item::Error(err) => {
                    return Err(ParseError::FailedParse(err.to_string()).into());
                }
            }
        }

        Ok(ReadonlyIni { sections })
    }
}

pub struct Ini {
    sections: Map<Option<String>, SectionDocument>,
}

impl Ini {
    /// Create a new INI instance, preset a general section (None)
    pub fn new() -> Self {
        let mut sections = Map::new();
        // Preset a general section
        sections.insert(None, SectionDocument::new(Properties::new(), vec![]));
        Self { sections }
    }

    /// Ensure a named section exists in the INI document.
    ///
    /// If the section does not exist, it will be created. The general section (None) cannot be created explicitly.
    ///
    /// # Arguments
    /// * `section_name` - The name of the section to ensure exists.
    pub fn set_section(&mut self, section_name: &str) {
        let section_key = Some(section_name.to_string());
        self.sections
            .entry(section_key)
            .or_insert_with(|| SectionDocument::new(Properties::new(), vec![]));
    }

    /// Set a property value in a section.
    ///
    /// The section must already exist. If the property does not exist, it will be created; if it exists, it will be overwritten.
    /// If `value` is `Some`, sets the key-value pair; if `None`, sets only the key name (no value).
    ///
    /// # Arguments
    /// * `section_name` - The section name (None for general section).
    /// * `key` - The property key.
    /// * `value` - The property value (optional).
    ///
    /// # Errors
    /// Returns `ConfigError::SectionNotFound` if the section does not exist.
    pub fn set_property<T: ToString>(
        &mut self,
        section_name: Option<&str>,
        key: &str,
        value: Option<T>,
    ) -> Result<(), ConfigError> {
        let section_key = section_name.map(|s| s.to_string());

        // Get section, return error if not exists
        let properties =
            self.sections
                .get_mut(&section_key)
                .ok_or_else(|| ConfigError::SectionNotFound {
                    section: section_name.map(|s| s.to_string()),
                })?;

        // Set property
        let property_value = PropertyValue {
            value: value.map(|v| v.to_string()),
        };
        properties.set(key, property_value);

        Ok(())
    }

    /// Set documentation comments for a section.
    ///
    /// # Arguments
    /// * `section_name` - The section name (None for general section).
    /// * `doc_texts` - Documentation lines to associate with the section.
    ///
    /// # Errors
    /// Returns `ConfigError::SectionNotFound` if the section does not exist.
    pub fn set_section_doc(
        &mut self,
        section_name: Option<&str>,
        doc_texts: Vec<String>,
    ) -> Result<(), ConfigError> {
        let section_key = section_name.map(|s| s.to_string());

        // Get section, error if not exists
        let section_doc =
            self.sections
                .get_mut(&section_key)
                .ok_or_else(|| ConfigError::SectionNotFound {
                    section: section_name.map(|s| s.to_string()),
                })?;

        // Update documentation
        section_doc.doc_texts = doc_texts;
        Ok(())
    }

    /// Set documentation comments for a property.
    ///
    /// # Arguments
    /// * `section_name` - The section name (None for general section).
    /// * `key` - The property key.
    /// * `doc_texts` - Documentation lines to associate with the property.
    ///
    /// # Errors
    /// Returns `ConfigError::SectionNotFound` if the section does not exist, or `ConfigError::PropertyNotFound` if the property does not exist.
    pub fn set_property_doc(
        &mut self,
        section_name: Option<&str>,
        key: &str,
        doc_texts: Vec<String>,
    ) -> Result<(), ConfigError> {
        let section_key = section_name.map(|s| s.to_string());

        // Get properties in the section, error if not exists
        let properties =
            self.sections
                .get_mut(&section_key)
                .ok_or_else(|| ConfigError::SectionNotFound {
                    section: section_name.map(|s| s.to_string()),
                })?;

        // Get property
        let property = properties
            .get_mut(key)
            .ok_or(ConfigError::PropertyNotFound {
                section: section_name.map(|s| s.to_string()),
                property: key.to_string(),
            })?;

        property.set_doc_texts(doc_texts);
        Ok(())
    }

    /// Get the value of the specified section and key, parse to the specified type via FromStr
    pub fn get_value<T: FromStr>(
        &self,
        section_name: Option<&str>,
        key: &str,
    ) -> Result<Option<T>, T::Err> {
        let section_key = section_name.map(|s| s.to_string());

        if let Some(properties) = self.sections.get(&section_key) {
            return properties.get_value(key);
        }
        Ok(None)
    }

    /// Get the raw string value of the specified section and key
    pub fn get_string(&self, section_name: Option<&str>, key: &str) -> Option<&str> {
        let section_key = section_name.map(|s| s.to_string());

        if let Some(section_doc) = self.sections.get(&section_key) {
            if let Some(property_doc) = section_doc.data.get(key) {
                return property_doc.value.as_deref();
            }
        }
        None
    }

    /// Check if the specified property exists
    pub fn has_property(&self, section_name: Option<&str>, key: &str) -> bool {
        let section_key = section_name.map(|s| s.to_string());

        if let Some(section_doc) = self.sections.get(&section_key) {
            return section_doc.data.contains_key(key);
        }
        false
    }

    /// Remove the specified property
    pub fn remove_property(&mut self, section_name: Option<&str>, key: &str) -> bool {
        let section_key = section_name.map(|s| s.to_string());
        match self.sections.get_mut(&section_key) {
            Some(properties) => properties.remove(key).is_some(),
            None => false,
        }
    }

    /// Remove the specified section
    pub fn remove_section(&mut self, section_name: Option<&str>) -> bool {
        let section_key = section_name.map(|s| s.to_string());
        self.sections.shift_remove(&section_key).is_some()
    }

    /// Get all sections
    pub fn sections(&self) -> &Map<Option<String>, SectionDocument> {
        &self.sections
    }

    /// Get the specified section properties
    pub fn section(&self, name: Option<&str>) -> Option<&SectionDocument> {
        let key = name.map(|s| s.to_string());
        self.sections.get(&key)
    }

    /// Get a mutable reference to the specified section properties
    pub fn section_mut(&mut self, name: Option<&str>) -> Option<&mut SectionDocument> {
        let key = name.map(|s| s.to_string());
        self.sections.get_mut(&key)
    }
}

impl Display for Ini {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // First handle the global section (no name)
        // std::collections::HashMap orders are not guaranteed, so we handle the global section first
        if let Some(global_section) = self.sections.get(&None) {
            // Print documentation content of the global section
            for doc_line in &global_section.doc_texts {
                writeln!(f, "{}", doc_line)?;
            }
            // Print properties of the global section
            write!(f, "{}", global_section.data)?;
        }

        // Then handle named sections
        for (section_key, section_doc) in &self.sections {
            if section_key.is_none() {
                // Global section already handled
                continue;
            }

            // Print documentation content of the section
            for doc_line in &section_doc.doc_texts {
                writeln!(f, "{}", doc_line)?;
            }

            // Print section header
            if let Some(section_name) = section_key {
                writeln!(f, "[{}]", section_name)?;
            }

            // Print properties of the section
            write!(f, "{}", section_doc.data)?;
        }
        Ok(())
    }
}

impl FromStr for Ini {
    type Err = IniError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let items: Vec<Item> = ini_engine::Parser::new(s).collect();
        Self::try_from(items)
    }
}

impl TryFrom<Vec<Item<'_>>> for Ini {
    type Error = IniError;

    fn try_from(value: Vec<Item<'_>>) -> Result<Self, Self::Error> {
        // First parse to ReadonlyIni, then use From trait to convert to Ini
        let readonly_ini: ReadonlyIni = value.try_into()?;
        Ok(Ini::from(readonly_ini))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_item_structure() {
        // For debugging the structure of Item
        let content = r#"
; This is a comment
[section1]
; Property comment
key1=value1

[section2]
key2=value2
"#;

        // Let's see how ini_roundtrip parses
        let items = ini_engine::Parser::new(content).collect::<Vec<_>>();
        for item in items {
            println!("{:?}", item);
        }
    }

    #[test]
    fn test_parse_and_display() {
        let content = r#"
; Global comment
global_key=global_value

; section1 comment
[section1]
; key1 comment
key1=value1

; key2 comment
key2=value2

[section2]
key3=value3
"#;

        let ini: Ini = content.parse().expect("Parse failed");
        let result = ini.to_string();
        println!("Parse result:\n{}", result);
    }

    #[test]
    fn test_round_trip() {
        let content = r#"; Config file header comment

; Database config
[database]
; Host address
host=localhost
; Port number  
port=3306

user=admin

; Web service config
[web]
; Listen port
listen_port=8080
; Static directory
static_dir=/var/www
"#;

        let ini: Ini = content.parse().expect("Parse failed");
        let result = ini.to_string();

        // Verify that parsing the output again does not fail
        let _ini2: Ini = result.parse().expect("second parse failed");

        println!("Original content:\n{}", content);
        println!("Readonly result:\n{}", result);

        // Verify expected sections and properties
        assert!(
            ini.section(Some("database")).is_some(),
            "should contain database section"
        );
        assert!(
            ini.section(Some("web")).is_some(),
            "should contain web section"
        );

        let db_section = ini.section(Some("database")).unwrap();
        assert!(
            db_section.contains_key("host"),
            "database section should contain host"
        );
        assert!(
            db_section.contains_key("port"),
            "database section should contain port"
        );
        assert!(
            db_section.contains_key("user"),
            "database section should contain user"
        );
    }

    #[test]
    fn test_document_display() {
        // Test the Display implementation of Document<T>
        let property_value = PropertyValue {
            value: Some("test_value"),
        };

        let doc_texts = vec!["; This is a comment", "", "; Another comment"];

        let property_doc = ReadonlyDocument::new(property_value, 0, doc_texts);
        let result = property_doc.to_string();

        println!("PropertyDocument Display result:\n{}", result);
        assert!(result.contains("; This is a comment"));
        assert!(result.contains("; Another comment"));
        assert!(result.contains("test_value"));

        // Test the Display of Properties
        let mut properties = ReadonlyProperties::new();
        properties.inner.insert("key1", property_doc);

        let properties_result = properties.to_string();
        println!("Properties Display result:\n{}", properties_result);
        assert!(properties_result.contains("key1=test_value"));

        // Test the Display of SectionDocument
        let section_docs = vec!["; Section comment"];
        let section_doc = ReadonlyDocument::new(properties, 0, section_docs);

        let section_result = section_doc.to_string();
        println!("SectionDocument Display result:\n{}", section_result);
        assert!(section_result.contains("; Section comment"));
        assert!(section_result.contains("key1=test_value"));
    }

    #[test]
    fn test_original_content_preservation() {
        // Test original content preservation
        let content = r#"; This is an original comment, with special format
#This is a hash comment
global_key=global_value

; section1 comment, with spaces   
[section1]
;no space comment
key1=value1
    ; indented comment
key2=value2
"#;

        let ini: Ini = content.parse().expect("Parse failed");
        let result = ini.to_string();

        println!("Original content:\n{}", content);
        println!("Reconstructed result:\n{}", result);

        // Verify original comment format is preserved
        assert!(result.contains("; This is an original comment, with special format"));
        assert!(result.contains("#This is a hash comment"));
        assert!(result.contains("; section1 comment, with spaces   "));
        assert!(result.contains(";no space comment"));
        assert!(result.contains("; indented comment")); // Note: indentation may not be preserved

        // Verify property values are correct
        assert!(result.contains("global_key=global_value"));
        assert!(result.contains("key1=value1"));
        assert!(result.contains("key2=value2"));
    }

    #[test]
    fn test_ini_editing() {
        let mut ini = Ini::new();

        // Test setting section and properties
        ini.set_section("database");
        ini.set_section("flags");

        ini.set_property(Some("database"), "host", Some("localhost"))
            .unwrap();
        ini.set_property(Some("database"), "port", Some(3306))
            .unwrap();
        ini.set_property(None, "global_key", Some("global_value"))
            .unwrap();
        ini.set_property(Some("flags"), "debug", None::<String>)
            .unwrap(); // Only key name, no value

        // Test getting values
        assert_eq!(ini.get_string(Some("database"), "host"), Some("localhost"));
        assert_eq!(
            ini.get_value::<i32>(Some("database"), "port").unwrap(),
            Some(3306)
        );
        assert_eq!(ini.get_string(None, "global_key"), Some("global_value"));
        assert_eq!(ini.get_string(Some("flags"), "debug"), None); // Only key name, no value

        // Test if property exists
        assert!(ini.has_property(Some("database"), "host"));
        assert!(ini.has_property(Some("flags"), "debug"));
        assert!(!ini.has_property(Some("database"), "nonexistent"));

        // Test overwriting property
        ini.set_property(Some("database"), "host", Some("127.0.0.1"))
            .unwrap();
        assert_eq!(ini.get_string(Some("database"), "host"), Some("127.0.0.1"));

        // Test setting documentation
        ini.set_section_doc(
            Some("database"),
            vec![
                "; Database configuration".to_string(),
                "; Important configuration".to_string(),
            ],
        )
        .unwrap();
        ini.set_property_doc(
            Some("database"),
            "host",
            vec!["; Database host address".to_string()],
        )
        .unwrap();

        println!("Edited INI:\n{}", ini);

        // Verify documentation settings
        let result = ini.to_string();
        assert!(result.contains("; Database configuration"));
        assert!(result.contains("; Important configuration"));
        assert!(result.contains("; Database host address"));
    }

    #[test]
    fn test_ini_deletion() {
        let mut ini = Ini::new();

        // Add some data
        ini.set_section("section1");
        ini.set_section("section2");

        ini.set_property(Some("section1"), "key1", Some("value1"))
            .unwrap();
        ini.set_property(Some("section1"), "key2", Some("value2"))
            .unwrap();
        ini.set_property(Some("section2"), "key3", Some("value3"))
            .unwrap();

        // Test removing properties
        assert!(ini.remove_property(Some("section1"), "key1"));
        assert!(!ini.has_property(Some("section1"), "key1"));
        assert!(ini.has_property(Some("section1"), "key2"));

        // Test removing non-existent property
        assert!(!ini.remove_property(Some("section1"), "nonexistent"));

        // Test removing section
        assert!(ini.remove_section(Some("section2")));
        assert!(!ini.has_property(Some("section2"), "key3"));

        // Test removing non-existent section
        assert!(!ini.remove_section(Some("nonexistent")));
    }

    #[test]
    fn test_type_conversion() {
        let mut ini = Ini::new();

        // Set different types of values
        ini.set_section("config");

        ini.set_property(Some("config"), "port", Some(8080))
            .unwrap();
        ini.set_property(Some("config"), "timeout", Some(30.5))
            .unwrap();
        ini.set_property(Some("config"), "enabled", Some(true))
            .unwrap();
        ini.set_property(Some("config"), "name", Some("test_server"))
            .unwrap();

        // Test type conversion
        assert_eq!(
            ini.get_value::<i32>(Some("config"), "port").unwrap(),
            Some(8080)
        );
        assert_eq!(
            ini.get_value::<f64>(Some("config"), "timeout").unwrap(),
            Some(30.5)
        );
        assert_eq!(
            ini.get_value::<bool>(Some("config"), "enabled").unwrap(),
            Some(true)
        );
        assert_eq!(
            ini.get_value::<String>(Some("config"), "name").unwrap(),
            Some("test_server".to_string())
        );

        // Test type conversion failure
        assert!(ini.get_value::<i32>(Some("config"), "name").is_err());

        // Test non-existent value
        assert_eq!(
            ini.get_value::<i32>(Some("config"), "nonexistent").unwrap(),
            None
        );
    }

    #[test]
    fn test_edit_existing_ini() {
        // (Removed duplicated INI content block)
        let content = r#"; Original configuration
[database]
host=old_host
port=3306

[web]
port=8080
"#;

        let mut ini: Ini = content.parse().expect("Parse failed");

        // Modify existing configuration
        ini.set_property(Some("database"), "host", Some("new_host"))
            .unwrap();
        ini.set_section("cache");
        ini.set_property(Some("database"), "user", Some("admin"))
            .unwrap(); // Add new property
        ini.set_property(Some("cache"), "enabled", Some(true))
            .unwrap(); // Add new section

        // Modify documentation
        ini.set_property_doc(
            Some("database"),
            "host",
            vec![String::from("; New host address")],
        )
        .unwrap();

        let result = ini.to_string();
        println!("Modified configuration:\n{}", result);

        // Verify modification results
        assert_eq!(ini.get_string(Some("database"), "host"), Some("new_host"));
        assert_eq!(ini.get_string(Some("database"), "user"), Some("admin"));
        assert_eq!(
            ini.get_value::<bool>(Some("cache"), "enabled").unwrap(),
            Some(true)
        );
        assert!(result.contains("; New host address"));
    }

    #[test]
    fn test_doc_validation() {
        let mut ini = Ini::new();

        // Test that setting documentation for a nonexistent section should fail
        let result = ini.set_section_doc(
            Some("nonexistent"),
            vec![String::from("; Nonexistent section")],
        );
        assert!(result.is_err());
        match result.unwrap_err() {
            ConfigError::SectionNotFound { section } => {
                assert_eq!(section, Some("nonexistent".to_string()));
            }
            _ => panic!("Expected SectionNotFound error"),
        }

        // Test that setting documentation for a nonexistent property should fail
        ini.set_section("test");
        ini.set_property(Some("test"), "key1", Some("value1"))
            .unwrap();
        let result = ini.set_property_doc(
            Some("test"),
            "nonexistent",
            vec![String::from("; Nonexistent property")],
        );
        assert!(result.is_err());
        match result.unwrap_err() {
            ConfigError::PropertyNotFound { section, property } => {
                assert_eq!(section, Some("test".to_string()));
                assert_eq!(property, "nonexistent");
            }
            _ => panic!("Expected PropertyNotFound error"),
        }

        // Test that setting documentation for a property in a nonexistent section should fail
        let result = ini.set_property_doc(
            Some("nonexistent"),
            "key1",
            vec![String::from("; Nonexistent section")],
        );
        assert!(result.is_err());
        match result.unwrap_err() {
            ConfigError::SectionNotFound { section } => {
                assert_eq!(section, Some("nonexistent".to_string()));
            }
            _ => panic!("Expected SectionNotFound error"),
        }

        // Test correct case
        assert!(
            ini.set_section_doc(Some("test"), vec![String::from("; Test section")])
                .is_ok()
        );
        assert!(
            ini.set_property_doc(Some("test"), "key1", vec![String::from("; Test property")])
                .is_ok()
        );

        let result = ini.to_string();
        assert!(result.contains("; Test section"));
        assert!(result.contains("; Test property"));
    }

    #[test]
    fn test_strict_section_behavior() {
        let mut ini = Ini::new();

        // Test that setting a property in a nonexistent section should fail
        let result = ini.set_property(Some("nonexistent"), "key", Some("value"));
        assert!(result.is_err());
        match result.unwrap_err() {
            ConfigError::SectionNotFound { section } => {
                assert_eq!(section, Some("nonexistent".to_string()));
            }
            _ => panic!("Expected SectionNotFound error"),
        }

        // Test that setting a property in the global section (None) should now succeed, as the global section exists by default
        let result = ini.set_property(None, "global_key", Some("value"));
        assert!(
            result.is_ok(),
            "Setting global property should succeed because the global section exists by default"
        );

        // Correct workflow: create section first, then set property
        ini.set_section("test");

        assert!(
            ini.set_property(Some("test"), "key1", Some("value1"))
                .is_ok()
        );
        assert!(
            ini.set_property(None, "another_global_key", Some("another_global_value"))
                .is_ok()
        );

        // Verify values are set correctly
        assert_eq!(ini.get_string(Some("test"), "key1"), Some("value1"));
        assert_eq!(ini.get_string(None, "global_key"), Some("value")); // Previously set
        assert_eq!(
            ini.get_string(None, "another_global_key"),
            Some("another_global_value")
        );

        // Test idempotency of set_section (calling it multiple times for the same section should not fail)
        ini.set_section("test"); // Set the same section again
        assert!(
            ini.set_property(Some("test"), "key2", Some("value2"))
                .is_ok()
        );
    }

    #[test]
    fn test_debug_parsing() {
        let content = r#"; Global comment 1
global_key=global_value

; section1 comment
[section1]
; key1 comment
key1=value1
"#;

        println!("=== Debug parsing items ===");
        let items: Vec<Item> = ini_engine::Parser::new(content).collect();
        for (i, item) in items.iter().enumerate() {
            println!("Item {}: {:?}", i, item);
        }
    }

    #[test]
    fn test_readonly_ini_line_number_tracking() {
        let content = r#"; Global comment 1
global_key=global_value

; section1 comment
[section1]
; key1 comment
key1=value1

; key2 comment  
key2=value2

[section2]
key3=value3
"#;

        // First parse to ReadonlyIni to get line number information
        let items: Vec<Item> = ini_engine::Parser::new(content).collect();
        let readonly_ini: ReadonlyIni = items.try_into().expect("Parse failed");

        // Verify global section line number
        let global_section = readonly_ini.section(None).unwrap();
        println!("Global section line_num: {}", global_section.line_num());
        assert_eq!(global_section.line_num(), 0); // Default to 0

        // Verify global_key line number
        assert_eq!(
            readonly_ini
                .get_property(None, "global_key")
                .map(|v| v.line_num()),
            Some(2)
        );

        // Verify section1 line number
        assert_eq!(
            readonly_ini.section(Some("section1")).map(|s| s.line_num()),
            Some(5)
        );

        // Verify property line numbers
        assert_eq!(
            readonly_ini
                .get_property(Some("section1"), "key1")
                .map(|v| v.line_num()),
            Some(7)
        );
        assert_eq!(
            readonly_ini
                .get_property(Some("section1"), "key2")
                .map(|v| v.line_num()),
            Some(10)
        );

        // Verify section2 line number
        assert_eq!(
            readonly_ini.section(Some("section2")).map(|s| s.line_num()),
            Some(12)
        );
        assert_eq!(
            readonly_ini
                .get_property(Some("section2"), "key3")
                .map(|v| v.line_num()),
            Some(13)
        );

        // Verify document content is correctly preserved
        let global_property = readonly_ini
            .section(None)
            .unwrap()
            .data
            .inner
            .get("global_key")
            .unwrap();
        assert_eq!(
            global_property.doc_texts(),
            &["; Global comment 1".to_string()]
        );

        let section1 = readonly_ini.section(Some("section1")).unwrap();
        assert_eq!(
            section1.doc_texts(),
            &["", "; section1 comment"].map(|s| s.to_string())
        );

        let key1_property = section1.data.inner.get("key1").unwrap();
        assert_eq!(key1_property.doc_texts(), &["; key1 comment".to_string()]);

        // Test converting to editable structure
        let editable_ini: Ini = readonly_ini.into();
        assert_eq!(
            editable_ini.get_string(None, "global_key"),
            Some("global_value")
        );
        assert_eq!(
            editable_ini.get_string(Some("section1"), "key1"),
            Some("value1")
        );
    }

    #[test]
    fn test_editable_ini_functionality() {
        let content = r#"; Global comment 1
global_key=global_value

; section1 comment
[section1]
; key1 comment
key1=value1

; key2 comment  
key2=value2

[section2]
key3=value3
"#;

        let ini: Ini = content.parse().expect("Parse failed");

        // Test basic functionality of editable structure
        assert_eq!(ini.get_string(None, "global_key"), Some("global_value"));
        assert_eq!(ini.get_string(Some("section1"), "key1"), Some("value1"));
        assert_eq!(ini.get_string(Some("section1"), "key2"), Some("value2"));
        assert_eq!(ini.get_string(Some("section2"), "key3"), Some("value3"));

        // Verify document content is correctly preserved (but without line number information)
        let global_section = ini.section(None).unwrap();
        let global_property = global_section.data.get("global_key").unwrap();
        assert_eq!(
            global_property.doc_texts(),
            &["; Global comment 1".to_string()]
        );

        let section1 = ini.section(Some("section1")).unwrap();
        // Note: Due to blank line processing, this may contain empty strings
        assert!(
            section1
                .doc_texts()
                .contains(&"; section1 comment".to_string())
        );

        let key1_property = section1.data.get("key1").unwrap();
        assert_eq!(key1_property.doc_texts(), &["; key1 comment".to_string()]);

        let key2_property = section1.data.get("key2").unwrap();
        // Check that document content contains expected comments, ignoring blank lines and spaces
        assert!(
            key2_property
                .doc_texts()
                .iter()
                .any(|line| line.trim() == "; key2 comment")
        );
    }

    #[test]
    fn test_architecture_separation() {
        let content = r#"; Config header
global_setting=value

[database]
host=localhost
port=3306
"#;

        // 1. Parse to ReadonlyIni (preserve line number information)
        let items: Vec<Item> = ini_engine::Parser::new(content).collect();
        let readonly_ini: ReadonlyIni = items.try_into().expect("Parse failed");

        // Check line number information
        assert_eq!(
            readonly_ini
                .get_property(None, "global_setting")
                .map(|v| v.line_num()),
            Some(2)
        );
        assert_eq!(
            readonly_ini.section(Some("database")).map(|s| s.line_num()),
            Some(4)
        );
        assert_eq!(
            readonly_ini
                .get_property(Some("database"), "host")
                .map(|v| v.line_num()),
            Some(5)
        );
        assert_eq!(
            readonly_ini
                .get_property(Some("database"), "port")
                .map(|v| v.line_num()),
            Some(6)
        );

        // 2. 转换为可编辑的 Ini（丢弃行号，保留内容）
        let mut editable_ini: Ini = readonly_ini.into();

        // Verify content conversion is correct
        assert_eq!(
            editable_ini.get_string(None, "global_setting"),
            Some("value")
        );
        assert_eq!(
            editable_ini.get_string(Some("database"), "host"),
            Some("localhost")
        );
        assert_eq!(
            editable_ini.get_string(Some("database"), "port"),
            Some("3306")
        );

        // 3. 在可编辑结构上进行修改
        editable_ini
            .set_property(Some("database"), "host", Some("127.0.0.1"))
            .unwrap();
        editable_ini.set_section("cache");
        editable_ini
            .set_property(Some("cache"), "enabled", Some(true))
            .unwrap();

        // 4. Verify modified content
        assert_eq!(
            editable_ini.get_string(Some("database"), "host"),
            Some("127.0.0.1")
        );
        assert_eq!(
            editable_ini
                .get_value::<bool>(Some("cache"), "enabled")
                .unwrap(),
            Some(true)
        );

        println!(
            "Architecture separation test successful: parsing preserves line numbers, editing focuses on content"
        );
    }

    #[test]
    fn test_from_readonly_ini_trait() {
        let content = r#"; Config header
global_setting=value

[database]
host=localhost
port=3306
"#;

        // 1. 解析为 ReadonlyIni
        let items: Vec<Item> = ini_engine::Parser::new(content).collect();
        let readonly_ini: ReadonlyIni = items.try_into().expect("解析失败");

        // 2. 使用 From trait 转换为 Ini
        let editable_ini: Ini = readonly_ini.into();

        // 3. Verify conversion result
        assert_eq!(
            editable_ini.get_string(None, "global_setting"),
            Some("value")
        );
        assert_eq!(
            editable_ini.get_string(Some("database"), "host"),
            Some("localhost")
        );
        assert_eq!(
            editable_ini.get_string(Some("database"), "port"),
            Some("3306")
        );

        println!("From trait conversion test successful");
    }

    #[test]
    fn test_from_readonly_document_trait() {
        // 创建一个 ReadonlyDocument
        let property_value = PropertyValue {
            value: Some("test_value"),
        };
        let doc_texts = vec!["; Test comment"];
        let readonly_doc = ReadonlyDocument::new(property_value, 5, doc_texts.clone());

        // 使用 From trait 转换为 EditableDocument
        let editable_doc: EditableDocument<PropertyValue<String>> = readonly_doc.into();

        // Verify conversion result
        let expected_doc_texts: Vec<String> = doc_texts.iter().map(|s| s.to_string()).collect();
        assert_eq!(editable_doc.doc_texts(), &expected_doc_texts);
        assert_eq!(editable_doc.value, Some("test_value".to_string()));

        println!("ReadonlyDocument From trait conversion test successful");
    }

    #[test]
    fn test_all_from_traits() {
        // Test all From trait implementations

        // 1. PropertyValue -> EditableDocument<PropertyValue>
        let property_value = PropertyValue {
            value: Some("test_value".to_string()),
        };
        let editable_prop: EditableDocument<PropertyValue<String>> =
            EditableDocument::new(property_value, vec!["; Property comment".to_string()]);
        assert_eq!(editable_prop.value, Some("test_value".to_string()));

        // 2. Properties -> EditableDocument<Properties>
        let properties = Properties::new();
        let editable_section: EditableDocument<Properties> =
            EditableDocument::new(properties, vec!["; Section comment".to_string()]);
        assert!(editable_section.is_empty());

        // 3. ReadonlyDocument<PropertyValue> -> EditableDocument<PropertyValue>
        let readonly_prop = ReadonlyDocument::new(
            PropertyValue {
                value: Some("readonly_value"),
            },
            10,
            vec!["; Comment"],
        );
        let editable_prop2: EditableDocument<PropertyValue<String>> = readonly_prop.into();
        assert_eq!(editable_prop2.value, Some("readonly_value".to_string()));
        assert_eq!(editable_prop2.doc_texts(), &["; Comment".to_string()]);

        // 4. ReadonlyProperties -> Properties (Test through ReadonlyIni since ReadonlyProperties internals are private)

        // 5. ReadonlyIni -> Ini (Test through parsing)
        let content = r#"key=value"#;
        let items: Vec<Item> = ini_engine::Parser::new(content).collect();
        let readonly_ini: ReadonlyIni = items.try_into().expect("Parse failed");
        let editable_ini: Ini = readonly_ini.into();
        assert_eq!(editable_ini.get_string(None, "key"), Some("value"));

        println!("All From trait conversion tests successful");
    }
}
