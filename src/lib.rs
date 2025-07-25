use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
    str::FromStr,
};

extern crate ini_roundtrip as ini_engine;
#[cfg(feature = "ordered")]
use indexmap::IndexMap as Map;
#[cfg(not(feature = "ordered"))]
use std::collections::HashMap as Map;

use thiserror::Error;

use ini_engine::Item;

/// Wraps section and property values
/// Extends to record their preceding documentation content
/// Such as: blank lines, comment lines, etc.
#[derive(Clone)]
pub struct Document<T> {
    // Each line of documentation content before the data line
    doc_texts: Vec<String>,
    data: T,
}

impl<T> From<T> for Document<T> {
    fn from(data: T) -> Self {
        Self {
            doc_texts: Vec::new(),
            data,
        }
    }
}

impl<T> Document<T> {
    pub fn new(data: T, doc_texts: Vec<String>) -> Self {
        Self { doc_texts, data }
    }

    pub fn doc_texts(&self) -> &[String] {
        &self.doc_texts
    }
}

impl<T: Display> Display for Document<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for doc_line in &self.doc_texts {
            writeln!(f, "{}", doc_line)?;
        }
        write!(f, "{}", self.data)
    }
}

impl<T> DerefMut for Document<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl<T> Deref for Document<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

pub type SectionKey = Option<String>;
pub type PropertyKey = String;

#[derive(Clone)]
pub struct PropertyValue {
    pub value: Option<String>,
}

impl Display for PropertyValue {
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

impl Properties {
    pub fn new() -> Self {
        Self { inner: Map::new() }
    }

    pub fn insert(
        &mut self,
        key: PropertyKey,
        value: PropertyDocument,
    ) -> Option<PropertyDocument> {
        self.inner.insert(key, value)
    }

    pub fn get(&self, key: &str) -> Option<&PropertyDocument> {
        self.inner.get(key)
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

    pub fn set(&mut self, key: PropertyKey, value: PropertyValue) -> Option<PropertyDocument> {
        let property_doc = PropertyDocument::from(value);
        self.insert(key, property_doc)
    }

    pub fn remove(&mut self, key: &str) -> Option<PropertyDocument> {
        #[cfg(feature = "ordered")]
        {
            self.inner.shift_remove(key)
        }
        #[cfg(not(feature = "ordered"))]
        {
            self.inner.remove(key)
        }
    }

    #[cfg(feature = "ordered")]
    pub fn remove_at(&mut self, idx: usize) -> Option<(String, PropertyDocument)> {
        self.inner.shift_remove_index(idx)
    }

    #[cfg(feature = "ordered")]
    pub fn replace_at(
        &mut self,
        idx: usize,
        key: PropertyKey,
        value: PropertyDocument,
    ) -> Option<(String, PropertyDocument)> {
        let entry = self.inner.get_index_entry(idx);
        if let Some(mut entry) = entry {
            use indexmap::map::MutableEntryKey;
            let old_key = std::mem::replace(entry.key_mut(), key);
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

impl Display for Properties {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (property_key, property_doc) in &self.inner {
            // Print property's documentation content
            for doc_line in &property_doc.doc_texts {
                writeln!(f, "{}", doc_line)?;
            }

            // Print property
            if let Some(value) = &property_doc.value {
                writeln!(f, "{}={}", property_key, value)?;
            } else {
                writeln!(f, "{}", property_key)?;
            }
        }
        Ok(())
    }
}

pub type PropertyDocument = Document<PropertyValue>;

pub type SectionDocument = Document<Properties>;

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

pub struct Ini {
    sections: Map<SectionKey, SectionDocument>,
}

impl Ini {
    /// Create a new INI instance, preset a general section (None)
    pub fn new() -> Self {
        let mut sections = Map::new();
        // Preset a general section
        sections.insert(None, Properties::new().into());
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
            .or_insert_with(|| Properties::new().into());
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
        let section_doc =
            self.sections
                .get_mut(&section_key)
                .ok_or_else(|| ConfigError::SectionNotFound {
                    section: section_name.map(|s| s.to_string()),
                })?;

        // Set property
        let property_value = PropertyValue {
            value: value.map(|v| v.to_string()),
        };
        section_doc.insert(key.to_string(), property_value.into());

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

        // Get section
        let section_doc =
            self.sections
                .get_mut(&section_key)
                .ok_or_else(|| ConfigError::SectionNotFound {
                    section: section_name.map(|s| s.to_string()),
                })?;

        // Get property
        let property_doc =
            section_doc
                .data
                .inner
                .get_mut(key)
                .ok_or_else(|| ConfigError::PropertyNotFound {
                    section: section_name.map(|s| s.to_string()),
                    property: key.to_string(),
                })?;

        // Update property documentation
        property_doc.doc_texts = doc_texts;
        Ok(())
    }

    /// Get the value of the specified section and key, parse to the specified type via FromStr
    pub fn get_value<T: FromStr>(
        &self,
        section_name: Option<&str>,
        key: &str,
    ) -> Result<Option<T>, T::Err> {
        let section_key = section_name.map(|s| s.to_string());

        if let Some(section_doc) = self.sections.get(&section_key) {
            return section_doc.get_value(key);
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
            Some(section_doc) => {
                #[cfg(feature = "ordered")]
                {
                    let map = &mut section_doc.data.inner;
                    map.shift_remove(key).is_some()
                }
                #[cfg(not(feature = "ordered"))]
                {
                    let map = &mut section_doc.data.inner;
                    map.remove(key).is_some()
                }
            }
            None => false,
        }
    }

    /// Remove the specified section
    pub fn remove_section(&mut self, section_name: Option<&str>) -> bool {
        let section_key = section_name.map(|s| s.to_string());
        #[cfg(feature = "ordered")]
        {
            self.sections.shift_remove(&section_key).is_some()
        }
        #[cfg(not(feature = "ordered"))]
        {
            self.sections.remove(&section_key).is_some()
        }
    }

    /// Get all sections
    pub fn sections(&self) -> &Map<SectionKey, SectionDocument> {
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
                continue; // Global section already handled
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
        let mut ini = Ini::new();
        let mut current_section: Option<String> = None;
        let mut pending_docs: Vec<String> = Vec::new();

        for item in value {
            match item {
                Item::Blank { raw } => {
                    pending_docs.push(raw.to_string());
                }
                Item::Comment { raw, .. } => {
                    pending_docs.push(raw.to_string());
                }
                Item::Section { name, raw: _ } => {
                    if let Some(ref sec) = current_section {
                        if !pending_docs.is_empty() {
                            let docs: Vec<String> = pending_docs.drain(..).collect();
                            ini.set_section_doc(Some(sec), docs).ok();
                        }
                    } else if !pending_docs.is_empty() {
                        // Documentation for the global section
                        let docs: Vec<String> = pending_docs.drain(..).collect();
                        ini.set_section_doc(None, docs).ok();
                    }
                    ini.set_section(&name);
                    current_section = Some(name.to_string());
                }
                Item::Property { key, val, raw: _ } => {
                    let section = current_section.as_deref();
                    ini.set_property(section, &key, val.map(|v| v.to_string()))?;
                    // Set property documentation
                    if !pending_docs.is_empty() {
                        let docs: Vec<String> = pending_docs.drain(..).collect();
                        ini.set_property_doc(section, &key, docs).ok();
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

        Ok(ini)
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

        let ini: Ini = content.parse().expect("解析失败");
        let result = ini.to_string();
        println!("解析结果:\n{}", result);
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

        let ini: Ini = content.parse().expect("解析失败");
        let result = ini.to_string();

        // Verify that parsing the output again does not fail
        let _ini2: Ini = result.parse().expect("second parse failed");

        println!("Original content:\n{}", content);
        println!("Parsed result:\n{}", result);

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
            value: Some("test_value".to_string()),
        };

        let doc_texts = vec![
            "; This is a comment".to_string(),
            String::new(),
            "; Another comment".to_string(),
        ];

        let property_doc = Document::new(property_value, doc_texts);
        let result = property_doc.to_string();

        println!("PropertyDocument Display result:\n{}", result);
        assert!(result.contains("; This is a comment"));
        assert!(result.contains("; Another comment"));
        assert!(result.contains("test_value"));

        // Test the Display of Properties
        let mut properties = Properties::new();
        properties.insert("key1".to_string(), property_doc);

        let properties_result = properties.to_string();
        println!("Properties Display 结果:\n{}", properties_result);
        assert!(properties_result.contains("key1=test_value"));

        // Test the Display of SectionDocument
        let section_docs = vec!["; Section comment".to_string()];
        let section_doc = Document::new(properties, section_docs);

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

        let ini: Ini = content.parse().expect("解析失败");
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
        assert_eq!(ini.get_string(Some("flags"), "debug"), None); // 只有键名，没有值

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

        println!("编辑后的 INI:\n{}", ini);

        // Verify documentation settings
        let result = ini.to_string();
        assert!(result.contains("; Database configuration"));
        assert!(result.contains("; Important configuration"));
        assert!(result.contains("; Database host address"));
    }

    #[test]
    fn test_ini_deletion() {
        let mut ini = Ini::new();

        // 添加一些数据
        ini.set_section("section1");
        ini.set_section("section2");

        ini.set_property(Some("section1"), "key1", Some("value1"))
            .unwrap();
        ini.set_property(Some("section1"), "key2", Some("value2"))
            .unwrap();
        ini.set_property(Some("section2"), "key3", Some("value3"))
            .unwrap();

        // 测试删除属性
        assert!(ini.remove_property(Some("section1"), "key1"));
        assert!(!ini.has_property(Some("section1"), "key1"));
        assert!(ini.has_property(Some("section1"), "key2"));

        // 测试删除不存在的属性
        assert!(!ini.remove_property(Some("section1"), "nonexistent"));

        // 测试删除 section
        assert!(ini.remove_section(Some("section2")));
        assert!(!ini.has_property(Some("section2"), "key3"));

        // 测试删除不存在的 section
        assert!(!ini.remove_section(Some("nonexistent")));
    }

    #[test]
    fn test_type_conversion() {
        let mut ini = Ini::new();

        // 设置不同类型的值
        ini.set_section("config");

        ini.set_property(Some("config"), "port", Some(8080))
            .unwrap();
        ini.set_property(Some("config"), "timeout", Some(30.5))
            .unwrap();
        ini.set_property(Some("config"), "enabled", Some(true))
            .unwrap();
        ini.set_property(Some("config"), "name", Some("test_server"))
            .unwrap();

        // 测试类型转换
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

        // 测试类型转换失败
        assert!(ini.get_value::<i32>(Some("config"), "name").is_err());

        // 测试不存在的值
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
}
