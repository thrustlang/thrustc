/*

    Copyright (C) 2026  Stevens Benavides

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

*/

use std::collections::HashMap;

use serde_json::Value;

#[derive(Debug)]
pub struct Document {
    uri: String,
    text: String,
    version: i64,
}

#[derive(Debug)]
pub struct Documents {
    documents: HashMap<String, Document>,
}

impl Documents {
    #[inline]
    pub fn new() -> Self {
        Self {
            documents: HashMap::with_capacity(u8::MAX as usize),
        }
    }
}

impl Documents {
    pub fn open(&mut self, payload: &Value) -> Option<String> {
        let params: &Value = payload.get("params")?;
        let document: &Value = params.get("textDocument")?;
        let uri: &str = document.get("uri")?.as_str()?;
        let text: &str = document.get("text")?.as_str()?;
        let version: i64 = document.get("version").and_then(Value::as_i64).unwrap_or(0);

        let uri: String = uri.to_string();
        let document: Document = Document {
            uri: uri.clone(),
            text: text.to_string(),
            version,
        };

        self.documents.insert(uri.clone(), document);

        Some(uri)
    }

    pub fn change(&mut self, payload: &Value) -> Option<String> {
        let params: &Value = payload.get("params")?;
        let document: &Value = params.get("textDocument")?;
        let uri: &str = document.get("uri")?.as_str()?;
        let version: i64 = document.get("version").and_then(Value::as_i64).unwrap_or(0);

        let changes: &Value = params.get("contentChanges")?;
        let changes: &[Value] = changes.as_array()?;
        let change: &Value = changes.first()?;
        let text: &str = change.get("text")?.as_str()?;

        let uri: String = uri.to_string();

        let document: Document = Document {
            uri: uri.clone(),
            text: text.to_string(),
            version,
        };

        self.documents.insert(uri.clone(), document);

        Some(uri)
    }

    pub fn close(&mut self, payload: &Value) -> Option<String> {
        let params: &Value = payload.get("params")?;
        let document: &Value = params.get("textDocument")?;
        let uri: &str = document.get("uri")?.as_str()?;

        let uri: String = uri.to_string();

        self.documents.remove(&uri);

        Some(uri)
    }

    #[inline]
    pub fn get(&self, uri: &str) -> Option<&Document> {
        self.documents.get(uri)
    }
}

impl Document {
    #[inline]
    pub fn get_uri(&self) -> &str {
        &self.uri
    }

    #[inline]
    pub fn get_text(&self) -> &str {
        &self.text
    }

    #[inline]
    pub fn get_version(&self) -> i64 {
        self.version
    }
}
