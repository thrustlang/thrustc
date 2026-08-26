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

use thrustc_code_location::Span;

#[derive(Debug, Default, Clone)]
pub struct GenericScope {
    scopes: Vec<HashMap<String, Span>>,
}

impl GenericScope {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }
}

impl GenericScope {
    #[inline]
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::with_capacity(4));
    }

    #[inline]
    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }
}

impl GenericScope {
    #[inline]
    pub fn push_parameter(&mut self, name: String, span: Span) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, span);
        }
    }
}

impl GenericScope {
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.scopes.is_empty() || self.scopes.iter().all(|scope| scope.is_empty())
    }
}

impl GenericScope {
    #[inline]
    pub fn resolve(&self, name: &str) -> Option<Span> {
        for scope in self.scopes.iter().rev() {
            if let Some(span) = scope.get(name) {
                return Some(*span);
            }
        }

        None
    }
}
