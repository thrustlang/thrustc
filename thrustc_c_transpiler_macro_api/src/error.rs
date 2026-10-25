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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MacroExpansionError {
    UnknownMacro(String),
    ArgumentCountMismatch(String),
    ExpansionFailed(String),
    NeedsStatements(String),
    Malformed(String),
}

impl MacroExpansionError {
    #[inline]
    pub fn get_message(&self) -> String {
        match self {
            MacroExpansionError::UnknownMacro(name) => {
                format!("The C macro '{name}' was not found.")
            }
            MacroExpansionError::ArgumentCountMismatch(name) => {
                format!("The C macro '{name}' was invoked with the wrong number of arguments.")
            }
            MacroExpansionError::ExpansionFailed(detail) => detail.clone(),
            MacroExpansionError::NeedsStatements(name) => format!(
                "The C macro '{name}' needs statement expansion, which is not supported yet."
            ),
            MacroExpansionError::Malformed(detail) => detail.clone(),
        }
    }
}
