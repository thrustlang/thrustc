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

use thrustc_code_location::Span;
use thrustc_preprocessor::module::Module;

#[derive(Debug)]
pub struct MacroScope<'parser> {
    module: &'parser Module,
    access: Vec<String>,
    argument_ranges: Vec<(u32, u32)>,
}

impl<'parser> MacroScope<'parser> {
    #[inline]
    pub fn new(
        module: &'parser Module,
        access: Vec<String>,
        argument_ranges: Vec<(u32, u32)>,
    ) -> Self {
        Self {
            module,
            access,
            argument_ranges,
        }
    }
}

impl<'parser> MacroScope<'parser> {
    #[inline]
    pub fn get_module(&self) -> &'parser Module {
        self.module
    }

    #[inline]
    pub fn get_access(&self) -> &[String] {
        &self.access
    }

    #[inline]
    pub fn get_argument_ranges(&self) -> &[(u32, u32)] {
        &self.argument_ranges
    }
}

impl MacroScope<'_> {
    #[inline]
    pub fn is_argument_token(&self, span: Span) -> bool {
        let start: u32 = span.get_span_start();
        let end: u32 = span.get_span_end();

        self.argument_ranges
            .iter()
            .any(|(range_start, range_end)| start < *range_end && end > *range_start)
    }
}
