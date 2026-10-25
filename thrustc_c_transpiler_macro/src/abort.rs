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

use crate::macro_context::MacroContext;

use thrustc_code_location::Span;

impl MacroContext<'_> {
    pub fn abort_expansion(
        &self,
        message: &str,
        span: Span,
        file: std::path::PathBuf,
        line: u32,
    ) -> ! {
        eprintln!("MACRO EXPANSION ABORT - {message}");

        eprintln!("  --> {span:?} at {}:{line}", file.display());

        let expansion_stack: &[String] = self.get_expansion_stack();

        if !expansion_stack.is_empty() {
            eprintln!("  expansion: {}", expansion_stack.join(" -> "));
        }

        std::process::exit(thrustc_constants::FAILURE_CODE);
    }
}
