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

use thrustc_diagnostician::Diagnostician;
use thrustc_errors::{CompilationIssue, CompilationPosition};
use thrustc_logging::LoggingType;
use thrustc_span::Span;

pub fn abort_compilation(
    diagnostician: &mut Diagnostician,
    position: CompilationPosition,
    message: &str,
    span: Span,
    file: std::path::PathBuf,
    line: u32,
) -> ! {
    diagnostician.dispatch_diagnostic(
        &CompilationIssue::FrontendBug(
            "Failed to Compile".into(),
            message.into(),
            span,
            position,
            file,
            line,
        ),
        LoggingType::FrontendBug,
    );

    std::process::exit(1);
}
