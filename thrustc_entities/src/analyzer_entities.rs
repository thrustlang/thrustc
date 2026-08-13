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

use ahash::AHashMap as HashMap;

use thrustc_code_location::Span;
use thrustc_typesystem::Type;

pub type AnalyzerLocal<'symbol> = &'symbol Type;
pub type AnalyzerLocals<'symbol> = Vec<HashMap<&'symbol str, AnalyzerLocal<'symbol>>>;

pub type AnalyzerLLI<'symbol> = (&'symbol Type, Span);
pub type AnalyzerLLIs<'symbol> = Vec<HashMap<&'symbol str, AnalyzerLLI<'symbol>>>;

pub type AnalyzerAssemblerFunction<'symbol> = (&'symbol [Type], bool);
pub type AnalyzerAssemblerFunctions<'symbol> =
    HashMap<&'symbol str, AnalyzerAssemblerFunction<'symbol>>;

pub type AnalyzerFunction<'symbol> = (&'symbol [Type], bool);
pub type AnalyzerFunctions<'symbol> = HashMap<&'symbol str, AnalyzerFunction<'symbol>>;
