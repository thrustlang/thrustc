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

use thrustc_ast::Ast;
use thrustc_parser_table::SymbolTable;
use thrustc_preprocessor::module::Module;

#[derive(Debug)]
pub struct ImportContext<'parser, 'ctx> {
    modules: &'parser [Module],
    symbols: &'ctx mut SymbolTable<'parser>,
    ast: &'ctx mut Vec<Ast<'parser>>,
}

impl<'parser, 'ctx> ImportContext<'parser, 'ctx> {
    pub fn new(
        modules: &'parser [Module],
        symbols: &'ctx mut SymbolTable<'parser>,
        ast: &'ctx mut Vec<Ast<'parser>>,
    ) -> Self {
        Self {
            modules,
            symbols,
            ast,
        }
    }
}

impl<'parser, 'ctx> ImportContext<'parser, 'ctx> {
    #[inline(always)]
    pub fn get_modules(&self) -> &'parser [Module] {
        self.modules
    }

    #[inline(always)]
    pub fn get_symbols(&self) -> &SymbolTable<'parser> {
        self.symbols
    }

    #[inline(always)]
    pub fn get_mut_symbols(&mut self) -> &mut SymbolTable<'parser> {
        self.symbols
    }

    #[inline(always)]
    pub fn get_ast(&self) -> &[Ast<'parser>] {
        self.ast
    }

    #[inline(always)]
    pub fn get_mut_ast(&mut self) -> &mut Vec<Ast<'parser>> {
        self.ast
    }
}

impl<'parser, 'ctx> ImportContext<'parser, 'ctx> {
    #[inline(always)]
    pub fn add_ast_node(&mut self, ast: Ast<'parser>) {
        self.ast.push(ast);
    }
}
