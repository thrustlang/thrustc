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

use thrustc_analyzer::Analyzer;
use thrustc_ast::Ast;
use thrustc_ast_verifier::AstVerifier;
use thrustc_attribute_checker::AttributeChecker;
use thrustc_linter::Linter;
use thrustc_options::{CompilationUnit, CompilerOptions};
use thrustc_scoper::Scoper;
use thrustc_typechecker::TypeChecker;

#[derive(Debug)]
pub struct SemanticAnalysis<'semantic_analyzer> {
    type_checker: TypeChecker<'semantic_analyzer>,
    analyzer: Analyzer<'semantic_analyzer>,
    attr_checker: AttributeChecker<'semantic_analyzer>,
    scoper: Scoper<'semantic_analyzer>,
    verifier: AstVerifier<'semantic_analyzer>,
    linter: Linter<'semantic_analyzer>,

    options: &'semantic_analyzer CompilerOptions,
}

impl<'semantic_analyzer> SemanticAnalysis<'semantic_analyzer> {
    #[inline]
    pub fn new(
        ast: &'semantic_analyzer [Ast<'semantic_analyzer>],
        file: &'semantic_analyzer CompilationUnit,
        options: &'semantic_analyzer CompilerOptions,
    ) -> Self {
        let type_checker: TypeChecker<'_> = TypeChecker::new(ast, file, options);
        let analyzer: Analyzer<'_> = Analyzer::new(ast, file, options);
        let attr_checker: AttributeChecker<'_> = AttributeChecker::new(ast, file, options);
        let scoper: Scoper<'_> = Scoper::new(ast, file, options);
        let verifier: AstVerifier<'_> = AstVerifier::new(ast, file, options);
        let linter: Linter<'_> = Linter::new(ast, file, options);

        Self {
            type_checker,
            analyzer,
            attr_checker,
            scoper,
            verifier,
            linter,

            options,
        }
    }
}

impl<'semantic_analyzer> SemanticAnalysis<'semantic_analyzer> {
    pub fn analyze(&mut self, parser_throwed_errors: bool) -> bool {
        if parser_throwed_errors {
            return true;
        }

        let scoper_threw_errors: bool = self.scoper.start();

        if scoper_threw_errors {
            return true;
        }

        let verifier_threw_errors: bool = self.verifier.analyze_top();

        if verifier_threw_errors {
            return true;
        }

        let type_checker_threw_errors: bool = self.type_checker.start();
        let analyzer_threw_errors: bool = self.analyzer.start();
        let attr_checker_threw_errors: bool = self.attr_checker.start();

        if !type_checker_threw_errors
            && !analyzer_threw_errors
            && !attr_checker_threw_errors
            && !scoper_threw_errors
            && !self.options.disable_all_warnings()
        {
            self.linter.check();
        }

        type_checker_threw_errors
            || analyzer_threw_errors
            || attr_checker_threw_errors
            || scoper_threw_errors
    }
}
