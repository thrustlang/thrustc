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
use thrustc_ast_verifier::AstVerifier;
use thrustc_attribute_checker::AttributeChecker;
use thrustc_general_analyzer::GeneralAnalyzer;
use thrustc_linter::Linter;
use thrustc_options::{CompilationPhase, CompilationUnit, CompilerOptions};
use thrustc_scoper::Scoper;
use thrustc_typechecker::TypeChecker;

#[derive(Debug)]
pub struct SemanticAnalysis<'semantic_analyzer> {
    type_checker: TypeChecker<'semantic_analyzer>,
    general_analyzer: GeneralAnalyzer<'semantic_analyzer>,
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
        let general_analyzer: GeneralAnalyzer<'_> = GeneralAnalyzer::new(ast, file, options);
        let attr_checker: AttributeChecker<'_> = AttributeChecker::new(ast, file, options);
        let scoper: Scoper<'_> = Scoper::new(ast, file, options);
        let verifier: AstVerifier<'_> = AstVerifier::new(ast, file, options);
        let linter: Linter<'_> = Linter::new(ast, file, options);

        Self {
            type_checker,
            general_analyzer,
            attr_checker,
            scoper,
            verifier,
            linter,

            options,
        }
    }
}

impl<'semantic_analyzer> SemanticAnalysis<'semantic_analyzer> {
    pub fn execute(&mut self, parser_throwed_errors: bool) -> either::Either<bool, ()> {
        if parser_throwed_errors {
            return either::Either::Left(true);
        }

        let scoper_failed: bool = self.scoper.start();

        if scoper_failed {
            return either::Either::Left(true);
        }

        if self.options.stop_compilation_at(CompilationPhase::Scoper) {
            return either::Either::Right(());
        }

        let verifier_failed: bool = self.verifier.analyze();

        if verifier_failed {
            return either::Either::Left(true);
        }

        if self
            .options
            .stop_compilation_at(CompilationPhase::AstVerifier)
        {
            return either::Either::Right(());
        }

        let type_checker_fail: bool = self.type_checker.start();

        if self
            .options
            .stop_compilation_at(CompilationPhase::TypeChecker)
        {
            return either::Either::Right(());
        }

        let general_analyzer_failed: bool = self.general_analyzer.start();

        if self
            .options
            .stop_compilation_at(CompilationPhase::GeneralAnalyzer)
        {
            return either::Either::Right(());
        }

        let attribute_checker_failed: bool = self.attr_checker.start();

        if self
            .options
            .stop_compilation_at(CompilationPhase::AttributeChecker)
        {
            return either::Either::Right(());
        }

        if !type_checker_fail
            && !general_analyzer_failed
            && !attribute_checker_failed
            && !scoper_failed
            && !self.options.disable_all_warnings()
        {
            self.linter.start();

            if self.options.stop_compilation_at(CompilationPhase::Linter) {
                return either::Either::Right(());
            }
        }

        let fail: bool = type_checker_fail
            || general_analyzer_failed
            || attribute_checker_failed
            || scoper_failed;

        either::Either::Left(fail)
    }
}
