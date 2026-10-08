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

use std::path::PathBuf;

use thrustc_code_location::Span;
use thrustc_errors::{CompilationIssue, CompilationIssueCode};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MacroLimit {
    TokenPasting,
    VariadicArguments,
    Stringizing,
    CallBodied,
    SizeOfExpression,
    SizeOfMalformed,
    DeclaratorMalformed,
    DeclaratorUnsupported,
    ForHeaderUnsupported,
    UnsupportedStatementKeyword,
    ExpectedToken,
    UnexpectedEndOfTokens,
    TrailingTokens,
    ExpressionMalformed,
    StatementMalformed,
    ExpansionDepthExceeded,
    RecursiveExpansion,
    RescanLimitExceeded,
    InvocationMalformed,
    ArgumentCountMismatch,
    TokenBalanceError,
}

pub fn get_macro_issue_help(name: &str, limit: MacroLimit) -> (String, String) {
    let detail: String = match limit {
        MacroLimit::TokenPasting => format!(
            "The C macro '{name}' builds new names by gluing words together, which is not supported."
        ),
        MacroLimit::VariadicArguments => format!(
            "The C macro '{name}' takes a variable number of arguments, which is not supported."
        ),
        MacroLimit::Stringizing => {
            format!("The C macro '{name}' turns code into text, which is not supported.")
        }
        MacroLimit::CallBodied => format!(
            "The C macro '{name}' runs code to get its value, so it cannot become a constant."
        ),
        MacroLimit::SizeOfExpression => format!(
            "The C macro '{name}' uses sizeof on an expression, which is not supported yet."
        ),
        MacroLimit::SizeOfMalformed => format!(
            "The C macro '{name}' has a malformed sizeof(...) form that could not be parsed safely."
        ),
        MacroLimit::DeclaratorMalformed => format!(
            "The C macro '{name}' has a malformed declarator that could not be parsed safely."
        ),
        MacroLimit::DeclaratorUnsupported => {
            format!("The C macro '{name}' uses a declarator shape that is not supported yet.")
        }
        MacroLimit::ForHeaderUnsupported => {
            format!("The C macro '{name}' uses a for-loop header form that is not supported yet.")
        }
        MacroLimit::UnsupportedStatementKeyword => format!(
            "The C macro '{name}' uses a statement keyword that is not supported in macro statement lowering."
        ),
        MacroLimit::ExpectedToken => {
            format!("The C macro '{name}' is missing an expected token while parsing.")
        }
        MacroLimit::UnexpectedEndOfTokens => {
            format!("The C macro '{name}' ended unexpectedly while parsing.")
        }
        MacroLimit::TrailingTokens => format!(
            "The C macro '{name}' leaves trailing tokens after parsing, so it is ambiguous."
        ),
        MacroLimit::ExpressionMalformed => {
            format!("The C macro '{name}' has an expression form that could not be parsed safely.")
        }
        MacroLimit::StatementMalformed => {
            format!("The C macro '{name}' has a statement form that could not be parsed safely.")
        }
        MacroLimit::ExpansionDepthExceeded => {
            format!("The C macro '{name}' exceeded maximum expansion depth.")
        }
        MacroLimit::RecursiveExpansion => {
            format!("The C macro '{name}' recursively expands itself and cannot be lowered safely.")
        }
        MacroLimit::RescanLimitExceeded => {
            format!("The C macro '{name}' requires too many expansion rescan passes.")
        }
        MacroLimit::InvocationMalformed => {
            format!("The C macro '{name}' has a malformed invocation form.")
        }
        MacroLimit::ArgumentCountMismatch => format!(
            "The C macro '{name}' invocation has a different argument count than its definition."
        ),
        MacroLimit::TokenBalanceError => format!(
            "The C macro '{name}' has unbalanced tokens in an expression or statement body."
        ),
    };

    let help: String = match limit {
        MacroLimit::TokenPasting => {
            "Rewrite it in the C code without gluing names together.".to_string()
        }
        MacroLimit::VariadicArguments => {
            "Give it a fixed number of arguments in the C code.".to_string()
        }
        MacroLimit::Stringizing => "Write the text directly in the C code instead.".to_string(),
        MacroLimit::CallBodied => {
            "Call the underlying function directly in the C code.".to_string()
        }
        MacroLimit::SizeOfExpression => {
            "Use sizeof(type) in the macro or rewrite this part without sizeof(expr).".to_string()
        }
        MacroLimit::SizeOfMalformed => {
            "Rewrite sizeof(...) to a simple supported form like sizeof(type).".to_string()
        }
        MacroLimit::DeclaratorMalformed => {
            "Fix the macro declaration syntax so each declarator is unambiguous.".to_string()
        }
        MacroLimit::DeclaratorUnsupported => {
            "Rewrite the declarator to a simpler pointer/array form supported by macro lowering."
                .to_string()
        }
        MacroLimit::ForHeaderUnsupported => {
            "Rewrite the for header to a simpler init/cond/inc form.".to_string()
        }
        MacroLimit::UnsupportedStatementKeyword => {
            "Rewrite the macro statement body to if/while/do/for/block/expr/decl forms supported by macro lowering.".to_string()
        }
        MacroLimit::ExpectedToken => {
            "Fix delimiters and required tokens in the macro body (for example missing ')', ']', or '}').".to_string()
        }
        MacroLimit::UnexpectedEndOfTokens => {
            "Ensure the macro body is complete and not cut off before closing delimiters or operands.".to_string()
        }
        MacroLimit::TrailingTokens => {
            "Remove extra trailing tokens so the macro parses as a single unambiguous expression/statement.".to_string()
        }
        MacroLimit::ExpressionMalformed => {
            "Rewrite the macro expression to a simpler supported form.".to_string()
        }
        MacroLimit::StatementMalformed => {
            "Rewrite the macro statement to a simpler supported form.".to_string()
        }
        MacroLimit::ExpansionDepthExceeded => {
            "Reduce nested macro indirections or recursive expansion depth.".to_string()
        }
        MacroLimit::RecursiveExpansion => {
            "Break the recursive macro chain or replace it with non-recursive helpers.".to_string()
        }
        MacroLimit::RescanLimitExceeded => {
            "Simplify the macro body so expansion stabilizes in fewer passes.".to_string()
        }
        MacroLimit::InvocationMalformed => {
            "Fix macro invocation syntax and separators so arguments parse unambiguously.".to_string()
        }
        MacroLimit::ArgumentCountMismatch => {
            "Adjust the invocation argument count to match the macro parameter list.".to_string()
        }
        MacroLimit::TokenBalanceError => {
            "Ensure parentheses/brackets/braces are balanced in the macro body.".to_string()
        }
    };

    (detail, help)
}

pub fn add_macro_error(
    ctx: &mut crate::macros::MacroContext<'_>,
    entity: &clang::Entity<'_>,
    macro_name: &str,
    detail: &str,
    help: &str,
    span: Span,
) {
    let prefix: String = crate::macros::expansion_prefix(entity);
    let origin: String = crate::macros::origin_note(entity);

    ctx.get_mut_transpiler_context()
        .add_macros_error(CompilationIssue::Error(
            CompilationIssueCode::E0110,
            format!("{prefix}Macro '{macro_name}' translation failed: {detail}{origin}"),
            help.to_string(),
            None,
            span,
        ));
}

pub fn add_macro_parameter_type_error(
    ctx: &mut crate::macros::MacroContext<'_>,
    entity: &clang::Entity<'_>,
    macro_name: &str,
    parameter: &str,
    span: Span,
) {
    let prefix: String = crate::macros::expansion_prefix(entity);
    let origin: String = crate::macros::origin_note(entity);

    ctx.get_mut_transpiler_context()
        .add_macros_error(CompilationIssue::Error(
            CompilationIssueCode::E0112,
            format!(
                "{prefix}Macro '{macro_name}' translation failed: could not infer the type of parameter '{parameter}'.{origin}"
            ),
            "Give the parameter a typed context (for example a typed local declaration or a cast) so its type can be determined.".to_string(),
            None,
            span,
        ));
}

pub fn collapse_macro_site_errors(
    ctx: &mut crate::macros::MacroContext<'_>,
    site: &clang::Entity<'_>,
    macro_name: &str,
    base_error_count: usize,
) {
    let mut fresh: Vec<CompilationIssue> = ctx
        .get_mut_transpiler_context()
        .take_new_errors_since(base_error_count);

    if fresh.is_empty() {
        return;
    }

    let site_key: String =
        crate::macro_table::MacroTable::make_location_key(site).unwrap_or_default();

    let definition_site: String = ctx
        .get_macro_table()
        .get_macro_definition_site(macro_name)
        .unwrap_or_default();

    let mut notes: Vec<String> = Vec::new();

    if !definition_site.is_empty() {
        notes.push(format!("defined at {definition_site}"));
    }

    if !site_key.is_empty() {
        notes.push(format!("expanded at {site_key}"));
    }

    let note: Option<String> = if notes.is_empty() {
        None
    } else {
        Some(notes.join("\n"))
    };

    let first: CompilationIssue = fresh.remove(0);

    match first {
        CompilationIssue::Error(code, message, help, ..) => {
            ctx.get_mut_transpiler_context()
                .add_macros_error(CompilationIssue::Error(
                    code,
                    message,
                    help,
                    note,
                    Span::nothing(),
                ));
        }

        other => {
            ctx.get_mut_transpiler_context().add_macros_error(other);
        }
    }
}

pub fn collapse_macro_site_errors_with_failed_at(
    ctx: &mut crate::macros::MacroContext<'_>,
    site: &clang::Entity<'_>,
    macro_name: &str,
    failed_child: &clang::Entity<'_>,
    base_error_count: usize,
) {
    let mut fresh: Vec<CompilationIssue> = ctx
        .get_mut_transpiler_context()
        .take_new_errors_since(base_error_count);

    if fresh.is_empty() {
        return;
    }

    let site_key: String =
        crate::macro_table::MacroTable::make_location_key(site).unwrap_or_default();

    let definition_site: String = ctx
        .get_macro_table()
        .get_macro_definition_site(macro_name)
        .unwrap_or_default();

    let mut notes: Vec<String> = Vec::new();

    if !definition_site.is_empty() {
        notes.push(format!("defined at {definition_site}"));
    }

    if !site_key.is_empty() {
        notes.push(format!("expanded at {site_key}"));
    }

    let failed_at: String = failed_child
        .get_range()
        .map(|range| {
            let spelling = range.get_start().get_spelling_location();

            match spelling.file {
                Some(file) => format!(
                    "{}:{}:{}",
                    file.get_path().display(),
                    spelling.line,
                    spelling.column
                ),
                None => String::new(),
            }
        })
        .unwrap_or_default();

    if !failed_at.is_empty() {
        notes.push(format!("failed at {failed_at}"));

        if let Some(spelling_file) = failed_child
            .get_range()
            .and_then(|range| range.get_start().get_spelling_location().file)
        {
            let spelling_offset: u32 = failed_child
                .get_range()
                .map(|range| range.get_start().get_spelling_location().offset)
                .unwrap_or(0);

            let mut chain: Vec<String> = Vec::new();

            let mut current_file: PathBuf = spelling_file.get_path();

            let mut current_offset: u32 = spelling_offset;

            for _ in 0..8 {
                let Some(inner) = ctx
                    .get_macro_table()
                    .find_macro_at(&current_file, current_offset)
                else {
                    break;
                };

                if inner == macro_name {
                    continue;
                }

                let Some((body_file, _, _)) = ctx.get_macro_table().get_macro_body_range(&inner)
                else {
                    break;
                };

                chain.push(format!(
                    "through macro '{inner}' at {}",
                    body_file.display()
                ));

                current_file = body_file;
                current_offset = 0;
            }

            notes.extend(chain);
        }
    }

    let note: Option<String> = if notes.is_empty() {
        None
    } else {
        Some(notes.join("\n"))
    };

    let first: CompilationIssue = fresh.remove(0);

    match first {
        CompilationIssue::Error(code, message, help, ..) => {
            ctx.get_mut_transpiler_context()
                .add_macros_error(CompilationIssue::Error(
                    code,
                    message,
                    help,
                    note,
                    Span::nothing(),
                ));
        }

        other => {
            ctx.get_mut_transpiler_context().add_macros_error(other);
        }
    }
}
