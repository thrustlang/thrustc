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

use thrustc_ast::{
    Ast,
    traits::{AstCodeLocation, AstStandardExtensions},
};
use thrustc_errors::{CompilationIssue, CompilationIssueCode};

use crate::Scoper;

pub fn check_for_multiple_terminators(scoper: &mut Scoper, node: &Ast) {
    let Ast::Block { nodes, .. } = node else {
        return;
    };

    if nodes.is_empty() {
        return;
    }

    let unreacheable_positions: Vec<(usize, &Ast)> = nodes
        .iter()
        .enumerate()
        .filter(|(_, stmt)| stmt.is_unreacheable_keyword())
        .collect();

    if unreacheable_positions.len() > 1 {
        for (_, node) in &unreacheable_positions[1..] {
            scoper.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0015,
                "Multiple unreachable instructions; only one is allowed.".into(),
                "Remove all; keep one.".into(),
                None,
                node.get_span(),
            ));
        }
    }

    let return_positions: Vec<(usize, &Ast)> = nodes
        .iter()
        .enumerate()
        .filter(|(_, stmt)| stmt.is_terminator_keyword())
        .collect();

    if return_positions.len() > 1 {
        for (_, node) in &return_positions[1..] {
            scoper.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0015,
                "Multiple terminator instructions; only one is allowed.".into(),
                "Remove all; keep one.".into(),
                None,
                node.get_span(),
            ));
        }
    }

    let break_positions: Vec<(usize, &Ast)> = nodes
        .iter()
        .enumerate()
        .filter(|(_, stmt)| stmt.is_break_keyword())
        .collect();

    if break_positions.len() > 1 {
        for (_, node) in &break_positions[1..] {
            scoper.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0015,
                "Multiple loop controller instructions; only one is allowed.".into(),
                "Remove all; keep one.".into(),
                None,
                node.get_span(),
            ));
        }
    }

    let breakall_positions: Vec<(usize, &Ast)> = nodes
        .iter()
        .enumerate()
        .filter(|(_, stmt)| stmt.is_breakall_keyword())
        .collect();

    if breakall_positions.len() > 1 {
        for (_, node) in &breakall_positions[1..] {
            scoper.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0015,
                "Multiple loop controller instructions; only one is allowed.".into(),
                "Remove all; keep one.".into(),
                None,
                node.get_span(),
            ));
        }
    }

    let continue_positions: Vec<(usize, &Ast)> = nodes
        .iter()
        .enumerate()
        .filter(|(_, stmt)| stmt.is_continue_keyword())
        .collect();

    if continue_positions.len() > 1 {
        for (_, node) in &continue_positions[1..] {
            scoper.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0015,
                "Multiple loop controller instructions; only one is allowed.".into(),
                "Remove all; keep one.".into(),
                None,
                node.get_span(),
            ));
        }
    }

    let continueall_positions: Vec<(usize, &Ast)> = nodes
        .iter()
        .enumerate()
        .filter(|(_, stmt)| stmt.is_continueall_keyword())
        .collect();

    if continueall_positions.len() > 1 {
        for (_, node) in &continueall_positions[1..] {
            scoper.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0015,
                "Multiple loop controller instructions; only one is allowed.".into(),
                "Remove all; keep one.".into(),
                None,
                node.get_span(),
            ));
        }
    }
}

pub fn check_for_unreachable_code_instructions<'ast>(scoper: &mut Scoper, node: &'ast Ast) {
    let Ast::Block { nodes, .. } = node else {
        return;
    };

    let total_nodes: usize = nodes.len();

    if total_nodes == 0 {
        return;
    }

    let Some((terminator_idx, _)) = nodes.iter().enumerate().find(|(_, stmt)| {
        stmt.is_terminator_keyword()
            || stmt.is_unreacheable_keyword()
            || stmt.is_break_keyword()
            || stmt.is_breakall_keyword()
            || stmt.is_continue_keyword()
            || stmt.is_continueall_keyword()
    }) else {
        return;
    };

    let unreachable_range: std::ops::Range<usize> = (terminator_idx + 1)..total_nodes;

    for idx in unreachable_range {
        if let Some(unreachable_node) = nodes.get(idx) {
            scoper.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0014,
                "Instruction will never be executed.".to_string(),
                "Remove it.".into(),
                None,
                unreachable_node.get_span(),
            ));
        }
    }

    let find_unreacheable_condition = |idx: usize, stmt: &'ast Ast<'ast>| match stmt {
        Ast::If {
            then_branch,
            else_if_branch,
            else_branch,
            ..
        } => {
            let mut is_if_unreacheable: bool = false;
            let mut is_else_if_unreacheable: bool = false;
            let mut is_else_unreacheable: bool = false;

            {
                if let Ast::Block { nodes, .. } = then_branch.as_ref() {
                    is_if_unreacheable = nodes.iter().any(|stmt| {
                        stmt.is_terminator_keyword()
                            || stmt.is_unreacheable_keyword()
                            || stmt.is_break_keyword()
                            || stmt.is_breakall_keyword()
                            || stmt.is_continue_keyword()
                            || stmt.is_continueall_keyword()
                    });
                }
            }

            {
                for node in else_if_branch.iter() {
                    if let Ast::Elif { block, .. } = node {
                        if let Ast::Block { nodes, .. } = &**block {
                            is_else_if_unreacheable = nodes.iter().any(|stmt| {
                                stmt.is_terminator_keyword()
                                    || stmt.is_unreacheable_keyword()
                                    || stmt.is_break_keyword()
                                    || stmt.is_breakall_keyword()
                                    || stmt.is_continue_keyword()
                                    || stmt.is_continueall_keyword()
                            });
                        }
                    }
                }
            }

            {
                if let Some(otherwise) = else_branch {
                    if let Ast::Else { block, .. } = &**otherwise {
                        if let Ast::Block { nodes, .. } = block.as_ref() {
                            is_else_unreacheable = nodes.iter().any(|stmt| {
                                stmt.is_terminator_keyword()
                                    || stmt.is_unreacheable_keyword()
                                    || stmt.is_break_keyword()
                                    || stmt.is_breakall_keyword()
                                    || stmt.is_continue_keyword()
                                    || stmt.is_continueall_keyword()
                            });
                        }
                    }
                }
            }

            let is_unreacheable_if_else: bool =
                is_if_unreacheable && is_else_unreacheable && else_if_branch.is_empty();

            let is_full_unreacheable: bool =
                is_if_unreacheable && is_else_if_unreacheable && is_else_unreacheable;

            if is_unreacheable_if_else || is_full_unreacheable {
                Some((idx, stmt))
            } else {
                None
            }
        }
        _ => None,
    };

    let Some((unreacheable_condition_idx, _)) =
        nodes.iter().enumerate().find_map(|(idx, stmt)| match stmt {
            Ast::If { .. } => find_unreacheable_condition(idx, stmt),
            _ => None,
        })
    else {
        return;
    };

    let unreachable_range: std::ops::Range<usize> = (unreacheable_condition_idx + 1)..total_nodes;

    for idx in unreachable_range {
        if let Some(unreachable_node) = nodes.get(idx) {
            scoper.add_error(CompilationIssue::Error(
                CompilationIssueCode::E0014,
                "Instruction will never be executed.".to_string(),
                "Remove it.".into(),
                None,
                unreachable_node.get_span(),
            ));
        }
    }
}
