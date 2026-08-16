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

use thrustc_typesystem::Type;

use crate::traits::{PositionExtensions, TypeContextExtensions};

pub mod traits;

#[derive(Debug, Clone, Copy, Default)]
pub enum SynchronizationPosition {
    Statement,
    Declaration,
    Expression,

    #[default]
    NoRelevant,
}

#[derive(Debug, Clone, Copy, Default)]
pub enum Position {
    Constant,
    Static,
    Variable,
    Expression,

    #[default]
    NoRelevant,
}

#[derive(Debug)]
pub struct ControlContext {
    position: Position,
    synchronous_position: Vec<SynchronizationPosition>,
    expression_depth: u32,
    type_depth: u32,
    block_depth: u32,
}

impl ControlContext {
    #[inline]
    pub fn new() -> Self {
        Self {
            position: Position::NoRelevant,
            synchronous_position: Vec::with_capacity(u8::MAX as usize),
            expression_depth: 0,
            type_depth: 0,
            block_depth: 0,
        }
    }
}

impl ControlContext {
    #[inline]
    pub fn set_position(&mut self, position: Position) {
        self.position = position;
    }

    #[inline]
    pub fn reset_position(&mut self) {
        self.position = Position::NoRelevant;
    }

    #[inline]
    pub fn add_sync_position(&mut self, other: SynchronizationPosition) {
        self.synchronous_position.push(other);
    }

    #[inline]
    pub fn pop_sync_position(&mut self) {
        self.synchronous_position.pop();
    }

    #[inline]
    pub fn reset_sync_position(&mut self) {
        self.synchronous_position.clear();
    }

    #[inline]
    pub fn increase_expression_depth(&mut self) {
        self.expression_depth = self.expression_depth.saturating_add(1);
    }

    #[inline]
    pub fn decrease_expression_depth(&mut self) {
        self.expression_depth = self.expression_depth.saturating_sub(1);
    }

    #[inline]
    pub fn reset_expression_depth(&mut self) {
        self.expression_depth = 0;
    }

    #[inline]
    pub fn increase_type_depth(&mut self) {
        self.type_depth = self.type_depth.saturating_add(1);
    }

    #[inline]
    pub fn decrease_type_depth(&mut self) {
        self.type_depth = self.type_depth.saturating_sub(1);
    }

    #[inline]
    pub fn reset_type_depth(&mut self) {
        self.type_depth = 0;
    }

    #[inline]
    pub fn increase_block_depth(&mut self) {
        self.block_depth = self.block_depth.saturating_add(1);
    }

    #[inline]
    pub fn decrease_block_depth(&mut self) {
        self.block_depth = self.block_depth.saturating_sub(1);
    }

    #[inline]
    pub fn reset_block_depth(&mut self) {
        self.block_depth = 0;
    }
}

impl ControlContext {
    #[inline]
    pub fn get_sync_position(&self) -> Option<&SynchronizationPosition> {
        self.synchronous_position.last()
    }

    #[inline]
    pub fn get_expression_depth(&self) -> u32 {
        self.expression_depth
    }

    #[inline]
    pub fn get_type_depth(&self) -> u32 {
        self.type_depth
    }

    #[inline]
    pub fn get_block_depth(&self) -> u32 {
        self.block_depth
    }

    #[inline]
    pub fn get_position(&self) -> Position {
        self.position
    }
}

#[derive(Debug, Default)]
pub struct TypeContext {
    infered_types: Vec<Type>,
}

impl TypeContext {
    pub fn new() -> Self {
        Self {
            infered_types: Vec::with_capacity(u8::MAX as usize),
        }
    }
}

impl TypeContextExtensions for TypeContext {
    #[inline]
    fn get_infered_type(&self) -> Option<Type> {
        self.infered_types.last().cloned()
    }

    #[inline]
    fn add_infered_type(&mut self, t: Type) {
        self.infered_types.push(t);
    }

    #[inline]
    fn pop_infered_type(&mut self) {
        self.infered_types.pop();
    }

    #[inline]
    fn reset_infered_types(&mut self) {
        self.infered_types.clear();
    }
}

impl PositionExtensions for Position {
    #[inline]
    fn is_constant_position(&self) -> bool {
        matches!(self, Position::Constant)
    }

    #[inline]
    fn is_static_position(&self) -> bool {
        matches!(self, Position::Static)
    }

    #[inline]
    fn is_variable_position(&self) -> bool {
        matches!(self, Position::Variable)
    }

    #[inline]
    fn is_expression_position(&self) -> bool {
        matches!(self, Position::Expression)
    }

    #[inline]
    fn is_irrelevant_position(&self) -> bool {
        matches!(self, Position::NoRelevant)
    }
}
