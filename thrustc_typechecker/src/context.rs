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
use thrustc_typesystem::Type;

#[derive(Debug, Clone, Copy)]
pub struct TypeCheckerControlContext {
    checking_depth: u32,
    type_cast_depth: u32,
    node_depth: u32,
}

impl TypeCheckerControlContext {
    #[inline]
    pub fn new() -> Self {
        Self {
            checking_depth: 0,
            type_cast_depth: 0,
            node_depth: 0,
        }
    }
}

impl TypeCheckerControlContext {
    #[inline]
    pub fn increase_checking_depth(&mut self) {
        self.checking_depth = self.checking_depth.saturating_add(1)
    }

    #[inline]
    pub fn reset_checking_depth(&mut self) {
        self.checking_depth = 0;
    }

    #[inline]
    pub fn increase_type_cast_depth(&mut self) {
        self.type_cast_depth = self.type_cast_depth.saturating_add(1);
    }

    #[inline]
    pub fn reset_type_cast_depth(&mut self) {
        self.type_cast_depth = 0;
    }

    #[inline]
    pub fn enter_node(&mut self) {
        self.node_depth = self.node_depth.saturating_add(1);
    }

    #[inline]
    pub fn leave_node(&mut self) {
        self.node_depth = self.node_depth.saturating_sub(1);
    }

    #[inline]
    pub fn reset_node_depth(&mut self) {
        self.node_depth = 0;
    }
}

impl TypeCheckerControlContext {
    #[inline]
    pub fn get_checking_depth(&self) -> u32 {
        self.checking_depth
    }

    #[inline]
    pub fn get_type_cast_depth(&self) -> u32 {
        self.type_cast_depth
    }

    #[inline]
    pub fn get_node_depth(&self) -> u32 {
        self.node_depth
    }
}

#[derive(Debug)]
pub struct TypeCheckerTypeContext<'type_checker> {
    current_function_type: Option<(&'type_checker Type, Span)>,
    current_function_is_variadic: bool,
    call_depth: u64,
}

impl<'type_checker> TypeCheckerTypeContext<'type_checker> {
    #[inline]
    pub fn new() -> Self {
        Self {
            current_function_type: None,
            current_function_is_variadic: false,
            call_depth: 0,
        }
    }
}

impl<'type_checker> TypeCheckerTypeContext<'type_checker> {
    #[inline]
    pub fn set_current_function_type(&mut self, function_type: (&'type_checker Type, Span)) {
        self.current_function_type = Some(function_type);
    }

    #[inline]
    pub fn unset_current_function_type(&mut self) {
        self.current_function_type = None;
    }

    #[inline]
    pub fn set_current_function_variadic(&mut self) {
        self.current_function_is_variadic = true;
    }

    #[inline]
    pub fn unset_current_function_variadic(&mut self) {
        self.current_function_is_variadic = false;
    }

    #[inline]
    pub fn increase_call_depth(&mut self) {
        self.call_depth = self.call_depth.saturating_add(1)
    }

    #[inline]
    pub fn reset_call_depth(&mut self) {
        self.call_depth = 0;
    }
}

impl<'type_checker> TypeCheckerTypeContext<'type_checker> {
    #[inline]
    pub fn get_current_function_type(&self) -> Option<(&'type_checker Type, Span)> {
        self.current_function_type
    }

    #[inline]
    pub fn is_current_function_variadic(&self) -> bool {
        self.current_function_is_variadic
    }
}
