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

use arbitrary::Unstructured;
use thrustc_ast::Ast;

use crate::gen_local_common::{Config, gen_root as gen_root_common};

pub fn gen_root<'ast>(u: &mut Unstructured<'ast>) -> arbitrary::Result<Ast<'ast>> {
    gen_root_with_config(u, &Config::general())
}

pub fn gen_root_with_config<'ast>(
    u: &mut Unstructured<'ast>,
    cfg: &Config,
) -> arbitrary::Result<Ast<'ast>> {
    gen_root_common(u, cfg)
}
