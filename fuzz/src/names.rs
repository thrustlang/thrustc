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

pub const IDENTIFIER_POOL: &[&str] = &[
    "value",
    "result",
    "index",
    "counter",
    "sum",
    "total",
    "count",
    "item",
    "element",
    "data",
    "buffer",
    "size",
    "length",
    "capacity",
    "offset",
    "node",
    "next",
    "previous",
    "current",
    "first",
    "last",
    "temp",
    "helper",
    "worker",
    "target",
    "source",
    "destination",
    "input",
    "output",
    "key",
    "part",
    "entry",
    "record",
    "field",
    "flag",
    "status",
    "code",
    "message",
    "state",
    "region",
    "chunk",
    "label",
    "tag",
    "slot",
    "frame",
    "mask",
    "weight",
    "amount",
    "limit",
    "step",
    "x",
    "y",
    "z",
    "w",
    "i",
    "j",
    "k",
    "n",
    "m",
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "foo",
    "bar",
    "baz",
    "qux",
    "quux",
    "corge",
    "grault",
    "garply",
    "waldo",
    "fred",
    "plugh",
    "xyzzy",
    "thud",
    "lorem",
    "ipsum",
    "dolor",
];

#[inline]
pub fn gen_name<'ast>(u: &mut Unstructured<'ast>) -> arbitrary::Result<&'ast str> {
    Ok(*u.choose(IDENTIFIER_POOL)?)
}
