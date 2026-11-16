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

#![allow(clippy::result_unit_err)]

pub mod simd;

use colored::Colorize;

use thrustc_code_location::Span;
use thrustc_errors_macros::CompilationIssueCodes;
use thrustc_logging::{self, LoggingType};

#[derive(Debug, Clone)]
pub enum CompilationIssue {
    Error(CompilationIssueCode, String, String, Option<String>, Span),
    Warning(CompilationIssueCode, String, Span),

    FrontendBug(
        String,
        String,
        Span,
        CompilationPosition,
        std::path::PathBuf,
        u32,
    ),

    BackendBug(
        String,
        String,
        Span,
        CompilationPosition,
        std::path::PathBuf,
        u32,
    ),
}

impl CompilationIssue {
    #[inline]
    pub fn is_bug(&self) -> bool {
        matches!(
            self,
            CompilationIssue::FrontendBug(..) | CompilationIssue::BackendBug(..)
        )
    }
}

lazy_static::lazy_static! {
    pub static ref COMPILATION_ISSUE_CODE_EXPLANATIONS: ahash::AHashMap<CompilationIssueCode, &'static str> = {
        let mut explanations: ahash::AHashMap<CompilationIssueCode, &'static str> = ahash::AHashMap::with_capacity(u8::MAX as usize);

        explanations.insert(CompilationIssueCode::E0001, r##"The parser expected a different token at this position. It reads the source from left to right and knows which token must come next. Compilation stops when another token appears.

The cause is usually a missing separator, a misspelled keyword, or a malformed literal. The message names the token the parser expected. Check the reported location and fix the code around it.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var total: s32 = 10   // here the error
                     ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var total: s32 = 10;
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0002, r##"The parser reached the end of the file while it still expected more tokens. This usually means a construct was left open: a missing closing brace, parenthesis, or bracket, or an unterminated string or comment. The message includes the location where the input ran out.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    if argc > 0 {
        return 1;
}   // here the error
^
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    if argc > 0 {
        return 1;
    }
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0003, r##"A compiler builtin is a function the compiler provides and evaluates at compile time. Builtins have fixed names. If the name is not registered, the call cannot be resolved.

Check the spelling and use a registered builtin.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var bytes: usize = sizeOfX(u32);   // here the error
                       ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var bytes: usize = sizeOf(u32);
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0004, r##"A name was declared twice in the same scope. Function parameters share one table, and local enums share the scope table. A duplicate entry in either one is an error. Rename one of the declarations or remove the copy.

Incorrect:
"""
fn sumar(a: s32, a: s32) s32 @public {   // here the error
               ^
    return a + a;
}
"""

Correct:
"""
fn sumar(a: s32, b: s32) s32 @public {
    return a + b;
}
"""##);

        explanations.insert(CompilationIssueCode::E0005, r##"Only one global assembler injection is allowed per compilation context. A second global_asm declaration is rejected because the generated assembly would be injected twice. Remove all but one.

Incorrect:
"""
global_asm("nop");
global_asm("ret");   // here the error
^
"""

Correct:
"""
global_asm("nop");
"""##);

        explanations.insert(CompilationIssueCode::E0006, r##"Some positions require a value known at compile time: constant and static initializers, array sizes, and the arguments of constant-folding builtins. A runtime value, such as a function parameter or a variable read, is not constant.

Replace the runtime expression with a constant expression.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    const LIMIT: s32 = argc;   // here the error
                       ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    const LIMIT: s32 = 10;
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0007, r##"A reference points to a memory location. The expression after `ref` must name a value stored in memory. The result of an arithmetic expression is a temporary, so it has no stable address.

Store the value in a variable and take the address of that variable.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var pointer: ptr[s32] = ref (1 + 2);   // here the error
                                ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var value: s32 = 1 + 2;
    var pointer: ptr[s32] = ref value;
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0008, r##"Operations that read or write through memory need an lvalue: a value with a stable memory address. A pure value, such as a literal or an arithmetic result, has no address, so it cannot be indexed or dereferenced.

Use a variable or an array element.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var first: s32 = (1 + 2)[0];   // here the error
                     ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var values: array[s32; 3] = fixed[1, 2, 3];
    var first: s32 = values[0];
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0010, r##"This code is reserved for future use. The current compiler does not emit it. Once active, it will report operations the language leaves undefined, such as signed integer overflow, division by zero, or an out-of-bounds array access.

Keep arithmetic within the range of the type.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var divisor: s32 = 0;
    var result: s32 = 10 / divisor;   // here the error
                      ^
    return result;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var divisor: s32 = 2;
    var result: s32 = 10 / divisor;
    return result;
}
"""##);

        explanations.insert(CompilationIssueCode::E0011, r##"A declaration is missing a required attribute. A function without a body that calls foreign code needs @extern. An external assembler function needs @asmSyntax. The diagnostic names the missing attribute.

Add that attribute.

Incorrect:
"""
fn printf(fmt: const array[char]) s32 @public @convention("C");   // here the error
                                      ^
"""

Correct:
"""
fn printf(fmt: const array[char]) s32 @public @extern("printf") @convention("C");
"""##);

        explanations.insert(CompilationIssueCode::E0012, r##"An attribute has an invalid value. The compiler checks each attribute argument and rejects values that do not match the expected form. Examples include an invalid alignment, an unknown assembler syntax, and conflicting constructor and destructor attributes.

Use a value the compiler accepts.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var buffer: array[u8; 64] @align(3);   // here the error
                              ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var buffer: array[u8; 64] @align(4);
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0013, r##"An attribute is not allowed on this declaration. The compiler checks each attribute against the declaration and its context. It rejects, for example, a foreign function with a body, a local constant marked @public, and a constructor that returns a value.

Remove the attribute or change the declaration.

Incorrect:
"""
fn printf(fmt: const array[char]) s32 @public @extern("printf") @convention("C") {   // here the error
                                                                                 ^
    return 0;
}
"""

Correct:
"""
fn printf(fmt: const array[char]) s32 @public @extern("printf") @convention("C");
"""##);

        explanations.insert(CompilationIssueCode::E0014, r##"A statement after a terminator never runs. A terminator is a return, unreachable, break, continue, or a branch whose paths all end in one. The compiler rejects code after it. Move the statement before the terminator or remove it.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return 0;
    var total: s32 = 10;   // here the error
    ^
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var total: s32 = 10;
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0015, r##"A block can have only one terminator. The first return, unreachable, break, or continue already ends the block, so the compiler rejects another one. Remove the extra terminator.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return 0;
    return 1;   // here the error
    ^
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0016, r##"Functions, types, enums, imports, intrinsics, and global assembly belong at module scope. Statements and expressions belong inside functions. The compiler rejects a declaration in the wrong scope.

Move it to the expected scope.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    fn helper() s32 @public {   // here the error
    ^
        return 1;
    }
    return helper();
}
"""

Correct:
"""
fn helper() s32 @public {
    return 1;
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return helper();
}
"""##);

        explanations.insert(CompilationIssueCode::E0017, r##"This code is reserved for future use. The current compiler reports loop control outside a loop as E0018. It will describe break and continue used where no loop is open. Loop control is valid only inside a loop body.

Move it into the loop.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    break;   // here the error
    ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    while true {
        break;
    }
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0018, r##"A terminator or loop control statement is outside the construct where it can appear. break, breakall, continue, and continueall are valid only inside a loop. return is valid only inside a function.

Move the statement into the loop or function.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    break;   // here the error
    ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    loop {
        break;
    }
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0019, r##"The compiler found a general type error. Examples include using void as a value, passing the wrong operand type to a builtin or operator, returning a value that does not match the declared type, a false staticAssert, and an invoked compileError.

The message names the value and its type. Fix the code so the types follow the operation.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    staticAssert(1 == 2, "one must never equal two");   // here the error
    ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    staticAssert(1 == 1, "one equals one");
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0020, r##"A value has a different type from the one expected. The compiler compares the expected type with the provided value and stops when they differ. Make the value match the expected type.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var label: s32 = "text";   // here the error
                     ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var label: s32 = 42;
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0021, r##"Two types are structurally equal but have different attributes. Structure metadata and function reference modifiers are part of the type. When code expects one set of attributes and receives another, the comparison fails even though the layout is the same. Make the attributes match.

Incorrect:
"""
struct Point @public {
    x: s32,
    y: s32
}

struct RawPoint @public @packed {
    x: s32,
    y: s32
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var point: Point = new RawPoint { x: 1, y: 2 };   // here the error
                           ^
    return 0;
}
"""

Correct:
"""
struct Point @public {
    x: s32,
    y: s32
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var point: Point = new Point { x: 1, y: 2 };
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0022, r##"A call must pass the number of arguments the signature declares. The compiler reports the expected and received counts. A variadic function declared with @arbitraryArgs is exempt. Add the missing arguments or remove the extras.

Incorrect:
"""
fn sumar(a: s32, b: s32) s32 @public {
    return a + b;
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return sumar(1);   // here the error
           ^
}
"""

Correct:
"""
fn sumar(a: s32, b: s32) s32 @public {
    return a + b;
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return sumar(1, 2);
}
"""##);

        explanations.insert(CompilationIssueCode::E0023, r##"A call has the right number of arguments, but their types do not match the parameter order. The compiler lists the expected types. Put each argument in the position that matches its type.

Incorrect:
"""
fn build(label: const array[char], count: s32) s32 @public {
    return count;
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return build(3, "items");   // here the error
                 ^
}
"""

Correct:
"""
fn build(label: const array[char], count: s32) s32 @public {
    return count;
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return build("items", 3);
}
"""##);

        explanations.insert(CompilationIssueCode::E0024, r##"A @convention attribute names a calling convention that the target architecture does not support. Each target supports a subset of the conventions the compiler knows. Use a convention from the list the diagnostic provides for the current target.

Incorrect:
"""
fn compute(value: f64) f64 @public @extern("compute") @convention("PTXKernel");   // here the error
                                                      ^
"""

Correct:
"""
fn compute(value: f64) f64 @public @extern("compute") @convention("C");
"""##);

        explanations.insert(CompilationIssueCode::E0025, r##"An intrinsic declaration names an LLVM intrinsic that the compiler does not recognize. Check the spelling and use the exact LLVM name.

Incorrect:
"""
intrinsic("llvm.doesNotExist") myIntrinsic(value: f64) f64 @public;   // here the error
          ^
"""

Correct:
"""
intrinsic("llvm.sqrt.f64") mySqrt(value: f64) f64 @public;
"""##);

        explanations.insert(CompilationIssueCode::E0026, r##"A struct constructor sets more fields than the struct defines. Each field can be set once, and only fields declared by the struct can be set. Remove the extra field.

Incorrect:
"""
struct Point {
    x: s32,
    y: s32
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var point := new Point { x: 1, y: 2, z: 3 };   // here the error
                                         ^
    return 0;
}
"""

Correct:
"""
struct Point {
    x: s32,
    y: s32
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var point := new Point { x: 1, y: 2 };
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0027, r##"A struct constructor omits fields that the struct type requires. Every field must be provided, in the order the struct declares them. Add the missing fields.

Incorrect:
"""
struct Point {
    x: s32,
    y: s32
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var point := new Point { x: 1 };   // here the error
                 ^
    return 0;
}
"""

Correct:
"""
struct Point {
    x: s32,
    y: s32
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var point := new Point { x: 1, y: 2 };
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0028, r##"A name does not exist in the current scope. It may refer to a local, parameter, constant, static, function, or imported symbol, but none of those declarations is visible here. Declare the name or correct the spelling.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var total: s32 = missing + 1;   // here the error
                     ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var total: s32 = argc + 1;
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0029, r##"This code is reserved for future use. The current compiler reports import failures as E0035. It will describe an import that cannot be resolved, such as a path that does not exist, a file with an invalid extension, or a standard library module missing from the current version. Fix the import so it points to a real module.

Incorrect:
"""
import "module_that_does_not_exist.thrust";   // here the error
       ^
"""

Correct:
"""
import "module.thrust";
"""##);

        explanations.insert(CompilationIssueCode::E0030, r##"An operator received operand types it does not support. Bitwise operators require integers, logical operators require booleans, and arithmetic operators require numeric values. The compiler rejects an operation when its operands do not fit the operator. Change an operand or use another operator.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var pointer: ptr[s32] = nullptr;
    var next: ptr[s32] = pointer + 1;   // here the error
                         ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var pointer: ptr[s32] = nullptr;
    var index: s32 = 1;
    var next: ptr[s32] = pointer[index];
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0031, r##"The typechecker does not recognize this operator and operand combination as an arithmetic or logical operation. Change the operator to one the types support.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var flag: bool = true;
    var result: bool = flag % flag;   // here the error
                            ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var flag: bool = true;
    var result: bool = flag and flag;
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0032, r##"A cast converts a value from one type to another. The compiler does not allow every pair of types to be cast directly. Use a permitted cast, or go through an intermediate type.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var pointer: ptr[s32] = nullptr;
    var character: char = pointer as char;   // here the error
                          ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var pointer: ptr[s32] = nullptr;
    var address: usize = pointer as usize;
    return address as s32;
}
"""##);

        explanations.insert(CompilationIssueCode::E0033, r##"Attributes on a declaration cannot contradict each other. Repeating an attribute or combining mutually exclusive ones, such as @inline and @noInline, is an error. Remove one of the conflicting attributes.

Incorrect:
"""
fn compute(value: s32) s32 @public @inline @noInline {   // here the error
                                           ^
    return value;
}
"""

Correct:
"""
fn compute(value: s32) s32 @public @inline {
    return value;
}
"""##);

        explanations.insert(CompilationIssueCode::E0034, r##"An overloaded LLVM intrinsic must use the dotted overload form. The name has a suffix for each overload, written as name.suffix. If the name does not contain enough segments to identify the overload, the compiler cannot resolve it. Use the full overload name.

Incorrect:
"""
intrinsic("llvm.memcpy") myMemcpy(dst: ptr, src: ptr, size: usize, volatile: bool) ptr @public;   // here the error
          ^
"""

Correct:
"""
intrinsic("llvm.memcpy.p0i8.p0i8.i64") myMemcpy(dst: ptr, src: ptr, size: usize, volatile: bool) ptr @public;
"""##);

        explanations.insert(CompilationIssueCode::E0035, r##"The compiler could not resolve an import. It rejects an import of the file itself, a path that does not exist or is not a file, a file with an invalid extension, and a standard library module missing from the current version. Check the path and module name.

Incorrect:
"""
import "module_that_does_not_exist.thrust";   // here the error
       ^
"""

Correct:
"""
import "module.thrust";
"""##);

        explanations.insert(CompilationIssueCode::E0036, r##"An assembler function has more than twelve parameters, or more than twelve of them must be passed in registers. Assembler functions allow at most twelve. Reduce the parameter count or pass values through pointers.

Incorrect:
"""
asmfn sum(a0: s32, a1: s32, a2: s32, a3: s32, a4: s32, a5: s32, a6: s32, a7: s32, a8: s32, a9: s32, a10: s32, a11: s32, a12: s32) s32 @public { "" } { "" };   // here the error
                                                                                                                        ^
"""

Correct:
"""
asmfn sum(a0: s32, a1: s32, a2: s32, a3: s32) s32 @public { "" } { "" };
"""##);

        explanations.insert(CompilationIssueCode::E0037, r##"Code, expressions, types, and blocks can nest only to a fixed depth. The compiler rejects deeper nesting. Split the code into smaller blocks or use helper functions.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    if true { if true { if true { if true { if true { return 1; } } } } }   // here the error
    ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    if true { return 1; }
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0038, r##"An assignment targets a value that is not mutable. Constants are always immutable, and references that were not declared mutable cannot be written through. Make the target mutable or remove the assignment.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    const LIMIT: s32 = 10;
    LIMIT = 20;   // here the error
    ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var limit: s32 = 10;
    limit = 20;
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0039, r##"The current target does not support this type. Some targets do not provide certain floating-point types, such as the 80-bit extended float. Use a type the target triple provides.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var value: f80 = 1.0;   // here the error
               ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var value: f64 = 1.0;
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0040, r##"A reference names a symbol that is not visible in the current scope. The name was never declared, or it was declared in a scope that is not visible here. Declare the symbol or use a visible name.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return undeclared;   // here the error
           ^
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var declared: s32 = 0;
    return declared;
}
"""##);

        explanations.insert(CompilationIssueCode::E0041, r##"The compiler could not resolve a type at compile time. The type still contains a placeholder that no expression or declaration filled in. Supply the missing type information.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var value: UnknownType = 0;   // here the error
               ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var value: s32 = 0;
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0042, r##"A qualified type reference names a type that the module does not export. The compiler resolves module::Type against the exports of that module. Use a type the module exports.

Incorrect:
"""
import "module.thrust";

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var value: module::MissingType = 0;   // here the error
                       ^
    return 0;
}
"""

Correct:
"""
import "module.thrust";

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var value: module::ExportedType = 0;
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0043, r##"More than one imported module exports this name. Without a qualifier, the compiler cannot tell which module the name comes from. Use a qualified access or restrict the imports.

Incorrect:
"""
import "module_a.thrust";
import "module_b.thrust";

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return shared();   // here the error
           ^
}
"""

Correct:
"""
import "module_a.thrust" as a;
import "module_b.thrust" as b;

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return a::shared();
}
"""##);

        explanations.insert(CompilationIssueCode::E0044, r##"A call uses a named argument the function does not declare, or named arguments on a function that does not support them. Named arguments must match parameter names. Use a declared name or pass the argument by position.

Incorrect:
"""
fn sumar(a: s32, b: s32) s32 @public {
    return a + b;
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return sumar(a = 1, c = 2);   // here the error
                        ^
}
"""

Correct:
"""
fn sumar(a: s32, b: s32) s32 @public {
    return a + b;
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return sumar(a = 1, b = 2);
}
"""##);

        explanations.insert(CompilationIssueCode::E0045, r##"The call provides a named argument twice for the same parameter, or a named argument repeats a position already filled by a positional argument. Each parameter takes one value. Remove the duplicate.

Incorrect:
"""
fn sumar(a: s32, b: s32) s32 @public {
    return a + b;
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return sumar(a = 1, a = 2);   // here the error
                        ^
}
"""

Correct:
"""
fn sumar(a: s32, b: s32) s32 @public {
    return a + b;
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return sumar(a = 1, b = 2);
}
"""##);

        explanations.insert(CompilationIssueCode::E0046, r##"Positional arguments must come before named arguments. The compiler rejects a positional argument after a named one because the order becomes ambiguous. Place all positional arguments first.

Incorrect:
"""
fn sumar(a: s32, b: s32) s32 @public {
    return a + b;
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return sumar(a = 1, 2);   // here the error
                        ^
}
"""

Correct:
"""
fn sumar(a: s32, b: s32) s32 @public {
    return a + b;
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return sumar(1, 2);
}
"""##);

        explanations.insert(CompilationIssueCode::E0047, r##"The arbitraryArg and arbitraryArgs builtins read extra arguments passed to a variadic function. They are available only inside a function declared with @arbitraryArgs. Add the attribute to the function or move the builtin.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var extra: s32 = arbitraryArg(s32);   // here the error
                     ^
    return 0;
}
"""

Correct:
"""
fn variadic(first: s32) s32 @public @arbitraryArgs {
    var extra: s32 = arbitraryArg(s32);
    return first + extra;
}
"""##);

        explanations.insert(CompilationIssueCode::E0048, r##"This builtin depends on the host platform and is not available here. hostName only works on systems that provide that service. Use a builtin the platform supports.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var name: const array[char] = hostName();   // here the error
                                  ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var name: const array[char] = hostOsName();
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0049, r##"A generic call or type provides more explicit type arguments than the generic declares. Each parameter takes one type. Remove the extra arguments or add the missing parameters to the generic.

Incorrect:
"""
fn identity[T](value: T) T @public {
    return value;
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return identity[s32, s32](1);   // here the error
           ^
}
"""

Correct:
"""
fn identity[T](value: T) T @public {
    return value;
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return identity[s32](1);
}
"""##);

        explanations.insert(CompilationIssueCode::E0050, r##"A call to a generic function passes a different number of arguments than the signature declares. The compiler reports the expected and received counts. A variadic function declared with @arbitraryArgs is exempt. Add the missing arguments or remove the extras.

Incorrect:
"""
fn sumar[T](a: T, b: T) T @public {
    return a + b;
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return sumar[s32](1);   // here the error
           ^
}
"""

Correct:
"""
fn sumar[T](a: T, b: T) T @public {
    return a + b;
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return sumar[s32](1, 2);
}
"""##);

        explanations.insert(CompilationIssueCode::E0051, r##"The compiler could not infer a generic type parameter. It needs a concrete type for every parameter, either from an explicit type argument or from the argument types. When neither provides one, the parameter stays unresolved. Pass the type in brackets or make it appear in the arguments.

Incorrect:
"""
fn identity[T](value: T) T @public {
    return value;
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return identity();   // here the error
           ^
}
"""

Correct:
"""
fn identity[T](value: T) T @public {
    return value;
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return identity[s32](1);
}
"""##);

        explanations.insert(CompilationIssueCode::E0052, r##"A generic structure constructor passes a different number of type arguments than the structure declares. Provide one type for each generic parameter.

Incorrect:
"""
struct Pair[A, B] @public {
    first: A,
    second: B
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var pair := new Pair[s32] { first: 1, second: 2 };   // here the error
                 ^
    return 0;
}
"""

Correct:
"""
struct Pair[A, B] @public {
    first: A,
    second: B
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var pair := new Pair[s32, s32] { first: 1, second: 2 };
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0053, r##"A generic type has a different number of type arguments than its declaration requires. This applies to generic structures and to generic custom types referenced by name. Provide one type for each generic parameter.

Incorrect:
"""
struct Box[T] @public {
    value: T
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var box: Box[s32, s32];   // here the error
             ^
    return 0;
}
"""

Correct:
"""
struct Box[T] @public {
    value: T
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var box: Box[s32];
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::E0054, r##"A compiler directive has an unknown flag, an invalid value, or a command-line option that cannot apply to a single source file. Directives use command-line spelling inside a string, and only file-scoped options are accepted.

Use the format shown by the command-line help. Build, target, standard-library, JIT, linker, cleanup, and process-control options stay on the command line because they affect the whole compilation.

Incorrect:
"""
directive "-target-triple=x86_64-unknown-linux-gnu";
"""

Correct:
"""
directive "-opt=O2";
""""##);

        explanations.insert(CompilationIssueCode::E0055, r##"A generic declaration lists the same type parameter more than once. Each parameter needs a distinct name. Rename the duplicate.

Incorrect:
"""
fn pair[T, T](a: T, b: T) T @public {   // here the error
             ^
    return a;
}
"""

Correct:
"""
fn pair[A, B](a: A, b: B) A @public {
    return a;
}
"""##);

        explanations.insert(CompilationIssueCode::E0056, r##"An atomic operation needs a memory location declared with an atomic ordering. The read-modify-write builtins (atomicStore, atomicAdd, atomicSubtract, atomicAnd, atomicNand, atomicOr, atomicXor, atomicSignedMaximum, atomicSignedMinimum, atomicUnsignedMaximum, atomicUnsignedMinimum) and atomicCompareAndSwap are atomic only when the target symbol has one of the atomic ordering modificators.

The atomic ordering modificators are atomicRelax, atomicGrab, atomicDrop, atomicSync, and atomicStrict. atomicNone and atomicFree do not describe an atomic memory operation. Declare the target with an atomic ordering, or use a non-atomic operation.

Incorrect:
"""
var counter: u32 = 0;                    // here the error
var old: u32 = atomicAdd(counter, 1);
"""

Correct:
"""
var atomicRelax counter: u32 = 0;
var old: u32 = atomicAdd(counter, 1);
"""##);

        explanations.insert(CompilationIssueCode::E0057, r##"A variable arguments list can carry only arguments with a scalar or pointer representation. The 'arbitraryArgFrom' builtin reads a value from a va_list with LLVM's va_arg instruction, which supports integers, floats, and pointers. Structs and arrays have no single va_arg representation, so the compiler rejects them.

Use a pointer type to receive a struct or array: pass a pointer and read it with 'ptr' or 'ptr[T]'.

Incorrect:
"""
var p: SomeStruct = arbitraryArgFrom(args, SomeStruct);   // here the error
"""

Correct:
"""
var p: ptr[SomeStruct] = arbitraryArgFrom(args, ptr[SomeStruct]);
"""##);

        explanations.insert(
            CompilationIssueCode::E0060,
            r##"SIMD code needs an explicit target CPU feature list. The compiler does not infer features from the selected CPU, so pass '-cpu-features' when SIMD code is present.

List every feature the target needs. An x86_64 build, for example, can pass '+sse2,+avx,+avx2'. Empty entries are rejected. The compiler does not resolve implications or contradictory entries, so include the complete list."##,
        );

        explanations.insert(
            CompilationIssueCode::E0061,
            r##"SIMD values are not supported in variadic functions on x86_64 System V. The variadic path skips the System V classification that places 32-byte and 64-byte NativeVector values in registers.

Pass the NativeVector through a fixed-arity function or a pointer.

Incorrect:
"""
fn forward(values: NativeVector[f32; 8]) void @public @arbitraryArgs {
}
"""

Correct:
"""
fn forwardFixed(values: NativeVector[f32; 8]) void @public {
}

fn forwardPointer(values: ptr[NativeVector[f32; 8]]) void @public @arbitraryArgs {
}
"""##,
        );

        explanations.insert(CompilationIssueCode::W0001, r##"An attribute does not apply to this kind of declaration. Each declaration kind accepts a fixed set of attributes. The compiler reports an attribute outside that set as irrelevant. Remove it.

Incorrect:
"""
fn printf(fmt: const array[char]) s32 @public @align(4);   // here the error
                    ^
"""

Correct:
"""
fn printf(fmt: const array[char]) s32 @public;
"""##);

        explanations.insert(CompilationIssueCode::W0002, r##"A @convention attribute names a convention the compiler does not know. The compiler cannot apply it, so it uses the C convention instead. Use a known convention or omit the attribute.

Incorrect:
"""
fn compute(value: f64) f64 @public @extern("compute") @convention("MysteryConv");   // here the error
                                                      ^
"""

Correct:
"""
fn compute(value: f64) f64 @public @extern("compute") @convention("C");
"""##);

        explanations.insert(CompilationIssueCode::W0003, r##"A @linkage attribute names a linkage the compiler does not know. The compiler cannot apply it, so it uses the standard C linkage instead. Use a known linkage or omit the attribute.

Incorrect:
"""
static mut counter: u32 @public @linkage("mystery") = 0;   // here the error
                                ^
"""

Correct:
"""
static mut counter: u32 @public @linkage("internal") = 0;
"""##);

        explanations.insert(CompilationIssueCode::W0004, r##"A linkage value conflicts with @public or @extern. A public or external symbol needs non-proprietary linkage; internal or linker-private linkage fails at link time. Change the linkage or remove the visibility attribute.

Incorrect:
"""
static mut counter: u32 @public @linkage("internal") = 0;   // here the error
                                ^
"""

Correct:
"""
static mut counter: u32 @public @linkage("standard") = 0;
"""##);

        explanations.insert(CompilationIssueCode::W0005, r##"A local variable is declared but never read. The variable itself has no runtime effect, but it marks dead code. Remove it or use it.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var total: s32 = 10;   // here the error
    ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var total: s32 = 10;
    return total;
}
"""##);

        explanations.insert(CompilationIssueCode::W0007, r##"This code is reserved for future use. The current compiler does not emit it. It will describe a low-level intermediate value that is declared but never used. Remove the value or use it.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var value: s32 = 1;   // here the error
    ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var value: s32 = 1;
    return value;
}
"""##);

        explanations.insert(CompilationIssueCode::W0008, r##"A function parameter is never read in the function body. Remove the parameter or use it.

Incorrect:
"""
fn sumar(a: s32, b: s32) s32 @public {   // here the error
                 ^
    return a;
}
"""

Correct:
"""
fn sumar(a: s32, b: s32) s32 @public {
    return a + b;
}
"""##);

        explanations.insert(CompilationIssueCode::W0009, r##"A static symbol is never referenced. Remove it or use it. A static marked @public is exempt because another module may reference it.

Incorrect:
"""
static mut counter: u32 = 0;   // here the error
^

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return 0;
}
"""

Correct:
"""
static mut counter: u32 = 0;

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    counter += 1;
    return counter as s32;
}
"""##);

        explanations.insert(CompilationIssueCode::W0010, r##"A constant is never used. Remove it or use it. A constant marked @public is exempt because another module may reference it.

Incorrect:
"""
const PI: f64 = 3.14159;   // here the error
^

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return 0;
}
"""

Correct:
"""
const PI: f64 = 3.14159;

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var area: f64 = PI * 2.0;
    return area as s32;
}
"""##);

        explanations.insert(CompilationIssueCode::W0011, r##"An assembler function is never called. Remove it or call it. A function marked @public is exempt because another module may reference it.

Incorrect:
"""
asmfn sumar(a: s32, b: s32) s32 @public { "" } { "" };   // here the error
^

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return 0;
}
"""

Correct:
"""
asmfn sumar(a: s32, b: s32) s32 @public { "" } { "" };

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return sumar(1, 2);
}
"""##);

        explanations.insert(CompilationIssueCode::W0012, r##"An enum type is never used. Remove it or use it. An enum marked @public is exempt because another module may reference it.

Incorrect:
"""
enum Color { Red: u32 = 0; Green: u32 = 1; }   // here the error
^

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return 0;
}
"""

Correct:
"""
enum Color { Red: u32 = 0; Green: u32 = 1; }

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var green: u32 = Color => Green;
    return green as s32;
}
"""##);

        explanations.insert(CompilationIssueCode::W0013, r##"A field of an enum is never used. Remove the field or use it.

Incorrect:
"""
enum Color { Red: u32 = 0; Green: u32 = 1; }   // here the error
             ^

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var green: u32 = Color => Green;
    return green as s32;
}
"""

Correct:
"""
enum Color { Red: u32 = 0; Green: u32 = 1; }

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var red: u32 = Color => Red;
    var green: u32 = Color => Green;
    return (red + green) as s32;
}
"""##);

        explanations.insert(CompilationIssueCode::W0014, r##"An intrinsic is never called. Remove it or call it. An intrinsic marked @public is exempt because another module may reference it.

Incorrect:
"""
intrinsic("llvm.sqrt.f64") mySqrt(value: f64) f64 @public;   // here the error
^

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return 0;
}
"""

Correct:
"""
intrinsic("llvm.sqrt.f64") mySqrt(value: f64) f64 @public;

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var root: f64 = mySqrt(2.0);
    return root as s32;
}
"""##);

        explanations.insert(CompilationIssueCode::W0015, r##"A struct type is never used. Remove it or use it. A struct marked @public is exempt because another module may reference it.

Incorrect:
"""
struct Point { x: s32, y: s32 }   // here the error
^

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return 0;
}
"""

Correct:
"""
struct Point { x: s32, y: s32 }

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var point := new Point { x: 1, y: 2 };
    return point.x;
}
"""##);

        explanations.insert(CompilationIssueCode::W0016, r##"A field of a struct is never accessed. Remove the field or use it.

Incorrect:
"""
struct Point { x: s32, y: s32 }   // here the error
                       ^

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var point := new Point { x: 1, y: 2 };
    return point.x;
}
"""

Correct:
"""
struct Point { x: s32, y: s32 }

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var point := new Point { x: 1, y: 2 };
    return point.x + point.y;
}
"""##);

        explanations.insert(CompilationIssueCode::W0017, r##"A function is never called. Remove it or call it. A function marked @public is exempt because another module may reference it.

Incorrect:
"""
fn helper() s32 @public {   // here the error
^
    return 1;
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return 0;
}
"""

Correct:
"""
fn helper() s32 @public {
    return 1;
}

fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return helper();
}
"""##);

        explanations.insert(CompilationIssueCode::W0018, r##"An import chain returns to a module that is already loading. The compiler omits the circular import so compilation can continue, but names from that module may remain undefined. Remove one of the imports to break the cycle.

Incorrect:
"""
// module_a.thrust
import "module_b.thrust";

// module_b.thrust
import "module_a.thrust";   // here the error
^
"""

Correct:
"""
// module_a.thrust
import "module_b.thrust";

// module_b.thrust
"""##);

        explanations.insert(CompilationIssueCode::W0019, r##"An unstable feature was used. Unstable features can change and may produce unexpected compiler behavior. Use a stable equivalent when one exists.

Incorrect:
"""
asmfn sumar(a: s32, b: s32) s32 @public { "" } { "" };   // here the error
^
"""

Correct:
"""
fn sumar(a: s32, b: s32) s32 @public {
    return a + b;
}
"""##);

        explanations.insert(CompilationIssueCode::W0020, r##"A variable is declared mutable but never changes. Remove the mutability or change the variable.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var counter: s32 = 0;   // here the error
    ^
    return counter;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var counter: s32 = 0;
    counter += 1;
    return counter;
}
"""##);

        explanations.insert(CompilationIssueCode::W0021, r##"A local variable has the same name as a variable in an enclosing scope. The new declaration hides the outer one. Rename one of the variables.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var value: s32 = 10;
    if value > 0 {
        var value: s32 = 20;   // here the error
        ^
        return value;
    }
    return value;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var value: s32 = 10;
    if value > 0 {
        var inner: s32 = 20;
        return inner;
    }
    return value;
}
"""##);

        explanations.insert(CompilationIssueCode::W0022, r##"A variable is assigned its own value, so the assignment changes nothing. Remove it.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var value: s32 = 10;
    value = value;   // here the error
    ^
    return value;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var value: s32 = 10;
    return value;
}
"""##);

        explanations.insert(CompilationIssueCode::W0023, r##"An empty block has no effect on the program. Remove it.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    {}   // here the error
    ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::W0024, r##"A name does not follow the naming convention for its declaration kind. Type names use PascalCase, values use lowerCamelCase, and constants use lowerCamelCase or UPPER_SNAKE. Rename the declaration.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var MY_VALUE: s32 = 10;   // here the error
        ^
    return MY_VALUE;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var myValue: s32 = 10;
    return myValue;
}
"""##);

        explanations.insert(CompilationIssueCode::W0025, r##"A loop has a constant true condition or no exit condition, and its body never reaches a terminator. It cannot end. Change the condition, or add a break, return, or unreachable path inside the loop.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    while true {   // here the error
          ^
    }
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var i: s32 = 0;
    while i < 10 {
        i += 1;
    }
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::W0026, r##"A comparison compares a value with itself. The result is always true or always false. Compare the value with something else.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var value: s32 = 10;
    if value == value {   // here the error
       ^
        return 1;
    }
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var value: s32 = 10;
    if value == 10 {
        return 1;
    }
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::W0027, r##"A value is assigned to a variable, then overwritten or lost when the variable goes out of scope, before the code reads it. The first assignment has no effect. Remove it or read the value before overwriting it.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var value: s32 = 10;
    value = 20;   // here the error
    ^
    return value;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var value: s32 = 20;
    return value;
}
"""##);

        explanations.insert(CompilationIssueCode::W0028, r##"An expression statement evaluates an expression and discards the result. If the expression has no side effects, the statement does nothing. Remove it or use the result.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    1 + 2;   // here the error
    ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var result: s32 = 1 + 2;
    return result;
}
"""##);

        explanations.insert(CompilationIssueCode::W0029, r##"The condition is constant at compile time, so the branch is always taken or always skipped. Use a condition that depends on a changing value.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    if true {   // here the error
       ^
        return 1;
    }
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    var flag: bool = true;
    if flag {
        return 1;
    }
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::W0030, r##"A module symbol is defined without the @public attribute. If another module references it, the reference fails at link time. Add the @public attribute to the declaration.

Incorrect:
"""
fn helper() s32 {   // here the error
^
    return 1;
}
"""

Correct:
"""
fn helper() s32 @public {
    return 1;
}
"""##);

        explanations.insert(CompilationIssueCode::W0031, r##"The compileWarning builtin emits a warning at compile time. The message is the string passed to it. Compilation continues. Remove the builtin to silence the warning, or keep it to mark a known limitation.

Incorrect:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    compileWarning("this build is experimental");   // here the error
    ^
    return 0;
}
"""

Correct:
"""
fn main(argc: s32, argv: ptr[array[char]]) s32 @public {
    return 0;
}
"""##);

        explanations.insert(CompilationIssueCode::W0032, r##"A generic declaration lists a type parameter that is never used. The parameter appears in the brackets but nowhere in the declaration, so it has no effect. Remove it or use it.

Incorrect:
"""
fn size[T](value: s32) s32 @public {   // here the warning
        ^
    return value;
}
"""

Correct:
"""
fn size[T](value: T) s32 @public {
    return sizeOf(T);
}
"""##);

        explanations.insert(CompilationIssueCode::W0033, r##"A variadic function has a hidden argument count parameter. The compiler prepends the number of extra arguments passed at the call site, and the body can read it with arbitraryArgsCount(). A function that forwards arbitraryArgs() to an FFI variadic, such as the vprintf family, must not have this hidden parameter because its ABI has to stay C-compatible.

Add @noArgCount to opt out.

Incorrect:
"""
fn print(fmt: CString) s32 @public @arbitraryArgs {   // here the warning
    return c::vprintf(fmt, arbitraryArgs());
}
"""

Correct:
"""
fn print(fmt: CString) s32 @public @arbitraryArgs @noArgCount {
    return c::vprintf(fmt, arbitraryArgs());
}
"""##);

        explanations.insert(
            CompilationIssueCode::E0100,
            r##"The compiler could not import C declarations through `importC`. Clang probably reported an error or fatal error while parsing the header.

Check the reported header path, include paths, defines, and C standard. If the header needs system includes, make sure they are available on the machine."##,
        );

        explanations.insert(
            CompilationIssueCode::E0110,
            r##"The compiler could not translate a C source file into Thrust (`--translate-c-to-thrust`) because the file uses a construct the translator does not support yet.

Limit the C input to the supported subset or rewrite that construct."##,
        );

        explanations.insert(
            CompilationIssueCode::E0111,
            r##"Clang reported errors while parsing a C source file in translation mode (`--translate-c-to-thrust`).

Check the input file, include paths, defines, and the C standard."##,
        );

        explanations.insert(
            CompilationIssueCode::E0112,
            r##"The compiler could not infer the type of a statement macro parameter while translating a C source file into Thrust (`--translate-c-to-thrust`).

Give the parameter a typed context, such as a typed local declaration or a cast, so the compiler can determine its type."##,
        );

        explanations.insert(
            CompilationIssueCode::W0100,
            r##"Clang emitted a diagnostic while importing a C header with `importC`. The compiler forwards the message to help debug include paths, defines, and unsupported constructs."##,
        );

        explanations.insert(
            CompilationIssueCode::W0101,
            r##"A C struct field had no name, so the importer generated a placeholder such as `field0`. The binding works, but the field name is invented."##,
        );

        explanations.insert(
            CompilationIssueCode::W0102,
            r##"The importer skipped a C union because `importC` does not support unions yet."##,
        );

        explanations.insert(
            CompilationIssueCode::W0103,
            r##"The importer skipped a C struct because it contains bitfields, which `importC` does not support yet."##,
        );

        explanations.insert(
            CompilationIssueCode::W0104,
            r##"The importer skipped a C declaration because it could not represent it safely in Thrust. The cause is an unsupported type, an incomplete type, or another unsupported construct."##,
        );

        explanations.insert(
            CompilationIssueCode::W0105,
            r##"A generated Thrust binding could not preserve the exact bytes of a C macro string literal. The emitter interpreted the macro bytes as UTF-8 and used lossy replacement for invalid sequences to produce a parseable literal.

The binding is valid Thrust, but the literal content may differ from the original C macro."##,
        );

        explanations
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, CompilationIssueCodes)]
pub enum CompilationIssueCode {
    E0001,
    E0002,
    E0003,
    E0004,
    E0005,
    E0006,
    E0007,
    E0008,
    E0010,
    E0011,
    E0012,
    E0013,
    E0014,
    E0015,
    E0016,
    E0017,
    E0018,
    E0019,
    E0020,
    E0021,
    E0022,
    E0023,
    E0024,
    E0025,
    E0026,
    E0027,
    E0028,
    E0029,
    E0030,
    E0031,
    E0032,
    E0033,
    E0034,
    E0035,
    E0036,
    E0037,
    E0038,
    E0039,
    E0040,
    E0041,
    E0042,
    E0043,
    E0044,
    E0045,
    E0046,
    E0047,
    E0048,
    E0049,
    E0050,
    E0051,
    E0052,
    E0053,
    E0054,
    E0055,
    E0056,
    E0057,
    E0060,
    E0061,

    // importC / cbindgen
    E0100,

    // translateC / transpiler
    E0110,
    E0111,
    E0112,

    W0001,
    W0002,
    W0003,
    W0004,
    W0005,
    W0007,
    W0008,
    W0009,
    W0010,
    W0011,
    W0012,
    W0013,
    W0014,
    W0015,
    W0016,
    W0017,
    W0018,
    W0019,
    W0020,
    W0021,
    W0022,
    W0023,
    W0024,
    W0025,
    W0026,
    W0027,
    W0028,
    W0029,
    W0030,
    W0031,
    W0032,
    W0033,

    // importC / cbindgen
    W0100,
    W0101,
    W0102,
    W0103,
    W0104,
    W0105,
}

#[inline]
pub fn filter_warnings(to: &[CompilationIssueCode], from: &mut Vec<CompilationIssue>) {
    from.retain(|issue| match issue {
        CompilationIssue::Warning(code, _, _) => !to.contains(code),
        _ => true,
    });
}

impl CompilationIssueCode {
    pub fn to_title(self) -> String {
        match self {
            CompilationIssueCode::E0001 => format!("SYNTAX ERROR - {}", "E0001".bright_red()),
            CompilationIssueCode::E0002 => {
                format!("UNEXPECTED END OF FILE - {}", "E0002".bright_red())
            }
            CompilationIssueCode::E0003 => {
                format!("UNKNOWN COMPILER BUILT-IN - {}", "E0003".bright_red())
            }
            CompilationIssueCode::E0004 => {
                format!("ALREADY DEFINED OR DECLARED - {}", "E0004".bright_red())
            }
            CompilationIssueCode::E0005 => {
                format!("DUPLICATED GLOBAL ASSEMBLER - {}", "E0005".bright_red())
            }
            CompilationIssueCode::E0006 => format!("NON-CONSTANT VALUE - {}", "E0006".bright_red()),
            CompilationIssueCode::E0007 => {
                format!("REFERENCE WITHOUT ADDRESS - {}", "E0007".bright_red())
            }
            CompilationIssueCode::E0008 => {
                format!("VALUE WITHOUT ADDRESS - {}", "E0008".bright_red())
            }
            CompilationIssueCode::E0010 => {
                format!("POSSIBLE UNDEFINED BEHAVIOR - {}", "E0010".bright_red())
            }
            CompilationIssueCode::E0011 => format!("MISSING ATTRIBUTE - {}", "E0011".bright_red()),
            CompilationIssueCode::E0012 => {
                format!("ATTRIBUTE SYNTAX ERROR - {}", "E0012".bright_red())
            }
            CompilationIssueCode::E0013 => {
                format!("ATTRIBUTE SITUATION ERROR - {}", "E0013".bright_red())
            }
            CompilationIssueCode::E0014 => {
                format!("UNREACHABLE INSTRUCTION - {}", "E0014".bright_red())
            }
            CompilationIssueCode::E0015 => {
                format!("TERMINATOR ALREADY DECLARED - {}", "E0015".bright_red())
            }
            CompilationIssueCode::E0016 => {
                format!("INVALID SCOPE POSITION - {}", "E0016".bright_red())
            }
            CompilationIssueCode::E0017 => {
                format!("LOOP CONTROL FLOW OUTSIDE LOOP - {}", "E0017".bright_red())
            }
            CompilationIssueCode::E0018 => format!("NOSENSE STATEMENT - {}", "E0018".bright_red()),
            CompilationIssueCode::E0019 => format!("TYPE ERROR - {}", "E0019".bright_red()),
            CompilationIssueCode::E0020 => format!("MISMATCHED TYPES - {}", "E0020".bright_red()),
            CompilationIssueCode::E0021 => {
                format!("MISMATCHED ATTRIBUTES - {}", "E0021".bright_red())
            }
            CompilationIssueCode::E0022 => {
                format!("MISSING CALL ARGUMENTS - {}", "E0022".bright_red())
            }
            CompilationIssueCode::E0023 => {
                format!("MISMATCHED CALL ARGUMENTS - {}", "E0023".bright_red())
            }
            CompilationIssueCode::E0024 => {
                format!("UNSUPPORTED CALL CONVENTION - {}", "E0024".bright_red())
            }
            CompilationIssueCode::E0025 => {
                format!("UNKNOWN COMPILER INTRINSIC - {}", "E0025".bright_red())
            }
            CompilationIssueCode::E0026 => {
                format!("TOO MANY FIELDS - {}", "E0026".bright_red())
            }
            CompilationIssueCode::E0027 => {
                format!("MISSING FIELDS - {}", "E0027".bright_red())
            }
            CompilationIssueCode::E0028 => {
                format!("UNKNOWN REFERENCE - {}", "E0028".bright_red())
            }
            CompilationIssueCode::E0029 => {
                format!("IMPORT ERROR - {}", "E0029".bright_red())
            }
            CompilationIssueCode::E0030 => {
                format!("INCOMPATIBLE OPERATION - {}", "E0030".bright_red())
            }
            CompilationIssueCode::E0031 => {
                format!("UNKNOWN OPERATION - {}", "E0031".bright_red())
            }
            CompilationIssueCode::E0032 => {
                format!("INCOMPATIBLE TYPE CAST - {}", "E0032".bright_red())
            }
            CompilationIssueCode::E0033 => {
                format!("ATTRIBUTE CONFLICT - {}", "E0033".bright_red())
            }
            CompilationIssueCode::E0034 => {
                format!("INTRINSIC SYNTAX - {}", "E0034".bright_red())
            }
            CompilationIssueCode::E0035 => {
                format!("IMPORT ERROR - {}", "E0035".bright_red())
            }
            CompilationIssueCode::E0036 => {
                format!("TOO MANY PARAMETERS - {}", "E0036".bright_red())
            }
            CompilationIssueCode::E0037 => {
                format!("TOO DEEP - {}", "E0037".bright_red())
            }
            CompilationIssueCode::E0038 => {
                format!("NOT MUTABLE - {}", "E0038".bright_red())
            }
            CompilationIssueCode::E0039 => {
                format!("UNSUPPORTED NATIVE TYPE - {}", "E0039".bright_red())
            }
            CompilationIssueCode::E0040 => {
                format!("NOT FOUND - {}", "E0040".bright_red())
            }
            CompilationIssueCode::E0041 => {
                format!("UNRESOLVED TYPE - {}", "E0041".bright_red())
            }
            CompilationIssueCode::E0042 => {
                format!("TYPE COULD NOT BE DETERMINED - {}", "E0042".bright_red())
            }
            CompilationIssueCode::E0043 => {
                format!("AMBIGUOUS IMPORTED NAME - {}", "E0043".bright_red())
            }
            CompilationIssueCode::E0044 => {
                format!("UNKNOWN NAMED ARGUMENT - {}", "E0044".bright_red())
            }
            CompilationIssueCode::E0045 => {
                format!("DUPLICATED NAMED ARGUMENT - {}", "E0045".bright_red())
            }
            CompilationIssueCode::E0046 => {
                format!("POSITIONAL AFTER NAMED ARGUMENT - {}", "E0046".bright_red())
            }
            CompilationIssueCode::E0047 => {
                format!(
                    "VARIABLE ARGUMENTS BUILTIN OUTSIDE OF A VARIADIC FUNCTION - {}",
                    "E0047".bright_red()
                )
            }
            CompilationIssueCode::E0048 => {
                format!(
                    "UNSUPPORTED BUILTIN FOR THIS PLATFORM - {}",
                    "E0048".bright_red()
                )
            }
            CompilationIssueCode::E0049 => {
                format!("TOO MANY GENERIC TYPE ARGUMENTS - {}", "E0049".bright_red())
            }
            CompilationIssueCode::E0050 => {
                format!("MISMATCHED GENERIC ARITY - {}", "E0050".bright_red())
            }
            CompilationIssueCode::E0051 => {
                format!(
                    "UNINFERRED GENERIC TYPE PARAMETER - {}",
                    "E0051".bright_red()
                )
            }
            CompilationIssueCode::E0052 => {
                format!("GENERIC STRUCT ARGUMENT COUNT - {}", "E0052".bright_red())
            }
            CompilationIssueCode::E0053 => {
                format!("GENERIC TYPE ARGUMENT COUNT - {}", "E0053".bright_red())
            }
            CompilationIssueCode::E0054 => {
                format!("INVALID COMPILER DIRECTIVE - {}", "E0054".bright_red())
            }
            CompilationIssueCode::E0055 => {
                format!("DUPLICATE TYPE PARAMETER - {}", "E0055".bright_red())
            }
            CompilationIssueCode::E0056 => {
                format!("INVALID ATOMIC OPERATION - {}", "E0056".bright_red())
            }
            CompilationIssueCode::E0057 => {
                format!("INVALID VARIADIC ARGUMENT TYPE - {}", "E0057".bright_red())
            }
            CompilationIssueCode::E0060 => {
                format!("SIMD TARGET FEATURES MISSING - {}", "E0060".bright_red())
            }
            CompilationIssueCode::E0061 => {
                format!("UNSUPPORTED SIMD VARIADIC - {}", "E0061".bright_red())
            }

            CompilationIssueCode::E0100 => {
                format!("C IMPORT FAILED - {}", "E0100".bright_red())
            }
            CompilationIssueCode::E0110 => {
                format!("C TRANSLATION UNSUPPORTED - {}", "E0110".bright_red())
            }
            CompilationIssueCode::E0111 => {
                format!("C TRANSLATION CLANG ERROR - {}", "E0111".bright_red())
            }
            CompilationIssueCode::E0112 => {
                format!("C TRANSLATION MACRO TYPE UNRESOLVED - {}", "E0112".bright_red())
            }
            CompilationIssueCode::W0001 => {
                format!("IRRELEVANT ATTRIBUTE - {}", "W0001".bright_yellow())
            }
            CompilationIssueCode::W0002 => {
                format!("UNKNOWN CALL CONVENTION - {}", "W0002".bright_yellow())
            }
            CompilationIssueCode::W0003 => format!("UNKNOWN LINKAGE - {}", "W0003".bright_yellow()),
            CompilationIssueCode::W0004 => {
                format!("ATTRIBUTE CONFLICT - {}", "W0004".bright_yellow())
            }
            CompilationIssueCode::W0005 => {
                format!("UNUSED LOCAL VARIABLE - {}", "W0005".bright_yellow())
            }
            CompilationIssueCode::W0007 => format!("UNUSED LLI - {}", "W0007".bright_yellow()),
            CompilationIssueCode::W0008 => {
                format!("UNUSED PARAMETER - {}", "W0008".bright_yellow())
            }
            CompilationIssueCode::W0009 => format!("UNUSED STATIC - {}", "W0009".bright_yellow()),
            CompilationIssueCode::W0010 => format!("UNUSED CONSTANT - {}", "W0010".bright_yellow()),
            CompilationIssueCode::W0011 => {
                format!("UNUSED ASSEMBLER FUNCTION - {}", "W0011".bright_yellow())
            }
            CompilationIssueCode::W0012 => format!("UNUSED ENUM - {}", "W0012".bright_yellow()),
            CompilationIssueCode::W0013 => {
                format!("UNUSED ENUM FIELD - {}", "W0013".bright_yellow())
            }
            CompilationIssueCode::W0014 => {
                format!("UNUSED INTRINSIC - {}", "W0014".bright_yellow())
            }
            CompilationIssueCode::W0015 => {
                format!("UNUSED STRUCTURE - {}", "W0015".bright_yellow())
            }
            CompilationIssueCode::W0016 => {
                format!("UNUSED STRUCTURE FIELD - {}", "W0016".bright_yellow())
            }
            CompilationIssueCode::W0017 => {
                format!("UNUSED FUNCTION - {}", "W0017".bright_yellow())
            }
            CompilationIssueCode::W0018 => {
                format!("CIRCULAR IMPORT - {}", "W0018".bright_yellow())
            }
            CompilationIssueCode::W0019 => {
                format!("UNSTABLE FEATURE - {}", "W0019".bright_yellow())
            }
            CompilationIssueCode::W0020 => {
                format!("MUTABLE BUT NEVER MUTATED - {}", "W0020".bright_yellow())
            }
            CompilationIssueCode::W0021 => {
                format!(
                    "NAME SHADOWS OUTER DECLARATION - {}",
                    "W0021".bright_yellow()
                )
            }
            CompilationIssueCode::W0022 => {
                format!("SELF ASSIGNMENT - {}", "W0022".bright_yellow())
            }
            CompilationIssueCode::W0023 => {
                format!("EMPTY BLOCK - {}", "W0023".bright_yellow())
            }
            CompilationIssueCode::W0024 => {
                format!("NON-STANDARD NAMING - {}", "W0024".bright_yellow())
            }
            CompilationIssueCode::W0025 => {
                format!("POSSIBLE INFINITE LOOP - {}", "W0025".bright_yellow())
            }
            CompilationIssueCode::W0026 => {
                format!("TAUTOLOGICAL COMPARISON - {}", "W0026".bright_yellow())
            }
            CompilationIssueCode::W0027 => {
                format!("DEAD STORE - {}", "W0027".bright_yellow())
            }
            CompilationIssueCode::W0028 => {
                format!("STATEMENT WITH NO EFFECT - {}", "W0028".bright_yellow())
            }
            CompilationIssueCode::W0029 => {
                format!("CONDITION ALWAYS CONSTANT - {}", "W0029".bright_yellow())
            }
            CompilationIssueCode::W0030 => {
                format!(
                    "MODULE SIGNATURE WITHOUT PUBLIC - {}",
                    "W0030".bright_yellow()
                )
            }
            CompilationIssueCode::W0031 => {
                format!("COMPILATION WARNING - {}", "W0031".bright_yellow())
            }
            CompilationIssueCode::W0032 => {
                format!("UNUSED TYPE PARAMETER - {}", "W0032".bright_yellow())
            }
            CompilationIssueCode::W0033 => {
                format!("VARIADIC ARGUMENT COUNT - {}", "W0033".bright_yellow())
            }

            CompilationIssueCode::W0100 => {
                format!("C IMPORT CLANG DIAGNOSTIC - {}", "W0100".bright_yellow())
            }
            CompilationIssueCode::W0101 => {
                format!("C IMPORT INVENTED FIELD NAME - {}", "W0101".bright_yellow())
            }
            CompilationIssueCode::W0102 => {
                format!("C IMPORT UNION SKIPPED - {}", "W0102".bright_yellow())
            }
            CompilationIssueCode::W0103 => {
                format!("C IMPORT BITFIELD STRUCT SKIPPED - {}", "W0103".bright_yellow())
            }
            CompilationIssueCode::W0104 => {
                format!("C IMPORT DECLARATION SKIPPED - {}", "W0104".bright_yellow())
            }
            CompilationIssueCode::W0105 => {
                format!(
                    "C BINDINGS EMIT STRING LOSSY - {}",
                    "W0105".bright_yellow()
                )
            }
        }
    }

    pub fn get_explanation(&self) -> &str {
        COMPILATION_ISSUE_CODE_EXPLANATIONS
            .get(self)
            .unwrap_or_else(|| {
                thrustc_logging::print_warning(
                    LoggingType::Warning,
                    &format!(
                        "Unable to get the properly '{}' issue explanation.",
                        self.to_title()
                    ),
                );

                &""
            })
    }
}

impl CompilationIssueCode {
    pub fn parse(n: &str) -> Result<Self, ()> {
        match n {
            "E0001" => Ok(CompilationIssueCode::E0001),
            "E0002" => Ok(CompilationIssueCode::E0002),
            "E0003" => Ok(CompilationIssueCode::E0003),
            "E0004" => Ok(CompilationIssueCode::E0004),
            "E0005" => Ok(CompilationIssueCode::E0005),
            "E0006" => Ok(CompilationIssueCode::E0006),
            "E0007" => Ok(CompilationIssueCode::E0007),
            "E0008" => Ok(CompilationIssueCode::E0008),
            "E0010" => Ok(CompilationIssueCode::E0010),
            "E0011" => Ok(CompilationIssueCode::E0011),
            "E0012" => Ok(CompilationIssueCode::E0012),
            "E0013" => Ok(CompilationIssueCode::E0013),
            "E0014" => Ok(CompilationIssueCode::E0014),
            "E0015" => Ok(CompilationIssueCode::E0015),
            "E0016" => Ok(CompilationIssueCode::E0016),
            "E0017" => Ok(CompilationIssueCode::E0017),
            "E0018" => Ok(CompilationIssueCode::E0018),
            "E0019" => Ok(CompilationIssueCode::E0019),
            "E0020" => Ok(CompilationIssueCode::E0020),
            "E0021" => Ok(CompilationIssueCode::E0021),
            "E0022" => Ok(CompilationIssueCode::E0022),
            "E0023" => Ok(CompilationIssueCode::E0023),
            "E0024" => Ok(CompilationIssueCode::E0024),
            "E0025" => Ok(CompilationIssueCode::E0025),
            "E0026" => Ok(CompilationIssueCode::E0026),
            "E0027" => Ok(CompilationIssueCode::E0027),
            "E0028" => Ok(CompilationIssueCode::E0028),
            "E0029" => Ok(CompilationIssueCode::E0029),
            "E0030" => Ok(CompilationIssueCode::E0030),
            "E0031" => Ok(CompilationIssueCode::E0031),
            "E0032" => Ok(CompilationIssueCode::E0032),
            "E0033" => Ok(CompilationIssueCode::E0033),
            "E0034" => Ok(CompilationIssueCode::E0034),
            "E0035" => Ok(CompilationIssueCode::E0035),
            "E0036" => Ok(CompilationIssueCode::E0036),
            "E0037" => Ok(CompilationIssueCode::E0037),
            "E0038" => Ok(CompilationIssueCode::E0038),
            "E0039" => Ok(CompilationIssueCode::E0039),
            "E0040" => Ok(CompilationIssueCode::E0040),
            "E0041" => Ok(CompilationIssueCode::E0041),
            "E0042" => Ok(CompilationIssueCode::E0042),
            "E0043" => Ok(CompilationIssueCode::E0043),
            "E0044" => Ok(CompilationIssueCode::E0044),
            "E0045" => Ok(CompilationIssueCode::E0045),
            "E0046" => Ok(CompilationIssueCode::E0046),
            "E0047" => Ok(CompilationIssueCode::E0047),
            "E0048" => Ok(CompilationIssueCode::E0048),

            "E0049" => Ok(CompilationIssueCode::E0049),
            "E0050" => Ok(CompilationIssueCode::E0050),
            "E0051" => Ok(CompilationIssueCode::E0051),
            "E0052" => Ok(CompilationIssueCode::E0052),
            "E0053" => Ok(CompilationIssueCode::E0053),
            "E0054" => Ok(CompilationIssueCode::E0054),
            "E0055" => Ok(CompilationIssueCode::E0055),
            "E0056" => Ok(CompilationIssueCode::E0056),
            "E0057" => Ok(CompilationIssueCode::E0057),
            "E0060" => Ok(CompilationIssueCode::E0060),
            "E0061" => Ok(CompilationIssueCode::E0061),

            "E0100" => Ok(CompilationIssueCode::E0100),

            "E0110" => Ok(CompilationIssueCode::E0110),
            "E0111" => Ok(CompilationIssueCode::E0111),
            "E0112" => Ok(CompilationIssueCode::E0112),

            "W0001" => Ok(CompilationIssueCode::W0001),
            "W0002" => Ok(CompilationIssueCode::W0002),
            "W0003" => Ok(CompilationIssueCode::W0003),
            "W0004" => Ok(CompilationIssueCode::W0004),
            "W0005" => Ok(CompilationIssueCode::W0005),
            "W0007" => Ok(CompilationIssueCode::W0007),
            "W0008" => Ok(CompilationIssueCode::W0008),
            "W0009" => Ok(CompilationIssueCode::W0009),
            "W0010" => Ok(CompilationIssueCode::W0010),
            "W0011" => Ok(CompilationIssueCode::W0011),
            "W0012" => Ok(CompilationIssueCode::W0012),
            "W0013" => Ok(CompilationIssueCode::W0013),
            "W0014" => Ok(CompilationIssueCode::W0014),
            "W0015" => Ok(CompilationIssueCode::W0015),
            "W0016" => Ok(CompilationIssueCode::W0016),
            "W0017" => Ok(CompilationIssueCode::W0017),
            "W0018" => Ok(CompilationIssueCode::W0018),
            "W0019" => Ok(CompilationIssueCode::W0019),
            "W0020" => Ok(CompilationIssueCode::W0020),
            "W0021" => Ok(CompilationIssueCode::W0021),
            "W0022" => Ok(CompilationIssueCode::W0022),
            "W0023" => Ok(CompilationIssueCode::W0023),
            "W0024" => Ok(CompilationIssueCode::W0024),
            "W0025" => Ok(CompilationIssueCode::W0025),
            "W0026" => Ok(CompilationIssueCode::W0026),
            "W0027" => Ok(CompilationIssueCode::W0027),
            "W0028" => Ok(CompilationIssueCode::W0028),
            "W0029" => Ok(CompilationIssueCode::W0029),
            "W0030" => Ok(CompilationIssueCode::W0030),
            "W0031" => Ok(CompilationIssueCode::W0031),
            "W0032" => Ok(CompilationIssueCode::W0032),
            "W0033" => Ok(CompilationIssueCode::W0033),

            "W0100" => Ok(CompilationIssueCode::W0100),
            "W0101" => Ok(CompilationIssueCode::W0101),
            "W0102" => Ok(CompilationIssueCode::W0102),
            "W0103" => Ok(CompilationIssueCode::W0103),
            "W0104" => Ok(CompilationIssueCode::W0104),
            "W0105" => Ok(CompilationIssueCode::W0105),

            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CompilationPosition {
    Lexer,
    Parser,
    TypeChecker,
    Analyzer,
    Linter,
    LLVMBackend,
}

impl std::fmt::Display for CompilationPosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lexer => write!(f, "{}", "Lexer".bright_blue().bold()),
            Self::Parser => write!(f, "{}", "Parser".red().bold()),
            Self::TypeChecker => write!(f, "{}", "Type Checker".bright_yellow().bold()),
            Self::Analyzer => write!(f, "{}", "Analyzer".bright_blue().bold()),
            Self::Linter => write!(f, "{}", "Linter".bright_magenta().bold()),
            Self::LLVMBackend => write!(f, "{}", "LLVMBackend".bright_red().bold()),
        }
    }
}
