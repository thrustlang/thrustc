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

        explanations.insert(CompilationIssueCode::E0001, r##"A token did not match the grammar the compiler expected at that position. The parser reads the source from left to right and, at each step, knows which token must come next. When a different token appears, the compilation stops.

Most of the time the cause is a missing separator, a misspelled keyword, or a malformed literal. The message names the token the parser was waiting for. Look at the reported location and fix the code around it.

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

        explanations.insert(CompilationIssueCode::E0003, r##"A compiler builtin is a function the compiler provides and evaluates at compile time. Each builtin has a fixed name registered in the compiler. When a name that is not registered is called as a builtin, the reference cannot be resolved.

Check the spelling of the builtin and use a name that exists in the compiler registry.

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

        explanations.insert(CompilationIssueCode::E0006, r##"Some positions require a value known at compile time. Constant and static initializers, array sizes, and the arguments of constant-folding builtins all need a constant expression. A runtime value, such as a function parameter or a variable read, does not satisfy this requirement.

Replace the runtime expression with a constant one.

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

        explanations.insert(CompilationIssueCode::E0007, r##"A reference points to a memory location. When the code needs a reference that is backed by an address, the expression must name a value that actually lives in memory. Taking the address of a temporary, like the result of an arithmetic expression, produces a reference without a stable address.

Store the value in a variable first and take the address of the variable.

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

        explanations.insert(CompilationIssueCode::E0008, r##"Operations that read or write through memory need an lvalue: a value with a stable memory address. A pure value, such as a literal or the result of an arithmetic expression, has no address and cannot be indexed or dereferenced.

Use a variable or an array element that lives in memory instead of the pure value.

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

        explanations.insert(CompilationIssueCode::E0010, r##"This code is reserved. The current compiler does not emit it. It is meant to describe operations whose behavior the language does not define, such as signed integer overflow, division by zero, or access outside the bounds of an array. When it becomes active, the compiler will reject code that performs these operations.

Keep arithmetic inside the defined range of the type.

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

        explanations.insert(CompilationIssueCode::E0011, r##"A declaration requires an attribute that was not provided. A function without a body that calls into foreign code needs the @extern attribute, and an external assembler function needs the @asmSyntax attribute. The compiler names the attribute that is missing.

Add the attribute that the diagnostic asks for.

Incorrect:
"""
fn printf(fmt: const array[char]) s32 @public @convention("C");   // here the error
                                      ^
"""

Correct:
"""
fn printf(fmt: const array[char]) s32 @public @extern("printf") @convention("C");
"""##);

        explanations.insert(CompilationIssueCode::E0012, r##"An attribute was written with a value that is not valid. The compiler checks the arguments of each attribute and reports when they do not follow the expected form. Invalid alignment values, unknown assembler syntax, and mutually exclusive constructor and destructor attributes are examples of this error.

Write the attribute with a value the compiler accepts.

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

        explanations.insert(CompilationIssueCode::E0013, r##"An attribute was applied in a place where it is not allowed. The compiler validates each attribute against the declaration that carries it and the values around it. A foreign function with a body, a local constant with public visibility, or a constructor that returns a value are all rejected.

Remove the attribute or change the declaration so the attribute is valid there.

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

        explanations.insert(CompilationIssueCode::E0014, r##"A statement placed after a terminator will never run. A terminator ends the flow of a block: return, unreachable, break, continue, or a branch where every path ends in a terminator. Code that follows it is dead and the compiler rejects it. Remove the statement or move it before the terminator.

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

        explanations.insert(CompilationIssueCode::E0015, r##"A block can only have one terminator. A second return, unreachable instruction, or loop control instruction in the same block is rejected because the first one already ends the block. Remove all but one.

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

        explanations.insert(CompilationIssueCode::E0016, r##"Every declaration has a scope where it belongs. Functions, types, enums, imports, intrinsics and global assembly only exist at module scope. Statements and expressions do not exist at module scope; they live inside functions. Using a declaration in the wrong scope is an error.

Move the declaration to the scope the compiler expects.

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

        explanations.insert(CompilationIssueCode::E0017, r##"This code is reserved. The current compiler reports loop control outside of a loop as E0018. This code describes break and continue statements used where no loop is open. Loop control only makes sense inside a loop body.

Move the loop control inside the loop it belongs to.

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

        explanations.insert(CompilationIssueCode::E0018, r##"A terminator or loop control statement was used outside the construct it belongs to. break, breakall, continue and continueall are only valid inside a loop. return is only valid inside a function. Using them elsewhere is an error.

Move the statement inside the loop or function it belongs to.

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

        explanations.insert(CompilationIssueCode::E0019, r##"A type was used in a way the compiler does not allow. This is a general error that covers many situations: void used as a value, an operand of the wrong type for a builtin or an operation, a return that does not match the declared type, a staticAssert whose condition is false, and an invoked compileError.

The message points at the value and the type involved. Fix the code so the types follow the rules of the operation.

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

        explanations.insert(CompilationIssueCode::E0020, r##"A value of one type was used where another type is required. The compiler compares the expected type with the type of the provided value and stops when they are not the same. Make the value match the expected type.

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

        explanations.insert(CompilationIssueCode::E0021, r##"Two types are structurally equal but carry different attributes. Structure metadata and function reference modifiers are part of the type. When code expects a type with one set of attributes and receives a type with another, the comparison fails even though the layout is the same. Make the attributes match.

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

        explanations.insert(CompilationIssueCode::E0022, r##"A function call must provide the number of arguments the signature declares. When the call passes a different total, the compiler reports the expected count and the received count. A variadic function declared with @arbitraryArgs is exempt from this rule. Fill the call with the missing arguments or remove the extras.

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

        explanations.insert(CompilationIssueCode::E0023, r##"A call provides the right number of arguments, but the types are not in the order the signature declares. The compiler lists the expected types so the call can be reordered. Put each argument in the position that matches its type.

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

        explanations.insert(CompilationIssueCode::E0025, r##"An intrinsic declaration names an LLVM intrinsic that the compiler does not recognize. The name must match an intrinsic the compiler knows. Check the spelling and use the exact LLVM name.

Incorrect:
"""
intrinsic("llvm.doesNotExist") myIntrinsic(value: f64) f64 @public;   // here the error
          ^
"""

Correct:
"""
intrinsic("llvm.sqrt.f64") mySqrt(value: f64) f64 @public;
"""##);

        explanations.insert(CompilationIssueCode::E0026, r##"A struct constructor provides more fields than the struct type defines. Each struct field can be set once, and no field that is not part of the struct can be set. Remove the extra field.

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

        explanations.insert(CompilationIssueCode::E0028, r##"A name was referenced that does not exist in the current scope. The reference can point to a local, a parameter, a constant, a static, a function, or an imported symbol. If the name is not in any of those tables, the reference cannot be resolved. Declare the name or correct the spelling.

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

        explanations.insert(CompilationIssueCode::E0029, r##"This code is reserved. The current compiler reports import failures as E0035. This code describes an import that cannot be resolved: a path that does not exist, a file with an invalid extension, or a standard library module that is not part of the current version. Fix the import so it points to a real module.

Incorrect:
"""
import "module_that_does_not_exist.thrust";   // here the error
       ^
"""

Correct:
"""
import "module.thrust";
"""##);

        explanations.insert(CompilationIssueCode::E0030, r##"An operator was applied to operand types it does not support. Bitwise operations require integers, logical operations require booleans, and arithmetic operations require numeric values. When the operand types do not follow the rule of the operator, the operation is rejected. Change the operand or the operator.

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

        explanations.insert(CompilationIssueCode::E0031, r##"An operator was used that the typechecker does not recognize as a valid arithmetic or logical operation. The operator and its operand types fall outside every rule the compiler defines. Change the operator to one the types support.

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

        explanations.insert(CompilationIssueCode::E0032, r##"A cast converts a value from one type to another. Not every pair of types can be cast directly. When the compiler does not allow the conversion, the cast is rejected. Use a cast the compiler permits, or go through an intermediate type.

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

        explanations.insert(CompilationIssueCode::E0033, r##"Attributes on a declaration must not contradict each other. Repeating the same attribute or combining mutually exclusive ones, like @inline with @noInline, is an error. Remove the attribute that conflicts.

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

        explanations.insert(CompilationIssueCode::E0034, r##"An overloaded LLVM intrinsic must use the dotted overload form. The name carries a suffix for each overload, written as name.suffix. When the name does not contain enough segments to identify the overload, the compiler cannot resolve it. Use the full overload name.

Incorrect:
"""
intrinsic("llvm.memcpy") myMemcpy(dst: ptr, src: ptr, size: usize, volatile: bool) ptr @public;   // here the error
          ^
"""

Correct:
"""
intrinsic("llvm.memcpy.p0i8.p0i8.i64") myMemcpy(dst: ptr, src: ptr, size: usize, volatile: bool) ptr @public;
"""##);

        explanations.insert(CompilationIssueCode::E0035, r##"An import could not be resolved. The compiler rejects imports of the file itself, paths that do not exist or do not point to a file, files with an invalid extension, and standard library modules that are not present in the current version. Check the path and the module name.

Incorrect:
"""
import "module_that_does_not_exist.thrust";   // here the error
       ^
"""

Correct:
"""
import "module.thrust";
"""##);

        explanations.insert(CompilationIssueCode::E0036, r##"An assembler function signature exceeds the limit of twelve parameters, or more than twelve parameters have to be passed in registers. Assembler functions have a fixed parameter budget. Reduce the number of parameters or pass the values through pointers.

Incorrect:
"""
asmfn sum(a0: s32, a1: s32, a2: s32, a3: s32, a4: s32, a5: s32, a6: s32, a7: s32, a8: s32, a9: s32, a10: s32, a11: s32, a12: s32) s32 @public { "" } { "" };   // here the error
                                                                                                                        ^
"""

Correct:
"""
asmfn sum(a0: s32, a1: s32, a2: s32, a3: s32) s32 @public { "" } { "" };
"""##);

        explanations.insert(CompilationIssueCode::E0037, r##"Code, expressions, types, and blocks are only allowed to nest up to a fixed depth. Deeper nesting is rejected to keep the compiler bounded. Flatten the structure: split the code into smaller blocks or use helper functions.

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

        explanations.insert(CompilationIssueCode::E0039, r##"A type that the current target does not support was used. Some targets lack certain floating-point types, such as the 80-bit extended float. Check which types the target triple provides and use one of them.

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

        explanations.insert(CompilationIssueCode::E0040, r##"A reference names a symbol that is not in the symbol table of the current scope. The name was never declared, or it was declared in a scope that is not visible here. Create the symbol or reference it correctly.

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

        explanations.insert(CompilationIssueCode::E0041, r##"A type could not be resolved at compile time. The type still contains a placeholder that no expression or declaration filled in. Resolve the type so the compiler can determine it.

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

        explanations.insert(CompilationIssueCode::E0042, r##"A qualified type reference names a type that the module does not export. The compiler resolves module::Type against the exports of that module. Use a type the module actually exports.

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

        explanations.insert(CompilationIssueCode::E0043, r##"A name is exported by more than one imported module. When the name is used without qualification, the compiler cannot decide which module it comes from. Disambiguate the reference, either with a qualified access or by restricting the imports.

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

        explanations.insert(CompilationIssueCode::E0044, r##"A call uses a named argument that the function does not declare, or uses named arguments on a function that does not support them. Named arguments must match the parameter names of the callee. Use a declared parameter name, or fall back to positional arguments.

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

        explanations.insert(CompilationIssueCode::E0045, r##"A named argument was provided twice for the same parameter, or a named argument repeats a position already filled by a positional argument. Each parameter can receive only one value. Remove the duplicated argument.

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

        explanations.insert(CompilationIssueCode::E0046, r##"Positional arguments must come before named arguments in a call. A positional argument that follows a named one is rejected because it makes the order ambiguous. Place all positional arguments first.

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

        explanations.insert(CompilationIssueCode::E0047, r##"The arbitraryArg and arbitraryArgs builtins read the extra arguments of a variadic function. They are only available inside a function declared with the @arbitraryArgs attribute. Add the attribute to the function or move the builtin.

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

        explanations.insert(CompilationIssueCode::E0048, r##"A builtin that depends on the host platform is not available on the current one. The hostName and sysconf-based builtins only work on systems that provide the underlying service. Use a builtin the platform supports.

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

        explanations.insert(CompilationIssueCode::E0049, r##"A generic call or type provides more explicit type arguments than the generic declares. Each generic parameter accepts exactly one type, so passing more arguments than there are parameters is rejected. Remove the extra type arguments or declare the missing parameters.

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

        explanations.insert(CompilationIssueCode::E0050, r##"A call to a generic function provides a number of arguments that does not match the number of parameters the signature declares. The compiler compares the received count with the expected count and stops when they differ. A variadic function declared with @arbitraryArgs is exempt from this rule. Fill the call with the missing arguments or remove the extras.

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

        explanations.insert(CompilationIssueCode::E0051, r##"A generic type parameter could not be inferred. The compiler needs to know the concrete type of every generic parameter, either from an explicit type argument or from the types of the arguments. When neither provides the type, the parameter stays unresolved. Provide the type explicitly between brackets or make it appear in the arguments.

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

        explanations.insert(CompilationIssueCode::E0052, r##"A generic structure constructor provides a number of type arguments that does not match the number of type parameters the structure declares. Each generic parameter accepts exactly one type. Provide one type per generic parameter of the structure.

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

        explanations.insert(CompilationIssueCode::E0053, r##"A generic type is used with a number of type arguments that does not match the number of type parameters it declares. This applies to generic structures and generic custom types referenced by name. Each generic parameter accepts exactly one type. Provide one type per generic parameter.

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

        explanations.insert(CompilationIssueCode::E0054, r##"A compiler directive contains an unknown flag, an invalid value, or a command-line option that cannot be applied to a single source file. Directives use command-line spelling inside a string and only file-scoped compilation options are accepted.

Use the value format shown by the command-line help. Build, target, standard-library, JIT, linker, cleanup, and process-control options must remain on the command line because they affect the complete compilation rather than one file.

Incorrect:
"""
directive "-target-triple=x86_64-unknown-linux-gnu";
"""

Correct:
"""
directive "-opt=O2";
""""##);

        explanations.insert(CompilationIssueCode::E0055, r##"A generic declaration lists the same type parameter more than once. Each type parameter needs a distinct name so the compiler can tell them apart. Rename the duplicate so every generic parameter is unique.

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

        explanations.insert(CompilationIssueCode::W0001, r##"An attribute was attached to a declaration kind it does not apply to. Each kind of declaration accepts a fixed set of attributes. An attribute outside that set has no meaning there and is reported as irrelevant. Remove the attribute.

Incorrect:
"""
struct Pair @public @align(4) {   // here the error
                    ^
    a: u32,
    b: u32
}
"""

Correct:
"""
struct Pair @public {
    a: u32,
    b: u32
}
"""##);

        explanations.insert(CompilationIssueCode::W0002, r##"A @convention attribute names a convention the compiler does not know. The compiler cannot apply it, so it falls back to the C convention and keeps going. Use a convention from the list of known values, or omit the attribute.

Incorrect:
"""
fn compute(value: f64) f64 @public @extern("compute") @convention("MysteryConv");   // here the error
                                                      ^
"""

Correct:
"""
fn compute(value: f64) f64 @public @extern("compute") @convention("C");
"""##);

        explanations.insert(CompilationIssueCode::W0003, r##"A @linkage attribute names a linkage the compiler does not know. The compiler cannot apply it, so it falls back to the standard C linkage and keeps going. Use a linkage from the list of known values, or omit the attribute.

Incorrect:
"""
static mut counter: u32 @public @linkage("mystery") = 0;   // here the error
                                ^
"""

Correct:
"""
static mut counter: u32 @public @linkage("internal") = 0;
"""##);

        explanations.insert(CompilationIssueCode::W0004, r##"A linkage value contradicts the @public or @extern attribute. A public or external symbol needs non-proprietary linkage; combining it with internal or linker-private linkage fails at link time. Change the linkage or remove the visibility attribute.

Incorrect:
"""
static mut counter: u32 @public @linkage("internal") = 0;   // here the error
                                ^
"""

Correct:
"""
static mut counter: u32 @public @linkage("standard") = 0;
"""##);

        explanations.insert(CompilationIssueCode::W0005, r##"A local variable was declared but never read afterwards. The variable occupies nothing at runtime, but it signals dead code. Remove the variable or use it.

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

        explanations.insert(CompilationIssueCode::W0007, r##"This code is reserved. The current compiler does not emit it. It is meant to describe a low-level intermediate value that was declared but never used. A declared value that is never referenced serves no purpose. Remove it or use it.

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

        explanations.insert(CompilationIssueCode::W0008, r##"A function parameter was never read inside the function body. The parameter is part of the signature, but the body does not use it. Remove the parameter or use it.

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

        explanations.insert(CompilationIssueCode::W0009, r##"A static symbol was declared but never referenced. A static that nothing reads or writes serves no purpose. Remove it or use it. Statics marked @public are exempt because other modules may reference them.

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

        explanations.insert(CompilationIssueCode::W0010, r##"A constant was declared but never used. A constant that is never referenced serves no purpose. Remove it or use it. Constants marked @public are exempt because other modules may reference them.

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

        explanations.insert(CompilationIssueCode::W0011, r##"An assembler function was declared but never called. An asmfn that nothing invokes serves no purpose. Remove it or call it. Functions marked @public are exempt because other modules may reference them.

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

        explanations.insert(CompilationIssueCode::W0012, r##"An enum type was declared but never used. An enum that nothing references serves no purpose. Remove it or use it. Enums marked @public are exempt because other modules may reference them.

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

        explanations.insert(CompilationIssueCode::W0013, r##"A field of an enum was never used. An enum field that no code reads serves no purpose. Remove the field or use it.

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

        explanations.insert(CompilationIssueCode::W0014, r##"An intrinsic was declared but never called. An intrinsic that nothing invokes serves no purpose. Remove it or call it. Intrinsics marked @public are exempt because other modules may reference them.

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

        explanations.insert(CompilationIssueCode::W0015, r##"A struct type was declared but never used. A struct that nothing constructs or references serves no purpose. Remove it or use it. Structs marked @public are exempt because other modules may reference them.

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

        explanations.insert(CompilationIssueCode::W0016, r##"A field of a struct was never accessed. A struct field that no code reads or writes serves no purpose. Remove the field or use it.

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

        explanations.insert(CompilationIssueCode::W0017, r##"A function was declared but never called. A function that nothing invokes serves no purpose. Remove it or call it. Functions marked @public are exempt because other modules may reference them.

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

        explanations.insert(CompilationIssueCode::W0018, r##"An import chain returns to a module that is already being loaded. The circular import is omitted so the compilation can continue, but the missing module may leave names undefined. Break the cycle by removing one of the imports.

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

        explanations.insert(CompilationIssueCode::W0019, r##"A feature that is still unstable was used. Unstable features can change and may produce unexpected compiler behavior. They are intended for experimentation. Prefer the stable equivalent when one exists.

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

        explanations.insert(CompilationIssueCode::W0020, r##"A variable was declared mutable but never changed. The mutability adds nothing if the value is only read. Remove the mutability or mutate the variable.

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

        explanations.insert(CompilationIssueCode::W0021, r##"A local variable has the same name as a variable declared in an enclosing scope. The new declaration hides the outer one, which is often a mistake. Rename one of the variables.

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

        explanations.insert(CompilationIssueCode::W0022, r##"A variable is assigned its own value. The assignment changes nothing. Remove it.

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

        explanations.insert(CompilationIssueCode::W0023, r##"A block contains no statements. It has no effect on the program. Remove it.

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

        explanations.insert(CompilationIssueCode::W0024, r##"A name does not follow the naming convention of its kind. Type names use PascalCase, values use lowerCamelCase, and constants use lowerCamelCase or UPPER_SNAKE. Rename the declaration to follow the convention.

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

        explanations.insert(CompilationIssueCode::W0025, r##"A loop has a constant true condition or no exit condition, and its body never reaches a terminator. The loop can never end. Add a condition that changes, or a break, return, or unreachable path inside the loop.

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

        explanations.insert(CompilationIssueCode::W0026, r##"A comparison compares a value with itself. The result is always true or always false, so the comparison adds nothing. Compare the value with another value instead.

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

        explanations.insert(CompilationIssueCode::W0027, r##"A value is assigned to a variable and overwritten, or the variable goes out of scope, before the value is ever read. The assignment has no effect. Remove the first assignment or read the value before overwriting it.

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

        explanations.insert(CompilationIssueCode::W0028, r##"An expression statement evaluates an expression and discards the result. When the expression has no side effects, the statement does nothing. Remove it or use the result.

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

        explanations.insert(CompilationIssueCode::W0029, r##"A condition folds to a constant at compile time. The branch is always taken or always skipped. Use a condition that depends on a value that changes.

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

        explanations.insert(CompilationIssueCode::W0031, r##"The compileWarning builtin emits a warning at compile time. The message is the string passed to the builtin. It does not stop the compilation. Remove the builtin to silence the warning, or keep it to mark a known limitation.

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

        explanations.insert(CompilationIssueCode::W0032, r##"A generic declaration lists a type parameter that is never used. The parameter appears in the brackets but never in the signature, the fields, or the body, so it has no effect on the code. Remove the parameter from the brackets or use it in the declaration.

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
