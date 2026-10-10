#!/usr/bin/env python3

#
#     Copyright (C) 2026  Stevens Benavides
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.
#

import argparse
import difflib
import re
import shutil
import subprocess
import sys
import time

from dataclasses import dataclass
from pathlib import Path
from typing import Literal


IMPORT_PATH_RE: re.Pattern[str] = re.compile(r'import\s+"([^"]+)"')
IMPORT_C_RE: re.Pattern[str] = re.compile(r'^\s*importC\b', re.MULTILINE)
MAIN_FUNCTION_RE: re.Pattern[str] = re.compile(r'^\s*fn\s+main\b', re.MULTILINE)
STD_MATH_RE: re.Pattern[str] = re.compile(r'import\s+std::(?:math|ffi::c::math)\b')
FLOAT_VALUE_RE: re.Pattern[str] = re.compile(r'\bf(?:32|64)\b|\b\d+\.\d+')

NEGATIVE_NAME_MARKERS = (
    "_invalid",
    "_error",
    "_unknown",
    "_duplicated",
    "_duplicate",
    "duplicate_",
    "_inactive",
    "invalid_",
)

NEGATIVE_TEST_PATHS: set[str] = {
    "arithmetic/const_assign.thrust",
    "functions/named_args_positional_after.thrust",
    "functions/named_args_varargs.thrust",
}

EXPECTED_RUN_CODES: dict[str, int] = {
    "imports/module_a.thrust": 123,
    "imports/module_alias_multi.thrust": 130,
    "imports/only_multi.thrust": 15,
    "imports/only_single.thrust": 3,
    "imports/struct_only.thrust": 3,
    "imports/struct_qualified.thrust": 3,
    "functions/named_args.thrust": 139,
    "modules/reexportation/reexport_a.thrust": 3,
    "modules/named_args_module.thrust": 3,
    "optimization/disable_default_optimization.thrust": 173,
    "abi/wasm/variadic.thrust": 1,
}

SKIPPED_TEST_PATHS: set[str] = {
    "memory/load/load_index.thrust",
    "modules/reexportation/std_reexport.thrust",
    "stress/stress_test_80k.thrust",
}

@dataclass
class TestResult:
    path: Path
    kind: str
    passed: bool
    compile_code: int
    run_code: int | None
    elapsed: float
    message: str
    compile_stdout: str
    compile_stderr: str
    run_stdout: str
    run_stderr: str


def parse_args() -> argparse.Namespace:

    parser = argparse.ArgumentParser(
        description="Compile and run Thrust tests."
    )

    parser.add_argument(
        "--compiler",
        type=Path,
        help="Path to a prebuilt thrustc binary.",
    )

    parser.add_argument(
        "--cc-args",
        default="",
        help="Extra arguments forwarded to the external linker compiler.",
    )

    parser.add_argument(
        "--emit",
        action="append",
        default=[],
        help="Forward a -emit value to thrustc. Can be repeated.",
    )

    parser.add_argument(
        "--compiler-arg",
        action="append",
        default=[],
        help="Forward a raw argument directly to thrustc. Can be repeated.",
    )

    parser.add_argument(
        "--filter",
        default="",
        help="Only run tests whose relative path contains this text.",
    )

    parser.add_argument(
        "--exclude-only",
        default="",
        help="Exclude tests whose relative path contains this text.",
    )

    parser.add_argument(
        "--fail-fast",
        action="store_true",
        help="Stop after the first failed test.",
    )

    parser.add_argument(
        "--keep-dist",
        action="store_true",
        help="Keep tests/dist after the run for debugging.",
    )

    parser.add_argument(
        "--no-build",
        action="store_true",
        help="Do not build thrustc before running tests.",
    )

    parser.add_argument(
        "--compile-timeout",
        default=120,
        type=float,
        help="Maximum seconds allowed for each compilation.",
    )

    parser.add_argument(
        "--run-timeout",
        default=30,
        type=float,
        help="Maximum seconds allowed for each test binary execution.",
    )

    return parser.parse_args()

def project_root() -> Path:
    return Path(__file__).resolve().parents[2]

def tests_root(root: Path) -> Path:
    return root / "tests"

def dist_root(root: Path) -> Path:
    return tests_root(root) / "dist"

def should_skip_path(path: Path, tests_dir: Path) -> bool:
    skipped_parts: set[str] = {
        "dist",
        "scripts",
        "build",
        "c_transpile",
        "stdroot",
        "stress",
    }

    relative_parts: tuple[str, ...] = path.relative_to(tests_dir).parts
    relative_path: str = path.relative_to(tests_dir).as_posix()

    if relative_path in SKIPPED_TEST_PATHS:
        return True

    return any(part in skipped_parts for part in relative_parts)

def has_main_function(path: Path) -> bool:
    return MAIN_FUNCTION_RE.search(path.read_text(encoding="utf-8")) is not None

def discover_test_roots(
    tests_dir: Path,
    filter_text: str,
    exclude_text: str,
) -> list[Path]:

    test_roots: list[Path] = []

    for path in sorted(tests_dir.rglob("*.thrust")):

        if should_skip_path(path, tests_dir):
            continue

        relative: str = path.relative_to(tests_dir).as_posix()

        if filter_text and filter_text not in relative:
            continue

        if exclude_text and exclude_text in relative:
            continue

        if has_main_function(path):
            test_roots.append(path)

    return test_roots

def discover_c_transpile_tests(
    tests_dir: Path,
    filter_text: str,
    exclude_text: str,
) -> list[Path]:
    test_roots: list[Path] = []
    root: Path = tests_dir / "c_transpile"

    if not root.exists():
        return test_roots

    for path in sorted(root.rglob("*.c")):

        relative: str = path.relative_to(tests_dir).as_posix()

        if filter_text and filter_text not in relative:
            continue

        if exclude_text and exclude_text in relative:
            continue

        if path.with_suffix(".expected.thrust").exists():
            test_roots.append(path)

    return test_roots

def is_negative_test(path: Path) -> bool:

    name: str = path.name
    normalized_path: str = path.as_posix()

    if any(normalized_path.endswith(expected) for expected in NEGATIVE_TEST_PATHS):
        return True

    return any(marker in name for marker in NEGATIVE_NAME_MARKERS)

def expected_run_code(path: Path) -> int:
    normalized_path: str = path.as_posix()

    for expected_path, expected_code in EXPECTED_RUN_CODES.items():
        if normalized_path.endswith(expected_path):
            return expected_code

    return 0

def resolve_import_path(source_path: Path, imported: str) -> Path:
    import_path = Path(imported)

    if import_path.is_absolute():
        return import_path.resolve()

    return (source_path.parent / import_path).resolve()

def discover_user_dependencies(root_file: Path) -> list[Path]:
    dependencies: list[Path] = []
    visited: set[Path] = {root_file.resolve()}

    def visit(source_path: Path) -> None:
        content: str = source_path.read_text(encoding="utf-8")

        for import_match in IMPORT_PATH_RE.finditer(content):
            dependency: Path = resolve_import_path(source_path, import_match.group(1))

            if dependency in visited:
                continue

            visited.add(dependency)

            dependencies.append(dependency)

            if dependency.exists() and dependency.is_file():
                visit(dependency)

    visit(root_file.resolve())

    return dependencies

def uses_import_c(test_path: Path) -> bool:
    return IMPORT_C_RE.search(test_path.read_text(encoding="utf-8")) is not None

def detect_clang_system_include_dirs() -> list[str]:
    include_dirs: list[str] = []

    for command in ("clang", "clang-17", "clang-18"):

        try:
            result = subprocess.run(
                [command, "-E", "-x", "c", "-", "-v"],
                input="",
                capture_output=True,
                text=True,
                timeout=10,
            )
        except (FileNotFoundError, subprocess.TimeoutExpired):
            continue

        output: str = result.stderr or ""
        lines: list[str] = output.splitlines()
        in_search_list = False

        for line in lines:
            stripped: str = line.strip()

            if stripped == "#include <...> search starts here:":
                in_search_list = True
                continue

            if stripped == "End of search list.":
                in_search_list = False
                break

            if not in_search_list:
                continue

            normalized: str = stripped.replace(" (framework directory)", "").strip()

            if not normalized:
                continue

            path = Path(normalized)

            if path.is_dir():
                resolved = str(path.resolve())

                if resolved not in include_dirs:
                    include_dirs.append(resolved)

        if include_dirs:
            return include_dirs

    for command in ("clang", "clang-17", "clang-18"):

        try:
            result: subprocess.CompletedProcess[str] = subprocess.run(
                [command, "-print-resource-dir"],
                capture_output=True,
                text=True,
                timeout=10,
            )
        except (FileNotFoundError, subprocess.TimeoutExpired):
            continue

        if result.returncode != 0:
            continue

        resource_dir: str = result.stdout.strip()

        if not resource_dir:
            continue

        include_dir: Path = Path(resource_dir) / "include"

        if include_dir.is_dir():
            resolved = str(include_dir.resolve())

            if resolved not in include_dirs:
                include_dirs.append(resolved)

            if include_dirs:
                return include_dirs

    return include_dirs

def needs_math_linkage(files: list[Path]) -> bool:
    for path in files:

        if not path.exists():
            continue

        content: str = path.read_text(encoding="utf-8")

        if STD_MATH_RE.search(content) is not None:
            return True

        if "%" in content and FLOAT_VALUE_RE.search(content) is not None:
            return True

    return False


def compiletime_std_args(test_path: Path, root: Path) -> list[str]:

    tests_dir: Path = tests_root(root)
    relative: str = test_path.relative_to(tests_dir).as_posix()

    if not relative.startswith("compiletime/if_imports/if_std_import"):
        return ["-std", str(root / "std")]

    stdroot: Path = tests_dir / "compiletime" / "if_imports" / "stdroot"

    return [
        "-std",
        str(stdroot),
        "-std-version",
        "0.1.8",
    ]

def test_identifier(test_path: Path, root: Path) -> str:
    relative: Path = test_path.relative_to(tests_root(root)).with_suffix("")

    return "__".join(relative.parts)

def compiler_path(args: argparse.Namespace, root: Path) -> Path:
    if args.compiler is not None:
        return args.compiler.resolve()

    return root / "target" / "debug" / "thrustc"

def build_compiler(root: Path) -> int:

    print("Building thrustc...", flush=True)

    result: subprocess.CompletedProcess[str] = subprocess.run(
        ["cargo", "build", "--bin", "thrustc"],
        cwd=root,
        text=True,
    )

    return result.returncode

def quote_cc_arg(argument: str) -> str:
    if not any(char.isspace() or char == ";" for char in argument):
        return argument

    return "'" + argument.replace("'", "'\\''") + "'"

def merge_cc_args(auto_args: list[str], user_args: str) -> str:

    all_args: list[str] = []

    all_args.extend(quote_cc_arg(argument) for argument in auto_args)

    if user_args.strip():
        all_args.append(user_args.strip())

    return ";".join(all_args)

def timeout_output(value: str | bytes | None) -> str:
    if value is None:
        return ""

    if isinstance(value, bytes):
        return value.decode("utf-8", errors="replace")

    return value


def compile_test(
    test_path: Path,
    compiler: Path,
    root: Path,
    args: argparse.Namespace,
) -> tuple[subprocess.CompletedProcess[str], Path, list[Path]]:

    identifier: str = test_identifier(test_path, root)
    build_dir: Path = dist_root(root) / "build" / identifier
    binary_path: Path = dist_root(root) / "bin" / identifier
    dependencies: list[Path] = discover_user_dependencies(test_path)
    files: list[Path] = [test_path.resolve(), *dependencies]
    auto_cc_args: list[str] = []
    import_c_test: bool = uses_import_c(test_path)

    auto_cc_args.extend(["-o", str(binary_path)])

    if needs_math_linkage(files):
        auto_cc_args.append("-lm")

    cc_args: str = merge_cc_args(auto_cc_args, args.cc_args)
    command: list[str] = [
        str(compiler),
        "-build-dir",
        str(build_dir),
    ]

    command.extend(compiletime_std_args(test_path, root))
    command.extend(args.compiler_arg)

    for emit in args.emit:
        command.extend(["-emit", emit])

    if "atomics" in test_path.parts or import_c_test:
        command.extend(["-mode", "unstable"])

    if import_c_test:
        for include_dir in detect_clang_system_include_dirs():
            command.extend(["--import-c-system-include", include_dir])

    command.extend(str(path) for path in files)

    if cc_args:
        command.extend(["-cc-args", cc_args])

    result: subprocess.CompletedProcess[str] = subprocess.run(
        command,
        cwd=root,
        capture_output=True,
        text=True,
        timeout=args.compile_timeout,
    )

    return result, binary_path, dependencies


def compile_translated_output(
    source_test_path: Path,
    translated_path: Path,
    compiler: Path,
    root: Path,
    args: argparse.Namespace,
) -> subprocess.CompletedProcess[str]:

    identifier: str = test_identifier(source_test_path, root) + "__translated"
    build_dir: Path = dist_root(root) / "build" / identifier
    command: list[str] = [
        str(compiler),
        "-build-dir",
        str(build_dir),
        "-emit",
        "ast",
    ]

    command.extend(compiletime_std_args(source_test_path, root))
    command.append(str(translated_path))

    return subprocess.run(
        command,
        cwd=root,
        capture_output=True,
        text=True,
        timeout=args.compile_timeout,
    )


def compile_runtime_driver(
    source_test_path: Path,
    driver_path: Path,
    compiler: Path,
    root: Path,
    args: argparse.Namespace,
    identifier: str,
) -> tuple[subprocess.CompletedProcess[str], Path]:

    build_dir: Path = dist_root(root) / "build" / identifier
    binary_path: Path = dist_root(root) / "bin" / identifier
    dependencies: list[Path] = discover_user_dependencies(driver_path)
    files: list[Path] = [driver_path.resolve(), *dependencies]
    auto_cc_args: list[str] = ["-o", str(binary_path)]
    import_c_test: bool = any(uses_import_c(path) for path in files if path.exists())

    if needs_math_linkage(files):
        auto_cc_args.append("-lm")

    cc_args: str = merge_cc_args(auto_cc_args, args.cc_args)
    command: list[str] = [
        str(compiler),
        "-build-dir",
        str(build_dir),
        "-std",
        str(root / "std"),
    ]

    command.extend(args.compiler_arg)

    for emit in args.emit:
        command.extend(["-emit", emit])

    if import_c_test:
        command.extend(["-mode", "unstable"])

        for include_dir in detect_clang_system_include_dirs():
            command.extend(["--import-c-system-include", include_dir])

    command.extend(str(path) for path in files)

    if cc_args:
        command.extend(["-cc-args", cc_args])

    result: subprocess.CompletedProcess[str] = subprocess.run(
        command,
        cwd=root,
        capture_output=True,
        text=True,
        timeout=args.compile_timeout,
    )

    return result, binary_path

def should_skip_translated_compile_check(translated_path: Path) -> bool:

    content: str = translated_path.read_text(encoding="utf-8")

    for line in content.splitlines():
        stripped: str = line.strip()

        if stripped.startswith("directive "):
            return True

        if stripped.startswith("importC "):
            return True

        if stripped.startswith("union "):
            return True

        if stripped.startswith("enum "):
            return True

        if stripped.startswith("deref ") and " = " in stripped:
            return True

    return False

def runtime_driver_path_for_c_transpile(test_path: Path) -> Path:
    return test_path.with_suffix(".run.thrust")

def materialize_c_transpile_runtime_driver(
    test_path: Path,
    translated_path: Path,
    output_dir: Path,
) -> Path:
    source_driver_path: Path = runtime_driver_path_for_c_transpile(test_path)
    materialized_driver_path: Path = output_dir / source_driver_path.name

    translated_source: str = translated_path.read_text(encoding="utf-8")
    driver_source: str = source_driver_path.read_text(encoding="utf-8")
    materialized_driver_path.write_text(
        translated_source + "\n\n" + driver_source,
        encoding="utf-8",
    )

    return materialized_driver_path

def run_translated_runtime(
    source_test_path: Path,
    translated_root: Path,
    compiler: Path,
    root: Path,
    args: argparse.Namespace,
    identifier: str,
    start: float,
) -> TestResult:

    runtime_identifier: str = identifier + "__translated_runtime"

    try:
        runtime_compile_result, runtime_binary_path = compile_runtime_driver(
            source_test_path,
            translated_root,
            compiler,
            root,
            args,
            runtime_identifier,
        )
    except subprocess.TimeoutExpired as error:

        elapsed: float = time.monotonic() - start

        return TestResult(
            path=source_test_path,
            kind="c-transpile",
            passed=False,
            compile_code=-1,
            run_code=None,
            elapsed=elapsed,
            message=f"translated program compile timeout after {args.compile_timeout:.2f}s",
            compile_stdout=timeout_output(error.stdout),
            compile_stderr=timeout_output(error.stderr),
            run_stdout="",
            run_stderr="",
        )

    if runtime_compile_result.returncode != 0:

        elapsed = time.monotonic() - start

        return TestResult(
            path=source_test_path,
            kind="c-transpile",
            passed=False,
            compile_code=runtime_compile_result.returncode,
            run_code=None,
            elapsed=elapsed,
            message="translated program failed to compile",
            compile_stdout=runtime_compile_result.stdout,
            compile_stderr=runtime_compile_result.stderr,
            run_stdout="",
            run_stderr="",
        )

    if not runtime_binary_path.exists():

        elapsed = time.monotonic() - start

        return TestResult(
            path=source_test_path,
            kind="c-transpile",
            passed=False,
            compile_code=runtime_compile_result.returncode,
            run_code=None,
            elapsed=elapsed,
            message="translated program binary was not generated",
            compile_stdout=runtime_compile_result.stdout,
            compile_stderr=runtime_compile_result.stderr,
            run_stdout="",
            run_stderr="",
        )

    try:
        runtime_run_result: subprocess.CompletedProcess[str] = run_binary(runtime_binary_path, root, runtime_identifier, args)
    except subprocess.TimeoutExpired as error:

        elapsed = time.monotonic() - start

        return TestResult(
            path=source_test_path,
            kind="c-transpile",
            passed=False,
            compile_code=runtime_compile_result.returncode,
            run_code=-1,
            elapsed=elapsed,
            message=f"translated program runtime timeout after {args.run_timeout:.2f}s",
            compile_stdout=runtime_compile_result.stdout,
            compile_stderr=runtime_compile_result.stderr,
            run_stdout=timeout_output(error.stdout),
            run_stderr=timeout_output(error.stderr),
        )

    elapsed = time.monotonic() - start
    passed: bool = runtime_run_result.returncode == 0
    message: Literal['ok', 'translated program runtime failure, expected 0'] = "ok" if passed else "translated program runtime failure, expected 0"

    return TestResult(
        path=source_test_path,
        kind="c-transpile",
        passed=passed,
        compile_code=runtime_compile_result.returncode,
        run_code=runtime_run_result.returncode,
        elapsed=elapsed,
        message=message,
        compile_stdout=runtime_compile_result.stdout,
        compile_stderr=runtime_compile_result.stderr,
        run_stdout=runtime_run_result.stdout,
        run_stderr=runtime_run_result.stderr,
    )


def run_binary(
    binary_path: Path,
    root: Path,
    identifier: str,
    args: argparse.Namespace,
) -> subprocess.CompletedProcess[str]:

    run_dir: Path = dist_root(root) / "run" / identifier
    run_dir.mkdir(parents=True, exist_ok=True)

    return subprocess.run(
        [str(binary_path)],
        cwd=run_dir,
        capture_output=True,
        text=True,
        timeout=args.run_timeout,
    )


def run_test(
    test_path: Path,
    compiler: Path,
    root: Path,
    args: argparse.Namespace,
) -> TestResult:

    start: float = time.monotonic()
    negative: bool = is_negative_test(test_path)

    try:
        compile_result, binary_path, _dependencies = compile_test(test_path, compiler, root, args)
    except subprocess.TimeoutExpired as error:

        elapsed: float = time.monotonic() - start

        return TestResult(
            path=test_path,
            kind="negative" if negative else "positive",
            passed=False,
            compile_code=-1,
            run_code=None,
            elapsed=elapsed,
            message=f"compile timeout after {args.compile_timeout:.2f}s",
            compile_stdout=timeout_output(error.stdout),
            compile_stderr=timeout_output(error.stderr),
            run_stdout="",
            run_stderr="",
        )

    elapsed = time.monotonic() - start

    if negative:
        passed: bool = compile_result.returncode != 0 or not binary_path.exists()
        message: Literal['compile failed as expected', 'expected compile failure'] = "compile failed as expected" if passed else "expected compile failure"

        return TestResult(
            path=test_path,
            kind="negative",
            passed=passed,
            compile_code=compile_result.returncode,
            run_code=None,
            elapsed=elapsed,
            message=message,
            compile_stdout=compile_result.stdout,
            compile_stderr=compile_result.stderr,
            run_stdout="",
            run_stderr="",
        )

    if compile_result.returncode != 0:
        return TestResult(
            path=test_path,
            kind="positive",
            passed=False,
            compile_code=compile_result.returncode,
            run_code=None,
            elapsed=elapsed,
            message="compile failed",
            compile_stdout=compile_result.stdout,
            compile_stderr=compile_result.stderr,
            run_stdout="",
            run_stderr="",
        )

    if not binary_path.exists():
        if args.emit:
            return TestResult(
                path=test_path,
                kind="positive",
                passed=True,
                compile_code=compile_result.returncode,
                run_code=None,
                elapsed=elapsed,
                message="compile emitted artifact",
                compile_stdout=compile_result.stdout,
                compile_stderr=compile_result.stderr,
                run_stdout="",
                run_stderr="",
            )

        return TestResult(
            path=test_path,
            kind="positive",
            passed=False,
            compile_code=compile_result.returncode,
            run_code=None,
            elapsed=elapsed,
            message="binary was not generated",
            compile_stdout=compile_result.stdout,
            compile_stderr=compile_result.stderr,
            run_stdout="",
            run_stderr="",
        )

    identifier: str = test_identifier(test_path, root)

    try:
        run_result: subprocess.CompletedProcess[str] = run_binary(binary_path, root, identifier, args)
    except subprocess.TimeoutExpired as error:

        elapsed = time.monotonic() - start

        return TestResult(
            path=test_path,
            kind="positive",
            passed=False,
            compile_code=compile_result.returncode,
            run_code=-1,
            elapsed=elapsed,
            message=f"run timeout after {args.run_timeout:.2f}s",
            compile_stdout=compile_result.stdout,
            compile_stderr=compile_result.stderr,
            run_stdout=timeout_output(error.stdout),
            run_stderr=timeout_output(error.stderr),
        )

    elapsed = time.monotonic() - start
    expected_code: int = expected_run_code(test_path)
    passed = run_result.returncode == expected_code
    message = "ok" if passed else f"runtime failure, expected {expected_code}"

    return TestResult(
        path=test_path,
        kind="positive",
        passed=passed,
        compile_code=compile_result.returncode,
        run_code=run_result.returncode,
        elapsed=elapsed,
        message=message,
        compile_stdout=compile_result.stdout,
        compile_stderr=compile_result.stderr,
        run_stdout=run_result.stdout,
        run_stderr=run_result.stderr,
    )


def run_c_transpile_test(
    test_path: Path,
    compiler: Path,
    root: Path,
    args: argparse.Namespace,
) -> TestResult:

    start: float = time.monotonic()
    identifier: str = test_identifier(test_path, root)
    output_dir: Path = dist_root(root) / "c_transpile" / identifier
    expected_path: Path = test_path.with_suffix(".expected.thrust")
    output_path: Path = output_dir / test_path.with_suffix(".thrust").name
    command: list[str] = [
        str(compiler),
        "-mode",
        "unstable",
        "--translate-c-to-thrust",
        str(test_path),
        "--translate-c-out-dir",
        str(output_dir),
    ]

    try:
        result: subprocess.CompletedProcess[str] = subprocess.run(
            command,
            cwd=root,
            capture_output=True,
            text=True,
            timeout=args.compile_timeout,
        )
    except subprocess.TimeoutExpired as error:

        elapsed = time.monotonic() - start

        return TestResult(
            path=test_path,
            kind="c-transpile",
            passed=False,
            compile_code=-1,
            run_code=None,
            elapsed=elapsed,
            message=f"translate timeout after {args.compile_timeout:.2f}s",
            compile_stdout=timeout_output(error.stdout),
            compile_stderr=timeout_output(error.stderr),
            run_stdout="",
            run_stderr="",
        )

    elapsed: float = time.monotonic() - start

    if result.returncode != 0:
        return TestResult(
            path=test_path,
            kind="c-transpile",
            passed=False,
            compile_code=result.returncode,
            run_code=None,
            elapsed=elapsed,
            message="translate failed",
            compile_stdout=result.stdout,
            compile_stderr=result.stderr,
            run_stdout="",
            run_stderr="",
        )

    if not output_path.exists():
        return TestResult(
            path=test_path,
            kind="c-transpile",
            passed=False,
            compile_code=result.returncode,
            run_code=None,
            elapsed=elapsed,
            message=f"missing output: {output_path.name}",
            compile_stdout=result.stdout,
            compile_stderr=result.stderr,
            run_stdout="",
            run_stderr="",
        )

    expected: str = expected_path.read_text(encoding="utf-8")
    actual: str = output_path.read_text(encoding="utf-8")

    if actual != expected:

        diff: str = "".join(difflib.unified_diff(
            expected.splitlines(keepends=True),
            actual.splitlines(keepends=True),
            fromfile=expected_path.name,
            tofile=output_path.name,
        ))

        return TestResult(
            path=test_path,
            kind="c-transpile",
            passed=False,
            compile_code=result.returncode,
            run_code=None,
            elapsed=elapsed,
            message="translated output differs",
            compile_stdout=result.stdout,
            compile_stderr=result.stderr,
            run_stdout=diff,
            run_stderr="",
        )

    if not should_skip_translated_compile_check(output_path):
        translated_compile_result: subprocess.CompletedProcess[str] = compile_translated_output(
            test_path,
            output_path,
            compiler,
            root,
            args,
        )

        if translated_compile_result.returncode != 0:
            return TestResult(
                path=test_path,
                kind="c-transpile",
                passed=False,
                compile_code=translated_compile_result.returncode,
                run_code=None,
                elapsed=elapsed,
                message="translated output does not parse",
                compile_stdout=translated_compile_result.stdout,
                compile_stderr=translated_compile_result.stderr,
                run_stdout="",
                run_stderr="",
            )

    if has_main_function(output_path):
        return run_translated_runtime(
            test_path,
            output_path,
            compiler,
            root,
            args,
            identifier,
            start,
        )

    runtime_driver_source_path: Path = runtime_driver_path_for_c_transpile(test_path)

    if not runtime_driver_source_path.exists():
        return TestResult(
            path=test_path,
            kind="c-transpile",
            passed=True,
            compile_code=result.returncode,
            run_code=None,
            elapsed=elapsed,
            message="ok",
            compile_stdout=result.stdout,
            compile_stderr=result.stderr,
            run_stdout="",
            run_stderr="",
        )

    runtime_driver_path: Path = materialize_c_transpile_runtime_driver(
        test_path,
        output_path,
        output_dir,
    )
    runtime_identifier: str = identifier + "__runtime"

    try:
        runtime_compile_result, runtime_binary_path = compile_runtime_driver(
            test_path,
            runtime_driver_path,
            compiler,
            root,
            args,
            runtime_identifier,
        )
    except subprocess.TimeoutExpired as error:
        elapsed = time.monotonic() - start

        return TestResult(
            path=test_path,
            kind="c-transpile",
            passed=False,
            compile_code=-1,
            run_code=None,
            elapsed=elapsed,
            message=f"runtime driver compile timeout after {args.compile_timeout:.2f}s",
            compile_stdout=timeout_output(error.stdout),
            compile_stderr=timeout_output(error.stderr),
            run_stdout="",
            run_stderr="",
        )

    if runtime_compile_result.returncode != 0:
        return TestResult(
            path=test_path,
            kind="c-transpile",
            passed=False,
            compile_code=runtime_compile_result.returncode,
            run_code=None,
            elapsed=elapsed,
            message="translated runtime driver failed to compile",
            compile_stdout=runtime_compile_result.stdout,
            compile_stderr=runtime_compile_result.stderr,
            run_stdout="",
            run_stderr="",
        )

    if not runtime_binary_path.exists():
        return TestResult(
            path=test_path,
            kind="c-transpile",
            passed=False,
            compile_code=runtime_compile_result.returncode,
            run_code=None,
            elapsed=elapsed,
            message="translated runtime binary was not generated",
            compile_stdout=runtime_compile_result.stdout,
            compile_stderr=runtime_compile_result.stderr,
            run_stdout="",
            run_stderr="",
        )

    try:
        runtime_run_result: subprocess.CompletedProcess[str] = run_binary(runtime_binary_path, root, runtime_identifier, args)
    except subprocess.TimeoutExpired as error:

        elapsed = time.monotonic() - start

        return TestResult(
            path=test_path,
            kind="c-transpile",
            passed=False,
            compile_code=runtime_compile_result.returncode,
            run_code=-1,
            elapsed=elapsed,
            message=f"translated runtime timeout after {args.run_timeout:.2f}s",
            compile_stdout=runtime_compile_result.stdout,
            compile_stderr=runtime_compile_result.stderr,
            run_stdout=timeout_output(error.stdout),
            run_stderr=timeout_output(error.stderr),
        )

    elapsed = time.monotonic() - start
    passed: bool = runtime_run_result.returncode == 0
    message: Literal['ok', 'translated output runtime failure, expected 0'] = "ok" if passed else "translated output runtime failure, expected 0"

    return TestResult(
        path=test_path,
        kind="c-transpile",
        passed=passed,
        compile_code=runtime_compile_result.returncode,
        run_code=runtime_run_result.returncode,
        elapsed=elapsed,
        message=message,
        compile_stdout=runtime_compile_result.stdout,
        compile_stderr=runtime_compile_result.stderr,
        run_stdout=runtime_run_result.stdout,
        run_stderr=runtime_run_result.stderr,
    )


def prepare_dist(root: Path) -> None:

    dist: Path = dist_root(root)

    shutil.rmtree(dist, ignore_errors=True)
    (dist / "bin").mkdir(parents=True, exist_ok=True)
    (dist / "build").mkdir(parents=True, exist_ok=True)
    (dist / "run").mkdir(parents=True, exist_ok=True)

def cleanup_dist(root: Path, keep_dist: bool) -> None:

    if keep_dist:
        return

    shutil.rmtree(dist_root(root), ignore_errors=True)


def print_test_result(result: TestResult, root: Path) -> None:

    relative: str = result.path.relative_to(tests_root(root)).as_posix()
    status: Literal['FAIL', 'PASS'] = "PASS" if result.passed else "FAIL"
    run_code: str = "-" if result.run_code is None else str(result.run_code)

    print(
        f"[{status}] {relative} "
        f"kind={result.kind} "
        f"compile={result.compile_code} "
        f"run={run_code} "
        f"time={result.elapsed:.2f}s "
        f"{result.message}",
        flush=True,
    )


def print_running_test(test_path: Path, root: Path) -> None:
    relative: str = test_path.relative_to(tests_root(root)).as_posix()
    print(f"[RUNNING] {relative}", flush=True)


def print_failure_details(result: TestResult, root: Path) -> None:
    relative: str = result.path.relative_to(tests_root(root)).as_posix()

    print(f"\n--- {relative} ---")
    print(f"message: {result.message}")
    print(f"compile exit: {result.compile_code}")

    if result.run_code is not None:
        print(f"run exit: {result.run_code}")

    if result.compile_stdout.strip():
        print("\ncompile stdout:")
        print(result.compile_stdout.rstrip())

    if result.compile_stderr.strip():
        print("\ncompile stderr:")
        print(result.compile_stderr.rstrip())

    if result.run_stdout.strip():
        print("\nrun stdout:")
        print(result.run_stdout.rstrip())

    if result.run_stderr.strip():
        print("\nrun stderr:")
        print(result.run_stderr.rstrip())


def print_summary(results: list[TestResult], root: Path) -> None:

    total: int = len(results)
    passed: int = sum(1 for result in results if result.passed)
    failed: int = total - passed
    positives: int = sum(1 for result in results if result.kind == "positive")
    negatives: int = sum(1 for result in results if result.kind == "negative")
    c_transpiles: int = sum(1 for result in results if result.kind == "c-transpile")

    print("\nTest report", flush=True)
    print(f"total: {total}", flush=True)
    print(f"passed: {passed}", flush=True)
    print(f"failed: {failed}", flush=True)
    print(f"positive: {positives}", flush=True)
    print(f"negative: {negatives}", flush=True)
    print(f"c-transpile: {c_transpiles}", flush=True)

    failures: list[TestResult] = [result for result in results if not result.passed]

    if failures:
        print("\nFailures", flush=True)
        for result in failures:
            print_failure_details(result, root)


def main() -> int:

    args: argparse.Namespace = parse_args()
    root: Path = project_root()
    tests_dir: Path = tests_root(root)
    compiler: Path = compiler_path(args, root)

    if not args.no_build and args.compiler is None:

        build_code: int = build_compiler(root)
        if build_code != 0:
            return build_code

    if not compiler.exists():
        print(f"compiler not found: {compiler}", file=sys.stderr)
        return 1

    test_roots: list[Path] = discover_test_roots(tests_dir, args.filter, args.exclude_only)
    c_transpile_roots: list[Path] = discover_c_transpile_tests(
        tests_dir,
        args.filter,
        args.exclude_only,
    )

    if not test_roots and not c_transpile_roots:
        print("no tests found", file=sys.stderr)
        return 1

    prepare_dist(root)
    results: list[TestResult] = []

    try:

        for test_path in test_roots:
            print_running_test(test_path, root)

            result: TestResult = run_test(test_path, compiler, root, args)
            results.append(result)

            print_test_result(result, root)

            if args.fail_fast and not result.passed:
                break

        if not (args.fail_fast and results and not results[-1].passed):

            for test_path in c_transpile_roots:

                print_running_test(test_path, root)

                result = run_c_transpile_test(test_path, compiler, root, args)
                results.append(result)

                print_test_result(result, root)

                if args.fail_fast and not result.passed:
                    break

        print_summary(results, root)

        return 0 if all(result.passed for result in results) else 1

    finally:
        cleanup_dist(root, args.keep_dist)


if __name__ == "__main__":

    raise SystemExit(main())
