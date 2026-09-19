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
import os
import re
import shutil
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path


IMPORT_PATH_RE = re.compile(r'import\s+"([^"]+)"')
MAIN_FUNCTION_RE = re.compile(r'^\s*fn\s+main\b', re.MULTILINE)
STD_MATH_RE = re.compile(r'import\s+std::(?:math|ffi::c::math)\b')
FLOAT_VALUE_RE = re.compile(r'\bf(?:32|64)\b|\b\d+\.\d+')

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

NEGATIVE_TEST_PATHS = {
    "arithmetic/const_assign.thrust",
    "functions/named_args_positional_after.thrust",
    "functions/named_args_varargs.thrust",
}

EXPECTED_RUN_CODES = {
    "imports/module_a.thrust": 123,
    "imports/module_alias_multi.thrust": 130,
    "imports/only_multi.thrust": 15,
    "imports/only_single.thrust": 3,
    "imports/struct_only.thrust": 3,
    "imports/struct_qualified.thrust": 3,
    "functions/named_args.thrust": 139,
    "module_reexportation/reexport_a.thrust": 3,
    "modules/named_args_module.thrust": 3,
    "optimization/disable_default_optimization.thrust": 173,
    "webassembly_abi/variadic.thrust": 1,
}

SKIPPED_TEST_PATHS = {
    "load/load_index.thrust",
    "module_reexportation/std_reexport.thrust",
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
        "--filter",
        default="",
        help="Only run tests whose relative path contains this text.",
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

    skipped_parts = {
        "dist",
        "scripts",
        "build",
        "stdroot",
    }

    relative_parts = path.relative_to(tests_dir).parts
    relative_path = path.relative_to(tests_dir).as_posix()

    if relative_path in SKIPPED_TEST_PATHS:
        return True

    return any(part in skipped_parts for part in relative_parts)


def read_text(path: Path) -> str:

    return path.read_text(encoding="utf-8")


def has_main_function(path: Path) -> bool:

    return MAIN_FUNCTION_RE.search(read_text(path)) is not None


def discover_test_roots(tests_dir: Path, filter_text: str) -> list[Path]:

    test_roots: list[Path] = []

    for path in sorted(tests_dir.rglob("*.thrust")):

        if should_skip_path(path, tests_dir):
            continue

        relative = path.relative_to(tests_dir).as_posix()

        if filter_text and filter_text not in relative:
            continue

        if has_main_function(path):
            test_roots.append(path)

    return test_roots


def is_negative_test(path: Path) -> bool:

    name = path.name
    normalized_path = path.as_posix()

    if any(normalized_path.endswith(expected) for expected in NEGATIVE_TEST_PATHS):
        return True

    return any(marker in name for marker in NEGATIVE_NAME_MARKERS)


def expected_run_code(path: Path) -> int:

    normalized_path = path.as_posix()

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

        content = read_text(source_path)

        for import_match in IMPORT_PATH_RE.finditer(content):
            dependency = resolve_import_path(source_path, import_match.group(1))

            if dependency in visited:
                continue

            visited.add(dependency)

            dependencies.append(dependency)

            if dependency.exists() and dependency.is_file():
                visit(dependency)

    visit(root_file.resolve())

    return dependencies


def needs_math_linkage(files: list[Path]) -> bool:

    for path in files:

        if not path.exists():
            continue

        content = read_text(path)

        if STD_MATH_RE.search(content) is not None:
            return True

        if "%" in content and FLOAT_VALUE_RE.search(content) is not None:
            return True

    return False


def compiletime_std_args(test_path: Path, root: Path) -> list[str]:

    tests_dir = tests_root(root)
    relative = test_path.relative_to(tests_dir).as_posix()

    if not relative.startswith("compiletime_if_imports/if_std_import"):
        return ["-std", str(root / "std")]

    stdroot = tests_dir / "compiletime_if_imports" / "stdroot"

    return [
        "-std",
        str(stdroot),
        "-std-version",
        "0.1.8",
    ]


def test_identifier(test_path: Path, root: Path) -> str:

    relative = test_path.relative_to(tests_root(root)).with_suffix("")

    return "__".join(relative.parts)


def compiler_path(args: argparse.Namespace, root: Path) -> Path:

    if args.compiler is not None:
        return args.compiler.resolve()

    return root / "target" / "debug" / "thrustc"


def build_compiler(root: Path) -> int:

    print("Building thrustc...", flush=True)

    result = subprocess.run(
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

    identifier = test_identifier(test_path, root)
    build_dir = dist_root(root) / "build" / identifier
    binary_path = dist_root(root) / "bin" / identifier
    dependencies = discover_user_dependencies(test_path)
    files = [test_path.resolve(), *dependencies]
    auto_cc_args: list[str] = []

    auto_cc_args.extend(["-o", str(binary_path)])

    if needs_math_linkage(files):
        auto_cc_args.append("-lm")

    cc_args = merge_cc_args(auto_cc_args, args.cc_args)
    command: list[str] = [
        str(compiler),
        "-build-dir",
        str(build_dir),
    ]

    command.extend(compiletime_std_args(test_path, root))

    command.extend(str(path) for path in files)

    if cc_args:
        command.extend(["-cc-args", cc_args])

    result = subprocess.run(
        command,
        cwd=root,
        capture_output=True,
        text=True,
        timeout=args.compile_timeout,
    )

    return result, binary_path, dependencies


def run_binary(
    binary_path: Path,
    root: Path,
    identifier: str,
    args: argparse.Namespace,
) -> subprocess.CompletedProcess[str]:

    run_dir = dist_root(root) / "run" / identifier

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

    start = time.monotonic()
    negative = is_negative_test(test_path)

    try:
        compile_result, binary_path, _dependencies = compile_test(test_path, compiler, root, args)
    except subprocess.TimeoutExpired as error:

        elapsed = time.monotonic() - start

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

        passed = compile_result.returncode != 0 or not binary_path.exists()
        message = "compile failed as expected" if passed else "expected compile failure"

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

    identifier = test_identifier(test_path, root)

    try:
        run_result = run_binary(binary_path, root, identifier, args)
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
    expected_code = expected_run_code(test_path)
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


def prepare_dist(root: Path) -> None:

    dist = dist_root(root)

    shutil.rmtree(dist, ignore_errors=True)
    (dist / "bin").mkdir(parents=True, exist_ok=True)
    (dist / "build").mkdir(parents=True, exist_ok=True)
    (dist / "run").mkdir(parents=True, exist_ok=True)


def cleanup_dist(root: Path, keep_dist: bool) -> None:

    if keep_dist:
        return

    shutil.rmtree(dist_root(root), ignore_errors=True)


def print_test_result(result: TestResult, root: Path) -> None:

    relative = result.path.relative_to(tests_root(root)).as_posix()
    status = "PASS" if result.passed else "FAIL"
    run_code = "-" if result.run_code is None else str(result.run_code)

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

    relative = test_path.relative_to(tests_root(root)).as_posix()

    print(f"[RUNNING] {relative}", flush=True)


def print_failure_details(result: TestResult, root: Path) -> None:

    relative = result.path.relative_to(tests_root(root)).as_posix()

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

    total = len(results)
    passed = sum(1 for result in results if result.passed)
    failed = total - passed
    positives = sum(1 for result in results if result.kind == "positive")
    negatives = sum(1 for result in results if result.kind == "negative")

    print("\nTest report", flush=True)
    print(f"total: {total}", flush=True)
    print(f"passed: {passed}", flush=True)
    print(f"failed: {failed}", flush=True)
    print(f"positive: {positives}", flush=True)
    print(f"negative: {negatives}", flush=True)

    failures = [result for result in results if not result.passed]

    if failures:
        print("\nFailures", flush=True)

        for result in failures:
            print_failure_details(result, root)


def main() -> int:

    args = parse_args()
    root = project_root()
    tests_dir = tests_root(root)
    compiler = compiler_path(args, root)

    if not args.no_build and args.compiler is None:

        build_code = build_compiler(root)

        if build_code != 0:
            return build_code

    if not compiler.exists():
        print(f"compiler not found: {compiler}", file=sys.stderr)
        return 1

    test_roots = discover_test_roots(tests_dir, args.filter)

    if not test_roots:
        print("no tests found", file=sys.stderr)
        return 1

    prepare_dist(root)

    results: list[TestResult] = []

    try:

        for test_path in test_roots:

            print_running_test(test_path, root)

            result = run_test(test_path, compiler, root, args)

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
