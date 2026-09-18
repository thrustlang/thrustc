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
import shutil
import sys
from pathlib import Path


def parse_args() -> argparse.Namespace:

    parser = argparse.ArgumentParser(
        description="Create a Thrust test file."
    )

    parser.add_argument(
        "-path",
        required=True,
        type=Path,
        help="Test file path. Relative paths are created under tests/.",
    )

    parser.add_argument(
        "-content",
        required=True,
        help="Literal content or a path to a file to copy from.",
    )

    parser.add_argument(
        "--force",
        action="store_true",
        help="Overwrite the destination if it already exists.",
    )

    parser.add_argument(
        "--append-newline",
        action="store_true",
        help="Append a final newline when writing literal content.",
    )

    return parser.parse_args()


def project_root() -> Path:

    return Path(__file__).resolve().parents[2]


def tests_root(root: Path) -> Path:

    return root / "tests"


def resolve_destination(path: Path, root: Path) -> Path:

    if path.is_absolute():
        return path

    parts = path.parts

    if parts and parts[0] == "tests":
        return root / path

    return tests_root(root) / path


def validate_destination(path: Path, root: Path) -> None:

    tests_dir = tests_root(root).resolve()
    resolved = path.resolve()

    try:
        resolved.relative_to(tests_dir)
    except ValueError:
        raise ValueError(f"destination must be inside tests/: {path}") from None

    if path.suffix != ".thrust":
        raise ValueError("destination must use the .thrust extension")


def content_source(value: str, root: Path) -> Path | None:

    direct_path = Path(value)

    if direct_path.exists() and direct_path.is_file():
        return direct_path.resolve()

    relative_path = root / direct_path

    if relative_path.exists() and relative_path.is_file():
        return relative_path.resolve()

    return None


def write_literal(path: Path, content: str, append_newline: bool) -> None:

    if append_newline and not content.endswith("\n"):
        content = f"{content}\n"

    path.write_text(content, encoding="utf-8")


def create_test_file(args: argparse.Namespace) -> Path:

    root = project_root()
    destination = resolve_destination(args.path, root)

    validate_destination(destination, root)

    if destination.exists() and not args.force:
        raise FileExistsError(
            f"destination already exists: {destination}. Use --force to overwrite."
        )

    destination.parent.mkdir(parents=True, exist_ok=True)

    source = content_source(args.content, root)

    if source is not None:
        shutil.copyfile(source, destination)
    else:
        write_literal(destination, args.content, args.append_newline)

    return destination


def main() -> int:

    args = parse_args()

    try:
        destination = create_test_file(args)
    except (FileExistsError, OSError, ValueError) as error:
        print(error, file=sys.stderr)
        return 1

    print(f"created: {destination}")

    return 0


if __name__ == "__main__":

    raise SystemExit(main())
