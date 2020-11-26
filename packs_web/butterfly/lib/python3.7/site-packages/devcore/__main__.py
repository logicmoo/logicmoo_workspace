import os
from argparse import ArgumentParser

import semver


def bump(project_name, level, dry, about_file_path):
    about_fn = os.path.abspath(
        about_file_path or os.path.join(project_name, "__about__.py")
    )
    with open(about_fn) as f:
        about_file = f.read()
    about = {}
    exec(about_file, about)
    old_version = about['__version__']
    new_version = getattr(semver, f'bump_{level}')(old_version)

    if not dry:
        about_file = about_file.replace(
            f'__version__ = "{ old_version }"',
            f'__version__ = "{ new_version }"'
        )
        with open(about_fn, 'w') as f:
            f.write(about_file)

    print(new_version)


parser = ArgumentParser(prog='devcore')
parser.add_argument('--project_name', default=os.getenv('PROJECT_NAME'))
subparsers = parser.add_subparsers(
    title='Commands', description="Command to run", help='Available commands'
)

parser_bump = subparsers.add_parser('bump', help="Bump project version")
parser_bump.add_argument(
    'level', choices=['major', 'minor', 'patch'], nargs='?', default='patch'
)
parser_bump.add_argument(
    '--dry',
    action='store_true',
    help="Return version without editing about file."
)
parser_bump.add_argument(
    '--about-file-path',
    help=f'Path to __about__.py if different from <project_name>/__about__.py'
)
parser_bump.set_defaults(func=bump)


def main():
    args = parser.parse_args()
    args.func(**{k: v for k, v in vars(args).items() if k != 'func'})
