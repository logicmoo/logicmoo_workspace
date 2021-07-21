import sys
from token import ERRORTOKEN, OP, NAME
from tokenize import generate_tokens, untokenize
import ast
from cutter import cut

if sys.version_info[0] == 2:
    from StringIO import StringIO
else:
    from io import StringIO


def tokenize_bang(source):
    """Browse the source and replace any ! token by ._ast_replace_me_with_cut()
    """

    stream = StringIO()
    stream.write(source)
    stream.seek(0)
    tokenized = []
    last_token = ''
    for token_type, token, srowcol, _, _ in generate_tokens(stream.readline):
        if token_type == ERRORTOKEN and token == '!' and srowcol != (1, 0):
            tokenized.append((OP, '.'))
            if last_token == '\x00':
                tokenized.append((NAME, '_'))
            else:
                tokenized.append((NAME, '_ast_replace_me_with_cut'))
                tokenized.append((OP, '('))
                tokenized.append((OP, ')'))
                last_token = '\x00'
        else:
            if last_token == '\x00':
                if token.isdigit():
                    tokenized.append((OP, '['))
                    tokenized.append((token_type, token))
                    tokenized.append((OP, ']'))
                elif token == '*':
                    tokenized.append((OP, '[...]'))
                elif token != '[':
                    tokenized.append((OP, '.'))
                    tokenized.append((token_type, token))
                else:
                    tokenized.append((token_type, token))
            else:
                tokenized.append((token_type, token))

            last_token = token

    return untokenize(tokenized)


class CutWrapper(ast.NodeTransformer):
    """Replace any thg.sthg._ast_replace_me_with_cut() by cut(thg.sthg)"""

    def visit_Call(self, node):
        self.generic_visit(node)
        if hasattr(node.func, 'attr') and (
                node.func.attr == '_ast_replace_me_with_cut'):
            new_node = ast.Call(
                func=ast.Name(id='cut', ctx=ast.Load()),
                args=[node.func.value],
                keywords=[], starargs=None, kwargs=None)
            ast.copy_location(new_node, node)
            ast.fix_missing_locations(new_node)
            return new_node
        return node


def cut_as_builtin():
    __builtins__['cut'] = cut


def bang_compile(
        source, filename, mode, *args, **kwargs):
    try:
        source_tokenized = tokenize_bang(source)
        ast_ = ast.parse(source_tokenized, mode=mode)
        source = CutWrapper().visit(ast_)
    except Exception:
        pass
    rv = compile(source, filename, mode, *args, **kwargs)
    return rv
