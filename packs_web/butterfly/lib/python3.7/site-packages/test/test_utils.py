from cutter import cut
from cutter.utils import bang_compile
from .test__init__ import (
    list_of_dict, list_of_list, list_of_tuple, list_of_obj, Cls,
    list_of_list_of_list_of_list)
import sys


cls = [Cls([Cls('a'), Cls('h')]), Cls([Cls('s'), Cls('u')])]


def test_bang_compile_exec():
    scope = {'cut': cut}
    source = '''a = ['abc', 'def', 'ghi']
b = []
print('Test tokenizer !!')
for i in range(3):
    b.append(str(a!2))
b.append('End')
'''
    code = bang_compile(source, '<test>', 'exec')

    if sys.version_info[0] > 2:
        exec(code, scope, scope)
    else:
        exec('exec code in scope, scope')
    assert 'a' in scope
    assert 'b' in scope
    assert scope['b'] == [
        "['c', 'f', 'i'].",
        "['c', 'f', 'i'].",
        "['c', 'f', 'i'].",
        'End']


def run(code):
    return eval(bang_compile(code, '<test>', 'eval'), globals())


def test_bang_compile_dict():
    assert run('list_of_dict!a') == ['a', None, 0]
    assert run('list_of_dict!a[2]') == 0
    assert run('list_of_dict!a!2') == []
    assert run('list_of_dict!b') == [2, 3]


def test_bang_compile_list():
    assert run('list_of_list!3') == [3, 0, 21]


def test_bang_compile_tuple():
    assert run('list_of_tuple!3') == [3, 0, 21]


def test_bang_compile_attr_chain():
    assert run('cls!attr._.attr') == list('ahsu')
    assert run('cls!attr!_!attr') == list('ahsu')
    assert run('cls!attr!_.attr') == list('ahsu')
    assert run('cls!attr!!attr') == list('ahsu')


def test_bang_compile_ellipsis():
    assert run('list_of_list_of_list_of_list!*') == list(range(1, 37))
