from cutter import cut, flatten, _

list_of_dict = [
    {'a': 'a', 'b': 2, 0: 0},
    {'a': None, 'c': 4},
    {'z': 5, 'a': 0, 'b': 3, 5: 'foo'}
]

list_of_list = [
    list(range(4)),
    list(reversed(range(4))),
    list(range(12, 37, 3))
]

list_of_tuple = [
    tuple(range(4)),
    tuple(reversed(range(4))),
    tuple(range(12, 37, 3))
]


class attr_cls(object):
    def __init__(self, **kwargs):
        self.__dict__.update(kwargs)


list_of_obj = [
    attr_cls(at1=1, at2=23, at3=None, at4='foo'),
    attr_cls(at1=2, at2='bar', at4=attr_cls()),
    attr_cls(at1=[2., 4], at2=23, at3=None),
    attr_cls(at2={})
]


list_of_dict_of_dict = [
    {'A': {'b': 1, 'c': 2},
     'B': {'b': 12, 'd': 21}},
    {'C': {'a': 0, 'c': 2},
     'B': {'b': 'u', 'd': None, 'a': {'p': 2, 'T': 12}}},
    {'B': {'a': {'T': 4, 'a': 3}, 'b': 17},
     'A': {'p': 'a', 'e': '_', 'b': ''}},
    {'D': {'c': '0', 'z': 'w'},
     'C': {'c': 'u', 'p': 'u'}}
]

list_of_dict_of_list_of_dict = [
    {
        'A': [
            {'b': 1, 'c': 2},
            {'b': 12, 'd': 21}],
        'B': [
            {'a': 0, 'c': 2},
            {'b': 'u', 'd': None, 'a': {'p': 2, 'T': 12}}],
    }, {
        'B': [
            {'a': {'T': 4, 'a': 3}, 'b': 17},
            {'p': 'a', 'e': '_', 'b': ''}],
        'D': [
            {'c': '0', 'z': 'w'},
            {'c': 'u', 'p': 'u'}],
    }
]


list_of_list_of_list_of_list = [
    [
        [
            [1, 2, 3],
            [4, 5, 6],
            [7, 8, 9]
        ], [
            [10, 11, 12],
            [13, 14, 15],
            [16, 17, 18]
        ]
    ],
    [
        [
            [19, 20, 21],
            [22, 23, 24],
            [25, 26, 27]
        ],
        [
            [28, 29, 30],
            [31, 32, 33],
            [34, 35, 36]
        ]
    ]
]


class Cls(object):
    def __init__(self, attr):
        self.attr = attr

    def get_upper_attr(self):
        return self.attr.upper()


def test_cut_list_of_dict():
    assert cut(list_of_dict)['a'] == ['a', None, 0]
    assert cut(list_of_dict)['a'][2] == 0
    assert cut(list_of_dict)['a', 2] == []
    assert cut(list_of_dict).a == ['a', None, 0]
    assert cut(list_of_dict)['b'] == [2, 3]
    assert cut(list_of_dict).b == [2, 3]
    assert cut(list_of_dict)['j'] == []
    assert cut(list_of_dict).j == []
    assert cut(list_of_dict)[5] == ['foo']
    assert cut(list_of_dict)[2] == []
    assert cut(list_of_dict)[0] == [0]


def test_cut_cut():
    assert cut(list_of_dict) == list_of_dict
    assert cut(list_of_dict) == cut(cut(list_of_dict))
    assert cut(list_of_dict)['a'] == cut(cut(list_of_dict))['a']
    assert repr(cut(list_of_dict)) == repr(list_of_dict) + '*'
    assert repr(cut(list_of_dict)['a']) == "['a', None, 0]" + '.'


def test_cut_list_of_list():
    assert cut(list_of_list)[3] == [3, 0, 21]
    assert cut(list_of_list)[1] == [1, 2, 15]
    assert cut(list_of_list)[5] == [27]
    assert cut(list_of_list)[50] == []
    assert cut(list_of_list)[0] == [0, 3, 12]
    assert cut(list_of_list)[:2] == [[0, 1], [3, 2], [12, 15]]


def test_cut_list_of_tuple():
    assert cut(list_of_tuple)[3] == [3, 0, 21]
    assert cut(list_of_tuple)[1] == [1, 2, 15]
    assert cut(list_of_tuple)[5] == [27]
    assert cut(list_of_tuple)[50] == []
    assert cut(list_of_tuple)[0] == [0, 3, 12]
    assert cut(list_of_tuple)[:2] == [(0, 1), (3, 2), (12, 15)]


def test_cut_list_of_obj():
    assert cut(list_of_obj)['at1'] == [1, 2, [2., 4]]
    assert cut(list_of_obj).at1 == [1, 2, [2., 4]]
    assert cut(list_of_obj)['at2'] == [23, 'bar', 23, {}]
    assert cut(list_of_obj)['at3'] == [None, None]
    assert cut(list_of_obj)['at5'] == []


def test_flatten():
    assert flatten([1, 2, 3]) == [1, 2, 3]
    assert flatten([[1], [2, 3]]) == [1, 2, 3]
    assert flatten([[[1, 2], [3]], [4, [5]]]) == [1, 2, 3, 4, 5]
    assert flatten([1]) == [1]
    assert flatten(['ab']) == ['ab']


def test_complex_cuts_list_of_dicts_of_dicts():
    assert cut(list_of_dict_of_dict)['A', 'b'] == [1, '']
    assert cut(list_of_dict_of_dict)['B', 'b'] == [12, 'u', 17]
    assert cut(list_of_dict_of_dict)['B', 'a', 'T'] == [12, 4]


def test_complex_cuts_list_of_dicts_of_list_of_dicts():
    assert cut(list_of_dict_of_list_of_dict)['A', ..., 'b'] == [1, 12]
    assert cut(list_of_dict_of_list_of_dict)['B', ..., 'b'] == ['u', 17, '']
    assert cut(list_of_dict_of_list_of_dict)['B', ..., 'a', 'T'] == [12, 4]
    assert cut(list_of_dict_of_list_of_dict)['B', 1:, ..., 'a', 'T'] == [12]
    assert cut(list_of_dict_of_list_of_dict)['B', :1, ..., 'a', 'T'] == [4]


def test_complex_cuts_list_of_list_of_list_of_list():
    assert cut(list_of_list_of_list_of_list)[1] == [
        [[10, 11, 12], [13, 14, 15], [16, 17, 18]],
        [[28, 29, 30], [31, 32, 33], [34, 35, 36]]]

    assert cut(list_of_list_of_list_of_list)[1, 1] == [
        [13, 14, 15], [31, 32, 33]
    ]
    assert cut(list_of_list_of_list_of_list)[1, 1] == (
        cut(list_of_list_of_list_of_list)[1][1])

    assert cut(list_of_list_of_list_of_list)[1, 1, 1] == [
        14, 32
    ]
    assert cut(list_of_list_of_list_of_list)[1, 1, 1] == (
        cut(list_of_list_of_list_of_list)[1][1][1])

    assert cut(list_of_list_of_list_of_list)[1, 1, 1, 1] == []

    assert cut(list_of_list_of_list_of_list)[...] == list(range(1, 37))

def test_cls():
    cls = [Cls('a'), Cls('r'), Cls('s')]
    assert cut(cls).attr == list('ars')


def test_chain():
    cls = [Cls([Cls('a'), Cls('h')]), Cls([Cls('s'), Cls('u')])]
    assert cut(flatten(cut(cls).attr)).attr == list('ahsu')
    assert cut(cls).attr._.attr == list('ahsu')

    assert cut(cut(cls).attr) == cut(cls).attr
    assert cut(cut(cls).attr)._.attr == cut(cls).attr._.attr
    assert cut(cut(cls).attr._) == cut(cls).attr._
    assert cut(cut(cls).attr._)._ellipsis_at_next == cut(
        cls).attr._._ellipsis_at_next
    assert cut(cut(cls).attr._).attr == cut(cls).attr._.attr
    assert cut(cut(cut(cls).attr)._).attr == cut(cls).attr._.attr


def test_call():
    cls = [Cls('a'), Cls('r'), Cls('s')]
    assert cut(cls).get_upper_attr() == list('ARS')
    assert cut('cu7').isalpha() == [True, True, False]


def test_underscore():
    assert list_of_dict | _['a'] == ['a', None, 0]
    assert (list_of_dict | _['a'])[2] == 0
    assert list_of_dict | _['a', 2] == []
    assert list_of_dict | _.a == ['a', None, 0]
    assert list_of_dict | _['b'] == [2, 3]
    assert list_of_dict | _.b == [2, 3]
    assert list_of_dict | _['j'] == []
    assert list_of_dict | _.j == []
    assert list_of_dict | _[5] == ['foo']
    assert list_of_dict | _[2] == []
    assert list_of_dict | _[0] == [0]
    assert ('cu7' | _['isalpha'])() == [True, True, False]
