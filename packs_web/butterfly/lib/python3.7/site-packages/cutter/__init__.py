# -*- coding: utf-8 -*-
# This file is part of cutter
#
# Python list cutter tool
# Copyright © 2013 Florian Mounier
#
# This library is free software: you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option) any
# later version.
#
# This library is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
# details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with pygal. If not, see <http://www.gnu.org/licenses/>.
"""
cutter - Python list cutter tool

"""

__version__ = '0.5.0'


class EllipsisGetter(object):
    def __getitem__(self, key):
        return key

ellipsis = EllipsisGetter()[...]


is_string = lambda s: (
    isinstance(s, bytes) or
    isinstance(s, str))

is_iterable = lambda i: (
    hasattr(i, '__iter__') and not
    isinstance(i, dict) and not
    is_string(i))


def flatten(iterable):
    return [
        item
        for subiterable in iterable
        for item in flatten(subiterable)
    ] if is_iterable(iterable) else [iterable]


class attr_cut(list):

    def __init__(self, iterable):
        self._ellipsis_at_next = getattr(iterable, '_ellipsis_at_next', False)
        super(attr_cut, self).__init__(iterable)

    @staticmethod
    def _cut(iterable, *args):
        """Cut a list by index or arg"""
        void = object()
        if not args:
            args = 0,

        if len(args) > 1:
            iterable = cut._cut(iterable, *args[:-1])
        index = args[-1]
        if index == ellipsis:
            return flatten(iterable)

        def cut_item(item):
            if isinstance(item, dict):
                return item.get(index, getattr(item, str(index), void))

            if hasattr(item, '__getitem__') and isinstance(
                    index, (int, slice)):
                return (
                    item.__class__.__getitem__(item, index)
                    if isinstance(index, slice) or len(item) > index
                    else void)
            return getattr(item, str(index), void)

        if not any(is_iterable(item) for item in iterable):
            # Can't use cut here because if nothing is iterable in the iterable
            # we want a real slicing list.
            # For cut chains use [1, 2] instead of [1][2]
            cut_cls = attr_cut
        else:
            cut_cls = cut
        return cut_cls([
            x for x in map(cut_item, iterable) if x is not void])

    def __getattr__(self, key):
        if key == '_ellipsis_at_next':
            return object.__getattribute__(self, '_ellipsis_at_next')

        if key == '_':
            self._ellipsis_at_next = True
            return self

        if self._ellipsis_at_next:
            key = ellipsis, key
        else:
            key = key,

        return self._cut(self, *key)

    def __call__(self, *args, **kwargs):
        return [e(*args, **kwargs) for e in self]

    def __repr__(self):
        return "%s." % list.__repr__(self)


class cut(attr_cut):
    def __getitem__(self, key):
        if not is_iterable(key):
            key = key,
        return self._cut(self, *key)

    def __getslice__(self, min, max):
        return self._cut(self, slice(min, max))

    def __repr__(self):
        return "%s*" % list.__repr__(self)


class ReverseCut(object):
    def __init__(self, key):
        self.key = key

    def __ror__(self, it):
        return cut(it)[self.key]


class SimpleGetItem(object):
    def __getitem__(self, key):
        return ReverseCut(key)

    def __getattr__(self, key):
        return ReverseCut(key)


_ = SimpleGetItem()
