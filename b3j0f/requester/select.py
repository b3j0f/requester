# -*- coding: utf-8 -*-

# --------------------------------------------------------------------
# The MIT License (MIT)
#
# Copyright (c) 2016 Jonathan Labéjof <jonathan.labejof@gmail.com>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
# --------------------------------------------------------------------

"""Selection module."""

__all__ = ['Selection', 'Cursor', 'Jointure']

from enum import IntEnum


class Jointure(IntEnum):

    INNER = 1  #: inner jointure.
    LEFT = 2  #: left jointure.
    LEFT_EX = 3  #: left exclusive jointure.
    RIGHT = 4  #: right exclusive jointure.
    RIGHT_EX = 5  #: right exclusive jointure.
    FULL = 6  #: full jointure.
    FULL_EX = 7  #: full exclusive jointure.
    CROSS = 8
    SELF = 9
    NATURAL = 10
    UNION = 11


class Selection(object):
    """In charge of parameterize a reading request.

    Execution is done in calling it or in using the getslice method.
    Result is a Cursor."""

    def __init__(
            self, request,
            skip=None, limit=None, sort=None, groupby=None,
            select=None, join=None,
            *args, **kwargs
    ):
        """
        :param Request request: source request.
        :param int skip: data to avoid.
        :param int limit: max number of data to retrieve.
        :param list sort: data sorting.
        :param list groupby: data field group.
        :param tuple select: data to select.
        :param Jointure join: jointure type (INNER, LEFT, etc.).
        """

        super(Selection, self).__init__(*args, **kwargs)

        self.request = request
        self._skip = skip
        self._limit = limit
        self._sort = sort
        self._groupby = groupby
        self._select = select
        self._join = join

    def skip(self, value):

        self._skip = value

        return self

    def limit(self, value):

        self._limit = value

        return self

    def sort(self, value):

        self._sort = value

        return self

    def groupby(self, value):

        self._groupby = value

        return self

    def select(self, value):

        self._select = value

        return self

    def join(self, value):

        self._join = value

        return self

    def __getslice__(self, i, j):
        """Set skip and limit and execute the selection.

        :param int i: skip property.
        :param int j: limit property.
        :return: selection execution result.
        :rtype: Cursor"""

        self._skip, self._limit = i, j

        return self()

    def __call__(self):
        """Execute this selection.

        :return: this selection result.
        :rtype: Cursor"""

        return self.request.read(selection=self)


class Cursor(Iterable):
    """Selection request result."""

    def __init__(self, cursor, *args, **kwargs):

        super(Cursor, self).__init__(*args, **kwargs)

        self._cursor = cursor

    def __len__(self):

        return len(self._cursor)

    def __iter__(self):

        return iter(self._cursor)

    def __getitem__(self, key):

        return self._cursor[key]

    def __getslice__(self, i, j):

        return self._cursor[i:j]
