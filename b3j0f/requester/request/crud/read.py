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

"""Read module."""

__all__ = ['Read', 'Cursor', 'Jointure']

from .base import CRUD

from enum import IntEnum, unique

from collections import Iterable


@unique
class Jointure(IntEnum):

    INNER = 0  #: inner jointure.
    LEFT = 1  #: left jointure.
    LEFT_EX = 2  #: left exclusive jointure.
    RIGHT = 3  #: right exclusive jointure.
    RIGHT_EX = 4  #: right exclusive jointure.
    FULL = 5  #: full jointure.
    FULL_EX = 6  #: full exclusive jointure.
    CROSS = 7  #: cross jointure.
    SELF = 8  #: self jointure.
    NATURAL = 9  #: natural jointure.
    UNION = 10  #: union jointure.


class Read(CRUD):
    """In charge of parameterize a reading request.

    Execution is done in calling it or in using the getslice method.
    Result is a Cursor."""

    __slots__ = [
        '_select', '_offset', '_limit', '_orderby', '_groupby', '_jointure'
    ] + CRUD.__slots__

    def __init__(
            self,
            select=None, offset=None, limit=None, orderby=None, groupby=None,
            jointure=None, *args, **kwargs
    ):
        """
        :param tuple select: data to select.
        :param int offset: data to avoid.
        :param int limit: max number of data to retrieve.
        :param list orderby: data sorting.
        :param list groupby: data field group.
        :param Jointure jointure: jointure type (INNER, LEFT, etc.).
        """

        super(Read, self).__init__(*args, **kwargs)

        self._offset = offset
        self._limit = limit
        self._orderby = orderby
        self._groupby = groupby
        self._select = select
        self._jointure = jointure.name if isinstance(jointure, Jointure) else jointure

    def offset(self, value):

        self._offset = value

        return self

    @property
    def getoffset(self):

        return self._offset

    def limit(self, value):

        self._limit = value

        return self

    @property
    def getlimit(self):

        return self._limit

    def orderby(self, value):

        self._orderby = value

        return self

    @property
    def getorderby(self):

        return self._orderby

    def groupby(self, value):

        self._groupby = value

        return self

    @property
    def getgroupby(self):

        return self._groupby

    def select(self, value):

        self._select = value

        return self

    @property
    def getselect(self):

        return self._select

    def jointure(self, value):

        self._jointure = value

        return self

    @property
    def getjointure(self):

        return self._jointure

    def __getslice__(self, i, j):
        """Set offset and limit and execute the selection.

        :param int i: offset property.
        :param int j: limit property.
        :return: selection execution result.
        :rtype: Cursor"""

        if i is not None:
            self._offset = i

        if j is not None:
            self._limit = j

        return self()

    def __call__(self):
        """Execute this selection.

        :return: this selection result.
        :rtype: Cursor"""

        if self.request is None:
            raise RuntimeError(
                'Impossible to execute this without associate it to a request.'
            )

        else:
            return self.request.process(self)


class Cursor(Iterable):
    """Read request result."""

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
