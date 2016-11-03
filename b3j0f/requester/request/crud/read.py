# -*- coding: utf-8 -*-

# --------------------------------------------------------------------
# The MIT License (MIT)
#
# Copyright (c) 2016 Jonathan Labéjof <jonathan.labejof@gmail.com>
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

from collections import Iterable

from six import string_types

from .base import CRUDElement
from .join import Join


__all__ = ['Read', 'Cursor']


class Read(CRUDElement):
    """In charge of parameterize a reading request.

    Execution is done in calling it or in using the getslice method.
    Result is a Cursor."""

    __slots__ = [
        '_distinct', '_select', '_offset', '_limit', '_orderby', '_groupby',
        '_scope', '_join'
    ] + CRUDElement.__slots__

    def __init__(
            self,
            distinct=False, select=None, offset=None, limit=None, orderby=None,
            groupby=None, join=None, scope=None, *args, **kwargs
    ):
        """
        :param list select: data to select.
        :param int offset: data to avoid.
        :param int limit: max number of data to retrieve.
        :param list orderby: data sorting.
        :param Expression groupby: data field group.
        :param Join join: join.
        :param list scope: scope.
        """

        super(Read, self).__init__(*args, **kwargs)

        # initialize protected properties
        self._distinct = False
        self._select = ()
        self._offset = None
        self._limit = None
        self._orderby = ()
        self._groupby = None
        self._join = None
        self._scope = None

        # set parameters
        self.distinct(distinct)

        if select is not None:
            self.select(*select)

        if offset is not None:
            self.offset(offset)

        if limit is not None:
            self.limit(limit)

        if orderby is not None:
            self.orderby(*orderby)

        if groupby is not None:
            self.groupby(groupby)

        if join is not None:
            self.join(join)

        if scope is not None:
            self.scope(scope)

    def distinct(self, *value):
        """Get or set distinct if value is not empty.

        :param bool value: value to set.
        :return: depending on value. If empty, return this distinct, otherwise
            this.
        :rtype: int or Read
        """
        if value:
            value = value[0]

            if not isinstance(value, bool):
                raise TypeError(
                    'Wrong value {0}. {1} expected'.format(value, bool)
                )

            self._distinct = value
            result = self

        else:
            result = self._distinct

        return result

    def offset(self, *value):
        """Get or set offset if value is not empty.

        :param int value: value to set.
        :return: depending on value. If empty, return this offset, otherwise
            this.
        :rtype: int or Read
        """
        if value:
            value = value[0]

            if not isinstance(value, int):
                raise TypeError(
                    'Wrong value {0}. {1} expected'.format(value, int)
                )

            self._offset = value
            result = self

        else:
            result = self._offset

        return result

    def limit(self, *value):
        """Get or set limit if value is not empty.

        :param int value: value to set.
        :return: depending on value. If empty, return this limit, otherwise
            this.
        :rtype: int or Read
        """
        if value:
            value = value[0]

            if not isinstance(value, int):
                raise TypeError(
                    'Wrong value {0}. {1} expected'.format(value, int)
                )

            result = self
            self._limit = value

        else:
            result = self._limit

        return result

    def orderby(self, *values):
        """Get or set orderby if value is not empty.

        :param tuple values: value to set.
        :return: depending on value. If empty, return this orderby, otherwise
            this.
        :rtype: tuple or Read
        """
        if values:
            self._orderby = values
            result = self

        else:
            result = self._orderby

        return result

    def groupby(self, *value):
        """Get or set groupby if value is not empty.

        :param int value: value to set.
        :return: depending on value. If empty, return this groupby, otherwise
            this.
        :rtype: int or Read
        """
        if value:
            value = value[0]

            if not isinstance(value, string_types):
                raise TypeError(
                    'Wrong value {0}. {1} expected'.format(value, str)
                )

            result = self
            self._groupby = value

        else:
            result = self._groupby

        return result

    def select(self, *values):
        """Get or set select if value is not empty.

        :param tuple value: value to set.
        :return: depending on value. If empty, return this select, otherwise
            this.
        :rtype: tuple or Read
        """
        if values:
            self._select = values
            result = self

        else:
            result = self._select

        return result

    def scope(self, *values):
        """Get or set scope if value is not empty.

        :param tuple value: value to set.
        :return: depending on value. If empty, return this scope, otherwise
            this.
        :rtype: tuple or Read
        """
        if values:
            self._scope = values
            result = self

        else:
            result = self._scope

        return result

    def join(self, *value):
        """Get or set join if value is not empty.

        :param Join value: value to set.
        :return: depending on value. If empty, return this join, otherwise
            this.
        :rtype: Join or Read
        """
        if value:
            value = value[0]

            if not isinstance(value, Join):
                raise TypeError(
                    'Wrong value {0}. {1} expected'.format(
                        value, Join
                    )
                )

            self._join = value
            result = self

        else:
            result = self._join

        return result

    def __getslice__(self, start, stop):
        """Set offset and limit and execute the selection.

        :param int start: offset property.
        :param int stop: limit property.
        :return: selection execution result.
        :rtype: Cursor"""

        if start is not None:
            self._offset = start

        if stop is not None:
            self._limit = stop

        return self()

    def __getitem__(self, key):

        if not isinstance(key, slice):
            key = slice(key, key + 1)

        return self.__getslice__(key.start, key.stop)

    def __repr__(self):

        if self._select:
            items = [repr(item) for item in self._select]
            select = ', '.join(items)

        else:
            select = 'ALL'

        result = 'READ {0} '.format(select)

        if (
            self._limit or self._offset or self._groupby or self._orderby or
            self._join
        ):
            if self._limit is not None:
                result += 'LIMIT {0} '.format(repr(self._limit))

            if self._offset is not None:
                result += 'OFFSET {0} '.format(repr(self._offset))

            if self._groupby is not None:
                result += 'GROUP BY {0} '.format(repr(self._groupby))

            if self._orderby:
                items = [repr(item) for item in self._orderby]
                result += 'ORDER BY {0} '.format(', '.join(items))

            if self._join is not None:
                result += 'JOIN {0} '.format(repr(self._join))

        if self.query:
            result += 'WHERE {0} '.format(repr(self.query))

        if self.dparams:
            result += 'WITH {0} '.format(repr(self.dparams))

        if self.alias:
            result += 'AS {0}'.format(self.alias)

        if result[-1] == ' ':
            result = result[:-1]

        return result


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
