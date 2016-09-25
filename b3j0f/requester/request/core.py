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

"""Request module."""

__all__ = ['Request']

from .expr import Expression
from .crud.base import CRUD
from .crud.create import Create
from .crud.read import Read
from .crud.update import Update
from .crud.delete import Delete


class Request(object):
    """CRUD/runnable object bound to a driver in order to access to data.

    Common use is to instanciate it from a RequestManager."""

    __slots__ = ['driver', 'ctx', '_query', 'cruds']

    def __init__(
            self, driver=None, ctx=None, query=None, cruds=None,
            *args, **kwargs
    ):
        """
        :param Driver driver: driver able to execute the request.
        :param dict ctx: request execution context.
        :param Expression query: request query.
        :param tuple ops: cruds.
        """

        super(Request, self).__init__(*args, **kwargs)

        self.driver = driver
        self.ctx = ctx or {}
        self._query = query
        self.cruds = cruds or []

        if query is not None and not isinstance(query, Expression):
            raise TypeError(
                'Wrong type {0}. {1} expected.'.format(query, Expression)
            )


        for crud in self.cruds:
            if not isinstance(crud, CRUD):
                raise TypeError(
                    'Wrong type {0}. {1} expected.'.format(crud, CRUD)
                )

    @property
    def query(self):
        """Get this query.

        :rtype: Expression"""

        return self._query

    @query.setter
    def query(self, value):
        """Update this query.

        Examples:

        - self.query = E.user.id == E.owner.id  # set 'equal' function
        - self.query &= E.user.id == E.owner.id  # apply 'and' on this query and
            new function.
        - self.query |= E.user.id == E.owner.id  # apply 'and' on this query and
            new function.

        :param Expression value: query to set.
        """

        if value is not None and not isinstance(value, Expression):
            raise TypeError(
                'Wrong type {0}. {1} expected.'.format(value, Expression)
            )

        self._query = value

    @query.deleter
    def query(self):
        """Delete query."""

        self._query = None

    def and_(self, query):
        """Apply input query to this query.

        :return: self
        :rtype: Request"""

        if self._query is None:
            self._query = query

        else:
            self._query &= query

        return self

    def or_(self, query):
        """Apply input query to this query.

        :return: self
        :rtype: Request"""

        if self._query is None:
            self._query = query

        else:
            self._query |= query

        return self

    def commit(self, explain=False):
        """Process this request and return self.

        :param bool explain: if True (default False), give additional
            informations about the request execution (indexes, etc.).
        :return: self
        :rtype: Request"""

        self.driver.process(self, explain=explain)

        return self

    def __getitem__(self, key):

        return self.read(key).ctx[key]

    def __setitem__(self, key, value):

        return self.update(name=key, **value)

    def __delitem__(self, key):

        return self.delete(key)

    def processcrud(self, *cruds):
        """Process several crud operations."""

        self.cruds += [cruds]

        self.driver.process(self)

        return self

    def create(self, name, **values):

        create = Create(name=name, params=[values])

        return self.processcrud(create)

    def read(self, name, **kwargs):
        """Read input expressions.

        :param Read read: read. All filtered data by default.
        :rtype: Cursor
        """

        read = Read(name=name, **kwargs)

        return self.processcrud(read)

    def update(self, name, **values):
        """Apply input updates.

        :param tuple updates: updates to apply.
        """

        update = Update(name=name, params=[values])

        return self.processcrud(update)

    def delete(self, *names):
        """Delete input deletes.

        :param tuple deletes: deletes to delete.
        :return: number of deleted deletes.
        """

        delete = Delete(name=names)

        return self.processcrud(delete)

    def run(self, name, *params):
        """Run input expr.

        :return: expr result.
        """

        function = CRUD(name=name, params=params)

        return self.processcrud(function)

    def groupby(self, *value):

        return Read(request=self, groupby=values)

    def sort(self, *values):

        return Read(request=self, sort=values)

    def select(self, *values):

        return Read(request=self, name=values)

    def skip(self, value):

        return Read(request=self, skip=value)

    def limit(self, value):

        return Read(request=self, limit=value)

    def join(self, value):

        return Read(request=self, join=value)
