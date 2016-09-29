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

from .base import BaseElement
from .expr import Expression
from .crude.base import CRUDEElement
from .crude.create import Create
from .crude.read import Read
from .crude.update import Update
from .crude.delete import Delete
from .crude.exe import Exe

from six import iteritems


class Context(dict):
    """Request execution context."""

    def __getitem__(self, key):

        if isinstance(key, BaseElement):
            key = key.ctxname

        return super(Context, self).__getitem__(key)

    def __setitem__(self, key, value):

        if isinstance(key, BaseElement):
            key = key.ctxname

        return super(Context, self).__setitem__(key, value)

    def __delitem__(self, key):

        if isinstance(key, BaseElement):
            key = key.ctxname

        return super(Context, self).__delitem__(key)

    def __contains__(self, key):

        if isinstance(key, BaseElement):
            key = key.ctxname

        return super(Context, self).__contains__(key)

    def fill(self, ctx):
        """Fill this content with ctx data not in this data.

        :param Context ctx: ctx context from where get items."""

        if isinstance(ctx, Context):
            for key, value in iteritems(ctx):

                if key in self:
                    self[key] += [
                        item for item in value if item not in self[key]
                    ]

                else:
                    self[key] = ctx[key]

                dotkey = '.{0}'.format(key)

                for key in list(self):
                    if key.endswith(dotkey):
                        self[key] += [
                            item for item in value if item not in self[key]
                        ]


        return self


class Request(object):
    """CRUDEElement/exenable object bound to a driver in order to access to data.

    Common use is to instanciate it from a RequestManager."""

    __slots__ = ['driver', 'ctx', '_query', 'crudes']

    def __init__(
            self, driver=None, ctx=None, query=None, crudes=None,
            *args, **kwargs
    ):
        """
        :param Driver driver: driver able to execute the request.
        :param Context ctx: request execution context.
        :param Expression query: request query.
        :param tuple ops: crudes.
        """

        super(Request, self).__init__(*args, **kwargs)

        self.driver = driver
        self.ctx = ctx or Context()
        self._query = query
        self.crudes = crudes or []

        if query is not None and not isinstance(query, Expression):
            raise TypeError(
                'Wrong type {0}. {1} expected.'.format(query, Expression)
            )


        for crude in self.crudes:
            if not isinstance(crude, CRUDEElement):
                raise TypeError(
                    'Wrong type {0}. {1} expected.'.format(crude, CRUDEElement)
                )

    def __repr__(self):

        result = 'Request('

        if self.driver:
            result += 'driver: {0},'.format(self.driver)

        if self.ctx:
            result += 'ctx: {0},'.format(self.ctx)

        if self._query:
            result += 'query: {0},'.format(self._query)

        if self.crudes:
            result += 'crudes: {0}'.format(self.crudes)

        result += ')'

        return result

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

    def processcrude(self, *crudes):
        """Process several crude operations."""

        self.crudes += list(crudes)

        self.driver.process(self)

        return self

    def create(self, name, **values):

        return Create(request=self, name=name, values=values)()

    def read(self, select, **kwargs):
        """Read input expressions.

        :param tuple select: selection fields.
        :param dict kwargs: additional selection parameters (limit, etc.).
        :rtype: Cursor
        """

        return Read(request=self, select=select, **kwargs)()

    def update(self, name, **values):
        """Apply input updates.

        :param tuple updates: updates to apply.
        """

        return Update(request=self, name=name, values=values)()

    def delete(self, *names):
        """Delete input deletes.

        :param tuple names: model name to delete.
        :return: number of deleted deletes.
        """

        return Delete(request=self, names=names)()

    def exe(self, name, *params):
        """Execute input operation with params.

        :param name: operation name.
        :type name: str or Expression
        :param tuple params: operation parameters.
        :return: operation result.
        """

        return Exe(request=self, name=name, params=params)()

    def select(self, *values):
        """Start a read operation in defining values to select.

        See the Read object for more details about how to use it.
        :return: a read object.
        :rtype: Read"""

        return Read(request=self, select=values)

    def offset(self, value):
        """Start a read operation in defining the offset.

        See the Read object for more details about how to use it.
        :return: a read object.
        :rtype: Read"""

        return Read(request=self, offset=value)

    def limit(self, value):
        """Start a read operation in defining the limit.

        See the Read object for more details about how to use it.
        :return: a read object.
        :rtype: Read"""

        return Read(request=self, limit=value)

    def groupby(self, *values):
        """Start a read operation in defining values to groupby.

        See the Read object for more details about how to use it.
        :return: a read object.
        :rtype: Read"""

        return Read(request=self, groupby=values)

    def orderby(self, *values):
        """Start a read operation in defining values to orderby.

        See the Read object for more details about how to use it.
        :return: a read object.
        :rtype: Read"""

        return Read(request=self, orderby=values)

    def join(self, value):
        """Start a read operation in defining the join.

        See the Read object for more details about how to use it.
        :return: a read object.
        :rtype: Read"""

        return Read(request=self, join=value)
