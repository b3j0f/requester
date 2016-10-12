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

"""Base crud module."""

from enum import IntEnum, unique

from ..expr import BaseElement

__all__ = ['CRUDElement', 'CRUD']


@unique
class CRUD(IntEnum):

    CREATE = 1
    READ = 2
    UPDATE = 3
    DELETE = 4


class CRUDElement(BaseElement):
    """Base crud operation.

    Can be associated to a transaction."""

    __slots__ = ['transaction', 'query', 'dparams'] + BaseElement.__slots__

    def __init__(
            self, query=None, transaction=None, dparams=None, *args, **kwargs
    ):
        """
        :param Expression query: query.
        :param Transaction transaction: related transaction.
        :param dict dparams: driver params.
        """

        super(CRUDElement, self).__init__(*args, **kwargs)

        self.query = query
        self.transaction = transaction
        self.dparams = dparams

    def __call__(self, **kwargs):
        """Execute this CRUD element.

        :param dict kwargs: driver specific kwargs. See Driver.process for more
            details.
        :return: this execution result if async is False.
        """
        if self.transaction is None:
            raise RuntimeError('No transaction attached.')

        else:
            self.transaction.process(cruds=[self], **kwargs)

            return self.transaction.ctx.get(self)

    def where(self, *query):
        """Getter/Setter for query.

        Setter if query is given. Getter otherwise.

        :return: self if query is given, self query otherwise.
        :rtype: Request or BaseElement"""

        if query:
            query = query[0]

            if not isinstance(query, BaseElement):
                raise TypeError('{0}. {1} expected'.format(query, BaseElement))

            if self.query is None:
                self.query = query

            else:
                self.query &= query

            result = self

        else:
            result = self.query

        return result

    def orwhere(self, query):
        """Apply input query to this query.

        :return: self
        :rtype: Request
        """
        if self.query is None:
            self.query = query

        else:
            self.query |= query

        return self

    def __iand__(self, other):

        return self.where(other)

    def __ior__(self, other):

        return self.orwhere(other)
