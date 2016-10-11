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

"""Transaction module."""

from uuid import uuid4 as uuid

from .ctx import Context

from ..request.crud.create import Create
from ..request.crud.delete import Delete
from ..request.crud.read import Read
from ..request.crud.update import Update

from ..utils import tostr

__all__ = ['Transaction', 'State']


class State(object):
    """Transaction state."""

    PENDING = 1
    COMMITTING = 2
    ROLLBACKING = 3


class Transaction(object):
    """Request execution context."""

    __slots__ = [
        'driver', 'parent', 'cruds', 'autocommit', 'uuid', 'state', 'ctx',
        'dparams'
    ]

    def __init__(
            self, driver,
            parent=None, autocommit=False, ctx=None, cruds=None, dparams=None,
            *args, **kwargs
    ):
        """
        :param Driver driver: data base driver.
        :param Transaction parent: parent transaction.
        :param bool autocommit: if True (default False), commit as soon a CRUD
            operation is processed by this transaction.
        :param Context ctx: CRUD execution context.
        :param list cruds: crud operations.
        :param dict dparams: driver specific parameters.
        """

        super(Transaction, self).__init__(*args, **kwargs)

        self.driver = driver
        self.uuid = uuid()
        self.parent = parent
        self.cruds = [] if cruds is None else cruds
        self.state = State.PENDING
        self.autocommit = autocommit
        self.ctx = Context() if ctx is None else ctx
        self.dparams = dparams

    def __enter__(self):

        return self

    def __exit__(self, exc_type, exc_value, exc_tb):

        if exc_type is None:
            self.commit()

        else:
            self.rollback()
            raise exc_value

    def __repr__(self):

        return tostr(self)

    def commit(self, **kwargs):
        """Commit this transaction and return this.

        While committing, this transaction change of state. After that, this
        transaction state go back to PENDING.
        :return: driver processing result."""

        self.state = State.COMMITTING

        result = self.driver.process(transaction=self, **kwargs)

        self.state = State.PENDING

        return result

    def rollback(self, **kwargs):
        """Rollback this transaction and return this.

        While rollbacking, this transaction change of state. After that, this
        transaction state go back to PENDING.

        :param dict kwargs: driver kwargs.
        :return: driver processing result."""

        self.state = State.ROLLBACKING

        result = self.driver.process(transaction=self, **kwargs)

        self.state = State.PENDING

        return result

    def process(self, cruds, **kwargs):
        """Process input cruds with control paremeters.

        :param dict kwargs: driver specific kwargs. See Driver.process for more
            details.
        :return: process execution."""

        self.cruds += cruds

        if self.autocommit:
            self.state = State.COMMITTING

        else:
            return self.driver.process(transaction=self, **kwargs)

    def open(self, autocommit=False, cruds=None):

        return Transaction(
            driver=self.driver, parent=self, ctx=self.ctx,
            autocommit=autocommit, cruds=cruds
        )

    def _process(self, cls, **kwargs):

        crud = cls(transaction=self, **kwargs)

        self.process(cruds=[crud])

        return self

    def create(self, **kwargs):

        return self._process(cls=Create, **kwargs)

    def read(self, **kwargs):

        return self._process(cls=Read, **kwargs)

    def update(self, **kwargs):

        return self._process(cls=Update, **kwargs)

    def delete(self, **kwargs):

        return self._process(cls=Delete, **kwargs)
