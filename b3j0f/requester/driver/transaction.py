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

from enum import IntEnum, unique

from .ctx import Context
from ..request.crud.create import Create
from ..request.crud.delete import Delete
from ..request.crud.read import Read
from ..request.crud.update import Update
from ..utils import tostr

__all__ = ['Transaction', 'State']


@unique
class State(IntEnum):
    """Transaction state."""

    PENDING = 1
    COMMITTING = 2
    ROLLBACKING = 3

    def __repr__(self):

        return self.name


class Transaction(object):
    """Request execution context."""

    __slots__ = [
        'driver', 'cruds', 'autocommit', 'uuid', 'state', 'ctx',
        'dparams'
    ]

    def __init__(
            self, driver, autocommit=False, ctx=None, cruds=None, dparams=None,
            *args, **kwargs
    ):
        """
        :param Driver driver: data base driver.
        :param bool autocommit: if True (default False), commit as soon a CRUD
            operation is processed by this transaction.
        :param Context ctx: CRUD execution context.
        :param list cruds: crud operations.
        :param dict dparams: driver specific parameters.
        """
        super(Transaction, self).__init__(*args, **kwargs)

        self.driver = driver
        self.uuid = uuid()
        self.cruds = [] if cruds is None else cruds
        self.state = State.COMMITTING if autocommit else State.PENDING
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
            raise

    def __repr__(self):

        return tostr(self)

    def commit(self, **kwargs):
        """Commit this transaction and return this.

        While committing, this transaction change of state. After that, this
        transaction state go back to PENDING.

        :param dict kwargs: driver kwargs.
        :return: self.
        :rtype: Transaction
        """

        self.state = State.COMMITTING

        result = self._process(**kwargs)

        if not self.autocommit:
            self.state = State.PENDING

        return result

    def rollback(self, **kwargs):
        """Rollback this transaction and return this.

        While rollbacking, this transaction change of state. After that, this
        transaction state go back to PENDING.

        :param dict kwargs: driver kwargs.
        :rtype: Transaction.
        """

        self.state = State.ROLLBACKING

        result = self._process(**kwargs)

        self.state = State.COMMITTING if self.autocommit else State.PENDING

        return result

    def _process(self, **kwargs):
        """Process input cruds with control paremeters.

        :param dict kwargs: driver specific kwargs. See Driver.process for more
            details.
        :rtype: Transaction.
        """

        if self.autocommit:
            self.state = State.COMMITTING

        result = self.driver.process(transaction=self, **kwargs)

        self.cruds = []

        return result

    def open(self, ctx=None, cruds=None, autocommit=None, driver=None):
        """Open a transaction.

        :param Context ctx: new execution context. Default is this ctx.
        :param list cruds: cruds to use. Default is empty.
        :param bool autocommit: auto commmit flag.
        :param b3j0f.requester.driver.base.Driver driver: transaction driver.
        :rtype: Transaction.
        """
        if autocommit is None:
            autocommit = self.autocommit

        if driver is None:
            driver = self.driver

        if ctx is None:
            ctx = self.ctx

        return Transaction(
            driver=driver, ctx=ctx, autocommit=autocommit, cruds=cruds
        )

    def _processcls(self, cls, **kwargs):
        """Custom processing.

        :return: self.
        :rtype: Transaction.
        """
        crud = cls(transaction=self, **kwargs)

        self._process(cruds=[crud])

        return self

    def create(self, **kwargs):
        """Quick creation.

        :param kwargs: CRUDElement parameters.
        :return: self.
        :rtype: Transaction
        """
        return self._processcls(cls=Create, **kwargs)

    def read(self, **kwargs):
        """Quick reading.

        :param kwargs: CRUDElement parameters.
        :return: self.
        :rtype: Transaction
        """
        return self._processcls(cls=Read, **kwargs)

    def update(self, **kwargs):
        """Quick updating.

        :param kwargs: CRUDElement parameters.
        :return: self.
        :rtype: Transaction
        """
        return self._processcls(cls=Update, **kwargs)

    def delete(self, **kwargs):
        """Quick deletion.

        :param kwargs: CRUDElement parameters.
        :return: self.
        :rtype: Transaction
        """
        return self._processcls(cls=Delete, **kwargs)
