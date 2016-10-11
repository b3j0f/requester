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

"""Module which specifices a composite of drivers."""

from copy import deepcopy

from b3j0f.schema import Schema, data2schema

from .base import Driver

from ..request.consts import FuncName
from ..request.crud.create import Create
from ..request.crud.delete import Delete
from ..request.crud.read import Read
from ..request.crud.update import Update
from ..request.expr import Expression, Function


__all__ = ['DriverComposite']


class DriverComposite(Driver):

    __LAST_DRIVER__ = '__last_driver__'

    def __init__(self, drivers, *args, **kwargs):
        """
        :param list drivers: drivers to use.
        """

        super(DriverComposite, self).__init__(*args, **kwargs)

        self.drivers = {}
        for driver in drivers:
            if not isinstance(driver, Schema):
                driver = data2schema(driver, name=driver.name, _force=True)

            self.drivers[driver.name] = driver

    def _process(self, transaction, crud, **kwargs):

        result = self._processquery(query=crud, ctx=transaction.ctx, **kwargs)

        if isinstance(crud, (Create, Update)):
            if isinstance(crud.name, Expression):
                transaction = self._processquery(
                    query=crud.name, ctx=transaction.ctx
                )
                names = [crud.name.name]

            else:
                names = [crud.name]

        elif isinstance(crud, (Read, Delete)):
            names = []

            items = crud.select() if isinstance(crud, Read) else crud.names

            for item in items:
                if isinstance(item, Expression):
                    ctx = self._processquery(query=item, ctx=transaction.ctx)
                    names.append(item.name)

                else:
                    names.append(item)

        if names:
            driver = self.drivers.get(names[0])

            if driver is None:

                processingdrivers = [
                    _driver for _driver in self.drivers.values()
                    if names[0] in _driver.getschemas()
                ]

            else:
                processingdrivers = [driver]

            transaction = transaction.open(ctx=ctx, cruds=[crud])

            for processingdriver in processingdrivers:

                pctx = processingdriver.process(transaction=transaction)

                ctx.fill(pctx)

        transaction.ctx = ctx

        return result

    def _processquery(self, query, ctx, _lastquery=None, **kwargs):
        """Parse deeply the query from the left to the right and aggregates
        queries which refers to the same system without any intermediate system
        .

        :param Expression query: query to process.
        :param Context ctx: context execution.
        :param Expression _lastquery: private parameter which save the last
            query to process.
        :return: ctx
        :rtype: ctx
        """

        if _lastquery is None:
            _lastquery = query

        names = query.name.split('.')

        driver = self.drivers.get(names[0])
        lastdriver = ctx.get(DriverComposite.__LAST_DRIVER__, driver)

        if driver is None:
            driver = lastdriver

        else:
            if driver != lastdriver:
                ctx[DriverComposite.__LAST_DRIVER__] = driver

        if isinstance(query, Function):

            isor = query.name == FuncName.OR.value

            for param in query.params:
                if isinstance(param, Expression):

                    pctx = deepcopy(ctx) if isor else ctx

                    self._processquery(
                        query=param, ctx=pctx,
                        _lastquery=query if _lastquery is None else _lastquery,
                        **kwargs
                    )

                    if isor:
                        ctx.fill(pctx)

        if query == _lastquery:

            if lastdriver is None:
                lastdriver = ctx.get(DriverComposite.__LAST_DRIVER__, driver)

            if lastdriver is None:

                processingdrivers = [
                    _driver for _driver in self.drivers.values()
                    if names[0] in _driver.getschemas()
                ]

            else:
                processingdrivers = [lastdriver]

            transaction = driver.open(cruds=[query], ctx=ctx)

            for processingdriver in processingdrivers:

                # prepare a read operation for the inner driver
                crudname = query.ctxname  # prepare crud name
                if crudname.startswith(processingdriver.name):
                    crudname = crudname[len(processingdriver.name) + 1:]

                crud = Read(
                    select=[crudname] if crudname else None,
                    alias=query.ctxname  # set alias equals query ctxname
                )

                transaction.cruds = [crud]

                transaction = processingdriver.process(
                    transaction=transaction, **kwargs
                )

                ctx = transaction.ctx

        return ctx

    def __repr__(self):

        return 'CompositeDriver({0}, {1})'.format(self.name, self.drivers)
