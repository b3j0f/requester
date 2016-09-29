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

"""Module which specifices a composite of drivers."""

__all__ = ['DriverComposite']

from .base import Driver

from ..request.core import Request
from ..request.expr import Expression, Function, FuncName
from ..request.crude.create import Create
from ..request.crude.read import Read
from ..request.crude.update import Update
from ..request.crude.delete import Delete
from ..request.crude.exe import Exe

from b3j0f.schema import data2schema, Schema

try:
    from threading import Thread

except ImportError:
    from dummy_threading import Thread


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
                driver = data2schema(driver, _force=True)
            self.drivers[driver.name] = driver

    def process(self, request, **kwargs):

        result = self._processquery(query=query, ctx=ctx, **kwargs)

        for crude in request.crudes:

            if isinstance(crude, (Create, Update, Exe)):
                if isinstance(crude.name, Expression):
                    ctx = self._processquery(query=crude.name, ctx=ctx).ctx
                    names = [crude.name.name]

                else:
                    names = [crude.name]

            elif isinstance(crude, (Read, Delete)):
                names = []

                items = crude.select() if isinstance(crude, Read) else crude.names

                for item in items:
                    if isinstance(item, Expression):
                        ctx = self._processquery(query=item, ctx=ctx).ctx
                        ctx = request.ctx
                        names.append(item.name)

                    else:
                        names.append(item)

            if names:
                driver = self.drivers.get(names[0])

                if driver is None:

                    processingdrivers = [
                        driver for driver in self.drivers.values()
                        if names[0] in driver.getschemas()
                    ]

                else:
                    processingdrivers = [driver]

                request = Request(query=query, ctx=ctx, crudes=crude)

                for processingdriver in processingdrivers:

                    request = processingdriver.process(request=request)

                    self._updatectx(ctx, request.ctx)

        request.ctx = ctx

        return result

    def _processquery(self, query, ctx, _lastquery=None, **kwargs):
        """Parse deeply the query from the left to the right and aggregates
        queries which refers to the same system without any intermediary system.

        :param Expression query: query to process.
        :param Context ctx: context execution.
        :param Expression _lastquery: private parameter which save the last
            query to process.
        """

        if _lastquery is None:
            _lastquery = query

        print('start', query, ctx, _lastquery, kwargs)

        names = query.name.split('.')
        print('names', names)

        driver = self.drivers.get(names[0])
        lastdriver = ctx.get(DriverComposite.__LAST_DRIVER__, driver)
        print('driver', driver, 'lastdriver', lastdriver)

        if driver is None:
            driver = lastdriver

        else:
            if driver != lastdriver:
                ctx[DriverComposite.__LAST_DRIVER__] = driver

        print('driver', driver)

        if isinstance(query, Function):

            isor = query.name == FuncName.OR.value

            for param in query.params:
                print('param', param)
                if isinstance(param, Expression):

                    pctx = deepcopy(ctx) if isor else ctx

                    self._processquery(
                        query=param, ctx=pctx, _lastquery=_lastquery or query,
                        **kwargs
                    )

                    if isor:
                        ctx.fill(pctx)

        if query == _lastquery:
            print('process', query)

            if lastdriver is None:
                lastdriver = ctx.get(DriverComposite.__LAST_DRIVER__, driver)

            if lastdriver is None:

                processingdrivers = [
                    driver for driver in self.drivers.values()
                    if names[0] in driver.getschemas()
                ]

            else:
                processingdrivers = [lastdriver]

            request = Request(
                query=query, ctx=ctx, crudes=[Read(select=[query.ctxname])]
            )

            for processingdriver in processingdrivers:

                processingdriver.process(request=request, **kwargs)

        return request
