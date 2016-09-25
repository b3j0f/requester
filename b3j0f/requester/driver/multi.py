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

"""Request system definition."""

__all__ = ['MultiDriver']


from .base import Driver

from ..request.core import Request
from ..request.expr import Expression, Function, FuncName
from ..request.cruder.create import Create
from ..request.cruder.read import Read
from ..request.cruder.update import Update
from ..request.cruder.delete import Delete
from ..request.cruder.exe import Exe

try:
    from threading import Thread

except ImportError:
    from dummy_threading import Thread


class MultiDriver(Driver):

    __LAST_DRIVER__ = '__last_driver__'

    def __init__(self, drivers, *args, **kwargs):

        super(MultiDriver, self).__init__(*args, **kwargs)

        self.drivers = drivers

    def process(self, request, **kwargs):

        result = self._processquery(query=query, ctx=ctx, **kwargs)

        for cruder in request.cruders:

            if isinstance(cruder, (Create, Update, Exe)):
                if isinstance(cruder.name, Expression):
                    ctx = self._processquery(query=cruder.name, ctx=ctx).ctx
                    names = [cruder.name.name]

                else:
                    names = [cruder.name]

            elif isinstance(cruder, (Read, Delete)):
                names = []

                items = cruder.select() if isinstance(cruder, Read) else cruder.names

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

                request = Request(query=query, ctx=ctx, cruders=cruder)

                for processingdriver in processingdrivers:

                    request = processingdriver.process(request=request)

                    self._updatectx(ctx, request.ctx)

        request.ctx = ctx

        return result

    def _processquery(self, query, ctx, _lastquery=None)

        if _lastquery is None:
            _lastquery = query

        names = query.name.split('.')

        driver = self.drivers.get(names[0])
        lastdriver = ctx.get(__LAST_DRIVER__, driver)

        if driver is None:
            driver = lastdriver

        else:
            if driver != lastdriver:
                ctx[__LAST_DRIVER__] = driver

        if isinstance(query, Function):

            isor = query.name == FuncName.OR.value

            for param in query.params:

                pctx = deepcopy(ctx) if isor else ctx

                self._processquery(param, pctx, _lastquery=_lastquery or query)

                if isor:
                    self._updatectx(ctx, pctx)

        if query == _lastquery:

            if lastdriver is None:
                lastdriver = ctx.get(__LAST_DRIVER__, driver)

            if lastdriver is None:

                processingdrivers = [
                    driver for driver in self.drivers.values()
                    if names[0] in driver.getschemas()
                ]

            else:
                processingdrivers = [lastdriver]

            request = Request(query=query, ctx=ctx)

            for processingdriver in processingdrivers:

                processingdriver.process(request=request)

        return request

    def _updatectx(ctx, newctx):

        for key, dataset in iteritems(pctx):

            ctx[key] = ctx.get(key, set()) + set(dataset)
