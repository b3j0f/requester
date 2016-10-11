#!/usr/bin/env python
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

"""conf file driver UTs."""

from b3j0f.utils.ut import UTCase

from unittest import main

from ..base import Driver
from ..py import PyDriver
from ..composite import DriverComposite
from ..custom import func2crudprocessing, obj2driver, DriverAnnotation
from ..transaction import Transaction
from ..ctx import Context
from ...request.expr import Expression as E, Function as F
from ...request.crud.create import Create
from ...request.crud.read import Read
from ...request.crud.update import Update
from ...request.crud.delete import Delete


class TestDriver(PyDriver):

    def __init__(self, *args, **kwargs):

        super(TestDriver, self).__init__(*args, **kwargs)

        self.requests = []

        for i in range(10):
            value = {'name': self.name, 'id': i, 'even': (i & 1) is 0}
            self.values.append(value)

    def process(self, request, **kwargs):

        result = super(TestDriver, self).process(request=request, **kwargs)

        self.requests.append((request, kwargs))

        return result


class DriverCompositeTest(UTCase):

    def setUp(self):

        self.drivers = [TestDriver(name='s{0}'.format(i)) for i in range(3)]

        self.driver = DriverComposite(drivers=self.drivers)

        self.ctx = Context()

        self.request = Transaction(driver=None, ctx=self.ctx)

    def test__processquery_no_system(self):

        query = E.a

        self.driver._processquery(query, ctx=self.request.ctx)

        self.assertFalse(self.request.ctx)

    def test__processquery_s0(self):

        query = E.s0

        self.driver._processquery(query, ctx=self.request.ctx)

        self.assertIn(query, self.request.ctx)
        self.assertEqual(
            self.driver.drivers['s0'].values,
            self.request.ctx[query]
        )

    def test__processquery_s0_property(self):

        query = E.s0.name_

        self.driver._processquery(query, ctx=self.request.ctx)

        self.assertIn(query, self.request.ctx)
        values = [{'name': 's0'}] * 10
        self.assertEqual(self.request.ctx[query], values)

    def test__processquery_none_s0(self):

        expr = E.s0
        query = F.A(expr)

        self.driver._processquery(query=query, ctx=self.request.ctx)

        self.assertIn(expr, self.request.ctx)
        self.assertIn(query, self.request.ctx)

if __name__ == '__main__':
    main()
