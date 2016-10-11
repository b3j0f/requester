#!/usr/bin/env python
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

"""conf file driver UTs."""

from unittest import main

from b3j0f.utils.ut import UTCase

from ..composite import DriverComposite, updatename

from ..base import Driver
from ..ctx import Context
from ..transaction import Transaction
from ...request.expr import Expression, Function
from ...request.crud.create import Create
from ...request.crud.delete import Delete
from ...request.crud.read import Read
from ...request.crud.update import Update


class TestDriver(Driver):
    """Test driver."""

    def __init__(self, *args, **kwargs):

        super(TestDriver, self).__init__(*args, **kwargs)

        self.transactions = []

    def process(self, transaction, *args, **kwargs):

        result = super(TestDriver, self).process(
            transaction=transaction, *args, **kwargs
        )

        self.transactions.append((transaction, kwargs))

        return result


class DriverCompositeTest(UTCase):
    """Tests for the driver composite."""

    def setUp(self):

        self.drivers = [TestDriver(name='s{0}'.format(i)) for i in range(3)]

        self.s0 = self.drivers[0]
        self.s1 = self.drivers[1]
        self.s2 = self.drivers[2]

        self.driver = DriverComposite(drivers=self.drivers)

        self.transaction = self.driver.open()

    def test_expr(self):

        expr = Expression.A

        self.assertRaises(ValueError, self.transaction.process(cruds=[expr]))

if __name__ == '__main__':
    main()
