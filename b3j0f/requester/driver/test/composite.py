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

from ..py import PyDriver
from ..base import Driver
from ..ctx import Context
from ..transaction import Transaction
from ...request.expr import Expression, Function
from ...request.crud.create import Create
from ...request.crud.delete import Delete
from ...request.crud.read import Read
from ...request.crud.update import Update


class TestDriver(PyDriver):
    """Test driver."""

    def __init__(self, *args, **kwargs):

        super(TestDriver, self).__init__(*args, **kwargs)

        self.transactions = []

        self.items = [
            {'d{0}'.format(self.name): i, 'id': i} for i in range(5)
        ]

    def _process(self, transaction, *args, **kwargs):

        super(TestDriver, self)._process(
            transaction=transaction, *args, **kwargs
        )
        #print(self.name, transaction.cruds)
        transaction.ctx.setdefault('test', [self])

        self.transactions.append((transaction, kwargs))

        return transaction


class DriverCompositeTest(UTCase):
    """Tests for the driver composite."""

    def setUp(self):

        self.drivers = [
            TestDriver(
                name='d{0}'.format(i)
            ) for i in range(4)
        ]

        self.d0 = self.drivers[0]
        self.d1 = self.drivers[1]
        self.d2 = self.drivers[2]
        self.d3 = self.drivers[3]

        self.driver = DriverComposite(drivers=self.drivers)

        for driver in self.drivers:
            self.assertFalse(driver.transactions)

    def test_expr(self):

        expr = Expression.in_

        self.assertRaises(ValueError, self.driver.open(cruds=[expr]).commit)

    def test_d0(self):

        expr = Expression.d0

        self.driver.open(cruds=[expr]).commit()

        for driver in self.drivers[1:]:
            self.assertFalse(driver.transactions)

        self.assertEqual(len(self.d0.transactions), 1)

    def test_d0_dd0(self):

        expr = Expression.d0.dd0

        self.driver.open(cruds=[expr]).commit()

        for driver in self.drivers[1:]:
            self.assertFalse(driver.transactions)

        self.assertEqual(len(self.d0.transactions), 1)

    def test_expr_d0(self):

        expr = Expression.in_(Expression.d0)

        self.driver.open(cruds=[expr]).commit()

        for driver in self.drivers[1:]:
            self.assertFalse(driver.transactions)

        self.assertEqual(len(self.d0.transactions), 1)

    def test_expr_expr_d0(self):

        expr = Expression.in_(Expression.in_(Expression.d0))

        self.driver.open(cruds=[expr]).commit()

        for driver in self.drivers[1:]:
            self.assertFalse(driver.transactions)

        self.assertEqual(len(self.d0.transactions), 1)

    def test_expr_d0_expr_d0(self):

        expr = Expression.in_(Expression.d0, Expression.in_(Expression.d0))

        self.driver.open(cruds=[expr]).commit()

        for driver in self.drivers[1:]:
            self.assertFalse(driver.transactions)

        self.assertEqual(len(self.d0.transactions), 1)

    def test_expr_d0_d1_d2(self):

        expr = Expression.in_(
            Expression.d0, Expression.in_(Expression.d1), Expression.d2
        )

        self.driver.open(cruds=[expr]).commit()

        for driver in self.drivers[3:]:
            self.assertFalse(driver.transactions)

        self.assertEqual(len(self.d0.transactions), 1)
        self.assertEqual(len(self.d1.transactions), 1)
        self.assertEqual(len(self.d2.transactions), 1)

    def test_expr_d0_expr_d1_expr_d0_d1(self):

        expr = Expression.in_(
            Expression.d0, Expression.in_(
                Expression.d1, Expression.in_(
                    Expression.d0, Expression.d2
                )
            )
        )

        self.driver.open(cruds=[expr]).commit()

        for driver in self.drivers[3:]:
            self.assertFalse(driver.transactions)

        self.assertEqual(len(self.d0.transactions), 1)
        self.assertEqual(len(self.d1.transactions), 1)
        self.assertEqual(len(self.d2.transactions), 1)

if __name__ == '__main__':
    main()
