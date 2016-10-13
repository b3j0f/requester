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

from ..base import Driver


class DriverTest(UTCase):

    def setUp(self):

        self.transactions = []
        self.lkwargs = []

        self_ = self

        class TestDriver(Driver):

            name = 'test'

            def _process(self, transaction, **kwargs):

                self_.transactions.append(transaction)
                self_.lkwargs.append(kwargs)

                return transaction

        self.drivercls = TestDriver
        self.driver = TestDriver()

        self.transaction = self.driver.open(autocommit=True)

    def test_class_name(self):

        self.assertEqual(self.drivercls.name, 'test')

    def test_custom_name(self):

        self.assertEqual(self.drivercls(name='example').name, 'example')

    def test_kwargs(self):

        kwargs = {'bar': 'foo'}

        fkwargs = kwargs.copy()
        fkwargs['async'] = False

        self.driver.process(transaction=self.transaction, **kwargs)

        self.assertEqual(self.transactions, [self.transaction])
        self.assertEqual(self.lkwargs, [fkwargs])

    def test_callback(self):

        kwargs = {'bar': 'foo'}

        fkwargs = kwargs.copy()
        fkwargs['async'] = False

        transactions = []

        def callback(transaction, **kwargs):
            transactions.append((transaction, kwargs))

        self.driver.process(
            transaction=self.transaction, callback=callback, **kwargs
        )

        self.assertEqual(self.transactions, [self.transaction])
        self.assertEqual(self.lkwargs, [fkwargs])
        self.assertEqual(transactions, [(self.transaction, fkwargs)])

    def test_async(self):

        kwargs = {'bar': 'foo'}

        fkwargs = kwargs.copy()
        fkwargs['async'] = True

        transactions = []

        def callback(transaction, **kwargs):
            transactions.append((transaction, kwargs))

        thread = self.driver.process(
            transaction=self.transaction, callback=callback, async=True,
            **kwargs
        )

        thread.join()

        self.assertEqual(self.transactions, [self.transaction])
        self.assertEqual(self.lkwargs, [fkwargs])
        self.assertEqual(transactions, [(self.transaction, fkwargs)])

if __name__ == '__main__':
    main()
