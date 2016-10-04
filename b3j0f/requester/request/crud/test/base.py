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

from ..base import CRUDElement
from ....driver.base import Driver
from ....driver.transaction import Transaction


class CRUDTest(UTCase):

    def test_init_defaul(self):

        crud = CRUDElement()

        self.assertIsNone(crud.transaction)

    def test_init(self):

        transaction = Transaction(driver=None)

        crud = CRUDElement(transaction=transaction)

        self.assertIs(transaction, crud.transaction)

    def test__call__(self):

        requests = []

        class TestDriver(Driver):

            def process(self, transaction, **kwargs):

                requests.append(transaction)

        transaction = Transaction(driver=TestDriver())

        crud = CRUDElement(transaction=transaction)
        crud()

        self.assertIn(transaction, requests)

    def test__call__error(self):

        self.assertRaises(RuntimeError, CRUDElement())

if __name__ == '__main__':
    main()
