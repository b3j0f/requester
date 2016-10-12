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

"""transaction.base UTs."""

from unittest import main

from b3j0f.utils.ut import UTCase

from ..base import Driver

from ..transaction import State, Transaction

from ...request.crud.base import CRUDElement
from ...request.crud.create import Create
from ...request.crud.delete import Delete
from ...request.crud.read import Read
from ...request.crud.update import Update


class TransactionTest(UTCase):

    def setUp(self):

        self.transactions = []
        self.states = []

        self_ = self

        class TestDriver(Driver):

            def process(self, transaction, **kwargs):

                self_.states.append(transaction.state)
                self_.transactions.append(transaction)
                for crud in transaction.cruds:
                    self_.transaction.ctx[crud] = crud

        self.driver = TestDriver()

        self.transaction = Transaction(driver=self.driver)

    def test_init_default(self):

        transaction = Transaction(driver=None)

        self.assertIsNone(transaction.driver)
        self.assertEqual(transaction.ctx, {})

    def test_init_errorquery(self):

        self.assertRaises(TypeError, Transaction, query=1)

    def test_init_errorcruds(self):

        self.assertRaises(TypeError, Transaction, cruds=[1])

    def test_processcrud(self):

        crud = CRUDElement()

        self.transaction.process(cruds=[crud])

        self.assertIs(self.transaction.state, State.PENDING)
        self.assertIn(self.transaction, self.transactions)
        self.assertIn(crud, self.transaction.ctx)

    def test_create(self):

        name = 'test'
        values = {'a': 1, 'b': 2}

        self.transaction.create(name=name, values=values)

        self.assertIn(self.transaction, self.transactions)
        for key in self.transaction.ctx:
            crud = self.transaction.ctx[key]
            break

        self.assertIsInstance(crud, Create)
        self.assertIs(crud.transaction, self.transaction)
        self.assertEqual(crud.name, name)
        self.assertEqual(crud.values, values)

    def test_read(self):

        select = ('test',)
        limit = 1

        self.transaction.read(select=select, limit=limit)

        self.assertIn(self.transaction, self.transactions)

        for key in self.transaction.ctx:
            crud = self.transaction.ctx[key]
            break

        self.assertIsInstance(crud, Read)
        self.assertIs(crud.transaction, self.transaction)
        self.assertEqual(crud.select(), select)
        self.assertEqual(crud.limit(), limit)

    def test_update(self):

        name = 'test'
        values = {'a': 1, 'b': 2}

        self.transaction.update(name=name, values=values)

        self.assertIn(self.transaction, self.transactions)

        for key in self.transaction.ctx:
            crud = self.transaction.ctx[key]
            break

        self.assertIsInstance(crud, Update)
        self.assertIs(crud.transaction, self.transaction)
        self.assertEqual(crud.name, name)
        self.assertEqual(crud.values, values)

    def test_delete(self):

        names = ('test',)

        self.transaction.delete(names=names)

        self.assertIn(self.transaction, self.transactions)

        for key in self.transaction.ctx:
            crud = self.transaction.ctx[key]
            break

        self.assertIsInstance(crud, Delete)
        self.assertIs(crud.transaction, self.transaction)
        self.assertEqual(crud.names(), names)

    def test_open(self):

        newtransaction = self.transaction.open()

        self.assertIsInstance(newtransaction, Transaction)
        self.assertIsNot(newtransaction, self.transaction)
        self.assertIs(newtransaction.parent, self.transaction)
        self.assertIs(newtransaction.ctx, self.transaction.ctx)

    def test_rollback(self):

        self.assertIs(self.transaction.state, State.PENDING)

        self.assertEqual(self.states, [])

        self.transaction.process(cruds=[])

        self.assertEqual(self.states, [State.PENDING])

        self.assertIs(self.transaction.state, State.PENDING)

        self.transaction.commit()

        self.assertEqual(self.states[-1], State.COMMITTING)

        self.assertIs(self.transaction.state, State.PENDING)

        self.transaction.rollback()

        self.assertEqual(self.states[-1], State.ROLLBACKING)

        self.assertIs(self.transaction.state, State.PENDING)

    def test_enter_exit(self):

        self.assertFalse(self.states)

        with self.transaction:
            pass

        self.assertEqual(self.states[-1], State.COMMITTING)

        self.assertIs(self.transaction.state, State.PENDING)

        try:
            with self.transaction:
                raise RuntimeError()

        except:
            pass

        self.assertEqual(self.states[-1], State.ROLLBACKING)

        self.assertIs(self.transaction.state, State.PENDING)

if __name__ == '__main__':
    main()
