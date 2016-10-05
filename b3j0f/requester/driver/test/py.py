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
from ..py import PyDriver, processcrud, create, read, update, delete
from ..generator import func2crudprocessing, obj2driver, DriverAnnotation
from ..transaction import Transaction
from ..ctx import Context
from ...request.crud.create import Create
from ...request.crud.read import Read
from ...request.crud.update import Update
from ...request.crud.delete import Delete


class CRUDTest(UTCase):

    def setUp(self):

        self.items = [{'name': str(i), 'id': i} for i in range(5)]


class CreateTest(CRUDTest):

    def test(self):

        crud = Create(name='test', values=True)

        result = create(items=self.items, create=crud)

        self.assertIn(crud.values, self.items)

        self.assertIs(result, self.items)

class ReadTest(CRUDTest):

    def test_default(self):

        crud = Read()

        result = read(items=self.items, read=crud)

        self.assertEqual(result, self.items)

        self.assertIsNot(result, self.items)

    def test_select(self):

        crud = Read(select=['id'])

        result = read(items=self.items, read=crud)

        self.assertEqual(result, [{'id': i} for i in range(5)])

        self.assertIsNot(result, self.items)

    def test_offset(self):

        crud = Read(offset=1)

        result = read(items=self.items, read=crud)

        self.assertEqual(result, self.items[1:])

        self.assertIsNot(result, self.items)

    def test_limit(self):

        crud = Read(limit=1)

        result = read(items=self.items, read=crud)

        self.assertEqual(result, self.items[:1])

        self.assertIsNot(result, self.items)

    def test_groupby(self):

        crud = Read(groupby=['a'])

        self.assertRaises(
            NotImplementedError, read, items=self.items, read=crud
        )

    def test_join(self):

        crud = Read(join='')

        self.assertRaises(
            NotImplementedError, read, items=self.items, read=crud
        )


class UpdateTest(CRUDTest):

    def test(self):

        crud = Update(name='', values={'name': 1})

        result = update(items=self.items, update=crud)

        self.assertIs(result, self.items)

        self.assertEqual(result, [{'name': 1, 'id': i} for i in range(5)])


class DeleteTest(CRUDTest):

    def test_default(self):

        crud = Delete()

        result = delete(items=self.items, delete=crud)

        self.assertIs(result, self.items)

        self.assertFalse(result)

    def test_name(self):

        crud = Delete(names=('name', ))

        result = delete(items=self.items, delete=crud)

        self.assertIs(result, self.items)

        self.assertEqual(result, [{'id': i} for i in range(5)])


class ProcessCRUDTest(CRUDTest):

    def setUp(self):

        super(ProcessCRUDTest, self).setUp()

        self.ctx = Context()

    def _assert(self, crud):

        result = processcrud(crud=crud, ctx=self.ctx, items=self.items)

        self.assertEqual(result, self.ctx[crud])

    def test_create(self):

        crud = Create(name='', values={})

        self._assert(crud=crud)

    def test_read(self):

        crud = Read()

        self._assert(crud=crud)

    def test_update(self):

        crud = Update(name='', values={})

        self._assert(crud=crud)

    def test_delete(self):

        crud = Delete()

        self._assert(crud=crud)


class PyDriverTest(UTCase):

    def setUp(self):

        self.driver = PyDriver()

if __name__ == '__main__':
    main()
