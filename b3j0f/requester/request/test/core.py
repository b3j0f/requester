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

"""request.base UTs."""

from b3j0f.utils.ut import UTCase

from unittest import main

from ..core import Request
from ..expr import Expression as E, Function as F, FuncName as FN
from ..crude.create import Create
from ..crude.read import Read
from ..crude.update import Update
from ..crude.delete import Delete
from ..crude.exe import Exe


class RequestTest(UTCase):

    def setUp(self):

        self.requests = []

        class TestDriver(object):

            def process(_self, request, **kwargs):

                self.requests.append(request)

        self.driver = TestDriver()

        self.request = Request(driver=self.driver)

    def test_init_default(self):

        request = Request()

        self.assertIsNone(request.driver)
        self.assertEqual(request.ctx, {})
        self.assertIsNone(request.query)
        self.assertEqual(request.crudes, [])

    def test_init_errorquery(self):

        self.assertRaises(TypeError, Request, query=1)

    def test_init_errorcrudes(self):

        self.assertRaises(TypeError, Request, crudes=[1])

    def test_and_query(self):

        self.request.query = E.A

        self.assertEqual(self.request.query.name, 'A')

        self.request.query &= E.B

        self.assertEqual(self.request.query.name, FN.AND.value)
        self.assertEqual(
            self.request.query.params[0].name,
            'A'
        )
        self.assertEqual(
            self.request.query.params[1].name,
            'B'
        )

    def test_or_query(self):

        self.request.query = E.A

        self.assertEqual(self.request.query.name, 'A')

        self.request.query |= E.B

        self.assertEqual(self.request.query.name, FN.OR.value)
        self.assertEqual(
            self.request.query.params[0].name,
            'A'
        )
        self.assertEqual(
            self.request.query.params[1].name,
            'B'
        )

    def test_del_query(self):

        self.request.query = E.A

        self.assertEqual(self.request.query.name, 'A')

        del self.request.query

        self.assertIsNone(self.request.query)

    def test_and__query(self):

        self.request.query = E.A

        self.assertEqual(self.request.query.name, 'A')

        request = self.request.and_(E.B)

        self.assertIs(request, self.request)

        self.assertEqual(self.request.query.name, FN.AND.value)
        self.assertEqual(
            self.request.query.params[0].name,
            'A'
        )
        self.assertEqual(
            self.request.query.params[1].name,
            'B'
        )

    def test_or__query(self):

        self.request.query = E.A

        self.assertEqual(self.request.query.name, 'A')

        request = self.request.or_(E.B)

        self.assertIs(request, self.request)

        self.assertEqual(self.request.query.name, FN.OR.value)
        self.assertEqual(
            self.request.query.params[0].name,
            'A'
        )
        self.assertEqual(
            self.request.query.params[1].name,
            'B'
        )

    def test_commit(self):

        self.request.commit()

        self.assertEqual(self.requests, [self.request])

    def test_select(self):

        value = 'test'
        read = self.request.select(value)

        self.assertIsInstance(read, Read)
        self.assertEqual(read.select(), (value,))

    def test_offset(self):

        value = 1
        read = self.request.offset(value)

        self.assertIsInstance(read, Read)
        self.assertEqual(read.offset(), value)

    def test_limit(self):

        value = 1
        read = self.request.limit(value)

        self.assertIsInstance(read, Read)
        self.assertEqual(read.limit(), value)

    def test_groupby(self):

        value = 'test'
        read = self.request.groupby(value)

        self.assertIsInstance(read, Read)
        self.assertEqual(read.groupby(), (value,))

    def test_orderby(self):

        value = 'test'
        read = self.request.orderby(value)

        self.assertIsInstance(read, Read)
        self.assertEqual(read.orderby(), (value,))

    def test_join(self):

        value = 'full'
        read = self.request.join(value)

        self.assertIsInstance(read, Read)
        self.assertEqual(read.join(), value)

    def test_processcrude(self):

        crudes = [
            Create('create', {}),
            Read(),
            Update('update', {}),
            Delete()
        ]

        self.request.processcrude(*crudes)

        self.assertIn(self.request, self.requests)
        self.assertEqual(self.request.crudes, crudes)

    def test_create(self):

        name = 'test'
        value = {'a': 1, 'b': 2}

        self.request.create(name, **value)

        self.assertIn(self.request, self.requests)
        crude = self.request.crudes[0]

        self.assertIsInstance(crude, Create)
        self.assertIs(crude.request, self.request)
        self.assertEqual(crude.name, name)
        self.assertEqual(crude.value, value)

    def test_read(self):

        select = ('test',)
        limit = 1

        self.request.read(select=select, limit=limit)

        self.assertIn(self.request, self.requests)
        crude = self.request.crudes[0]

        self.assertIsInstance(crude, Read)
        self.assertIs(crude.request, self.request)
        self.assertEqual(crude.select(), select)
        self.assertEqual(crude.limit(), limit)

    def test_update(self):

        name = 'test'
        values = {'a': 1, 'b': 2}

        self.request.update(name, **values)

        self.assertIn(self.request, self.requests)
        crude = self.request.crudes[0]

        self.assertIsInstance(crude, Update)
        self.assertIs(crude.request, self.request)
        self.assertEqual(crude.name, name)
        self.assertEqual(crude.values, values)

    def test_delete(self):

        names = ('test',)

        self.request.delete(*names)

        self.assertIn(self.request, self.requests)
        crude = self.request.crudes[0]

        self.assertIsInstance(crude, Delete)
        self.assertIs(crude.request, self.request)
        self.assertEqual(crude.names, names)

    def test_exe(self):

        name = 'test'
        params = (1, 2)

        self.request.exe(name, *params)

        self.assertIn(self.request, self.requests)
        crude = self.request.crudes[0]

        self.assertIsInstance(crude, Exe)
        self.assertIs(crude.request, self.request)
        self.assertEqual(crude.name, name)
        self.assertEqual(crude.params, params)

if __name__ == '__main__':
    main()
