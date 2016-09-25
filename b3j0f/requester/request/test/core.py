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
        self.assertEqual(request.cruds, [])

    def test_init_errorquery(self):

        self.assertRaises(TypeError, Request, query=1)

    def test_init_errorcruds(self):

        self.assertRaises(TypeError, Request, cruds=[1])

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

if __name__ == '__main__':
    main()
