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

"""request.base UTs."""

from collections import Hashable

from unittest import main

from b3j0f.utils.ut import UTCase

from ..ctx import Context, getctxname
from ...request.crud.base import BaseElement


class GetCtxNameTest(UTCase):

    def test_hashable(self):

        self.assertEqual(getctxname('test'), 'test')

    def test_baseelement(self):

        baseelement = BaseElement()

        self.assertEqual(getctxname(baseelement), baseelement.ctxname)

    def test_nothashable(self):

        elt = {}

        self.assertIsInstance(getctxname(elt), Hashable)

        self.assertEqual(getctxname(elt), getctxname(elt))

        self.assertEqual(getctxname({1: 1, 2: 2}), getctxname({2: 2, 1: 1}))

        self.assertNotEqual(getctxname({1: 1}), getctxname({1: None}))

    def test_nothashable_nothashable(self):

        elt = [[{1: []}]]

        self.assertIsInstance(getctxname(elt), Hashable)

        self.assertEqual(getctxname(elt), getctxname(list(elt)))


class ContextTest(UTCase):

    def test_init(self):

        self.assertFalse(Context())

    def test_init_params(self):

        context = Context({'a': 1})

        self.assertTrue(context)

        self.assertIn('a', context)

    def test_crud(self):

        context = Context()
        crud = BaseElement()

        self.assertNotIn(crud, context)

        context[crud] = 1

        self.assertIn(crud, context)
        self.assertIn(crud.ctxname, context)

        self.assertEqual(context[crud], 1)
        self.assertEqual(context[crud.ctxname], 1)

        del context[crud]

        self.assertNotIn(crud, context)
        self.assertNotIn(crud.ctxname, context)

    def test_fill(self):

        context = Context({
            'a.b': [1, 2, 4],
            'c': [5]
        })

        _context = Context({
            'b': [3],
            'c': [4],
            'a': [5]
        })

        context.fill(_context)

        self.assertEqual(context['a'], [5])
        self.assertEqual(context['b'], [3])
        self.assertEqual(context['c'], [5, 4])
        self.assertEqual(context['a.b'], [1, 2, 4, 3])

if __name__ == '__main__':
    main()
