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

from unittest import main

from b3j0f.utils.ut import UTCase

from ..base import BaseElement


class BaseElementTest(UTCase):

    def test_init_default(self):

        base = BaseElement()

        self.assertIsNone(base.alias)

    def test_init(self):

        base = BaseElement(alias='alias')

        self.assertEqual(base.alias, 'alias')

    def test_as_(self):

        alias = BaseElement()

        alias.as_(alias='alias')

        self.assertEqual(alias.alias, 'alias')

    def test_refers(self):

        alias = BaseElement.refers('test')

        self.assertIsInstance(alias, BaseElement)
        self.assertEqual(alias.ctxname, 'test')

    def test_refers_elt(self):

        elt = BaseElement()

        alias = BaseElement.refers(elt)

        self.assertIsInstance(alias, BaseElement)
        self.assertEqual(alias.ctxname, elt.ctxname)

    def test_uuid(self):

        base0, base1 = BaseElement(), BaseElement()

        self.assertNotEqual(base0.uuid, base1.uuid)

        base0, base1 = BaseElement(uuid=1), BaseElement(uuid=1)

        self.assertEqual(base0.uuid, base1.uuid)

    def test_ctxname(self):

        class TestElement(BaseElement):
            pass

        elt = TestElement()

        self.assertEqual(elt.ctxname, elt.uuid)

        elt.alias = True

        self.assertEqual(elt.ctxname, elt.alias)

    def test_eq(self):

        class TestElement(BaseElement):
            pass

        base0, base1 = TestElement(), TestElement()

        self.assertNotEqual(base0, base1)

        base0.alias = base1.alias = 1

        self.assertEqual(base0, base1)

        base0.alias = base1.alias = None

        self.assertNotEqual(base0, base1)

        base0.uuid = base1.uuid

        self.assertEqual(base0, base1)

        base0.uuid = None

        self.assertNotEqual(base0, base1)

if __name__ == '__main__':
    main()
