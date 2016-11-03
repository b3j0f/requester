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

from ..utils import getnames, getsubitem


class GetNamesTest(UTCase):

    def test_one(self):

        names = getnames('test')

        self.assertEqual(names, ['test'])

    def test_many(self):

        names = getnames('test.example')

        self.assertEqual(names, ['test', 'example'])


class GetSubItemTest(UTCase):

    def setUp(self):

        self.item = {'test': {'test': {'test': None}}}

    def test_one(self):

        subitem = getsubitem(item=self.item, name='test')

        self.assertIs(subitem, self.item['test'])

    def test_two(self):

        subitem = getsubitem(item=self.item, name='test.test')

        self.assertIs(subitem, self.item['test']['test'])

    def test_falseerror(self):

        subitem = getsubitem(item=self.item, name='a')

        self.assertIsNone(subitem)

    def test_error(self):

        self.assertRaises(
            KeyError, getsubitem, item=self.item, name='a', error=True
        )

if __name__ == '__main__':
    main()
