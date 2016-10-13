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

from ...expr import Expression as Exp
from ..delete import Delete


class DeleteTest(UTCase):

    def test_init_default(self):

        delete = Delete()

        self.assertFalse(delete.names())

    def test_init(self):

        names = 1

        delete = Delete(names=names)

        self.assertEqual(names, delete.names())

    def test___repr__(self):
        cases = [
            {
                'names': [Exp.d],
                'where': None,
                'as': None,
                'expected': "DELETE d",
            },
            {
                'names': [Exp.d1, Exp.d2],
                'where': None,
                'as': None,
                'expected': "DELETE d1, d2",
            },
            {
                'names': [Exp.d],
                'where': Exp.w,
                'as': None,
                'expected': "DELETE d WHERE w",
            },
            {
                'names': [Exp.d],
                'where': None,
                'as': 'a',
                'expected': "DELETE d AS a",
            },
        ]

        for test in cases:
            d = Delete(names=test['names'])

            if test['where']:
                d = d.where(test['where'])

            if test['as']:
                d = d.as_(test['as'])

            self.assertEqual(repr(d), test['expected'])


if __name__ == '__main__':
    main()
