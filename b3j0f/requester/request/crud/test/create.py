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
from ..create import Create


class CreateTest(UTCase):

    def test_init(self):

        values = {None: None}

        create = Create(name='test', values=values)

        self.assertEqual('test', create.name)
        self.assertEqual(values, create.values)

    def test___repr__(self):
        cases = [
            {
                'values': {},
                'name': Exp.c,
                'where': None,
                'as': None,
                'expected': "CREATE c:{}",
            },
            {
                'values': {Exp.key: 'value'},
                'name': Exp.c,
                'where': None,
                'as': None,
                'expected': "CREATE c:{key: 'value'}",
            },
            {
                'values': {Exp.k: 'v'},
                'name': '',
                'where': None,
                'as': None,
                'expected': "CREATE {k: 'v'}",
            },
            {
                'values': {Exp.k: 'v'},
                'name': '',
                'where': Exp.w,
                'as': None,
                'expected': "CREATE {k: 'v'} WHERE w",
            },
            {
                'values': {Exp.k: 'v'},
                'name': '',
                'where': None,
                'as': 'a',
                'expected': "CREATE {k: 'v'} AS a",
            },
        ]

        for test in cases:
            c = Create(
                values=test['values'],
                name=test['name'],
            )

            if test['where']:
                c = c.where(test['where'])

            if test['as']:
                c = c.as_(test['as'])

            self.assertEqual(repr(c), test['expected'])

if __name__ == '__main__':
    main()
