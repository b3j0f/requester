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

from ..read import Read


class ReadTest(UTCase):

    def test_init_default(self):

        read = Read()

        self.assertIsNone(read.select())
        self.assertIsNone(read.offset())
        self.assertIsNone(read.limit())
        self.assertIsNone(read.orderby())
        self.assertIsNone(read.groupby())
        self.assertFalse(read.join())

    def test_init(self):

        select = 'select'
        offset = 1
        limit = 2
        orderby = 'orderby'
        groupby = 'groupby'
        join = 'join'

        read = Read(
            select=(select,), offset=offset, limit=limit, orderby=(orderby,),
            groupby=(groupby,), join=join
        )

        self.assertEqual((select,), read.select())
        self.assertEqual(offset, read.offset())
        self.assertEqual(limit, read.limit())
        self.assertEqual((orderby,), read.orderby())
        self.assertEqual((groupby,), read.groupby())
        self.assertEqual(join, read.join())

    def test_init_error(self):

        select = 0
        offset = '1'
        limit = '2'
        orderby = 0
        groupby = 0
        join = 0

        self.assertRaises(TypeError, Read, select=select)
        self.assertRaises(TypeError, Read, offset=offset)
        self.assertRaises(TypeError, Read, limit=limit)
        self.assertRaises(TypeError, Read, orderby=orderby)
        self.assertRaises(TypeError, Read, groupby=groupby)
        self.assertRaises(TypeError, Read, join=join)

    def test_chaining(self):

        select = 'select'
        offset = 1
        limit = 2
        orderby = 'orderby'
        groupby = 'groupby'
        join = 'join'

        read = Read()

        readbis = read.select(select).offset(offset).limit(limit)
        readbis = readbis.orderby(orderby).groupby(groupby).join(join)

        self.assertIs(read, readbis)

        self.assertEqual((select,), read.select())
        self.assertEqual(offset, read.offset())
        self.assertEqual(limit, read.limit())
        self.assertEqual((orderby,), read.orderby())
        self.assertEqual((groupby,), read.groupby())
        self.assertEqual(join, read.join())

if __name__ == '__main__':
    main()
