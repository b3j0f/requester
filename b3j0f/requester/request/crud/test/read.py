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

from ..read import Read


class ReadTest(UTCase):

    def test_init_default(self):

        read = Read()

        self.assertIsNone(read.getselect)
        self.assertIsNone(read.getoffset)
        self.assertIsNone(read.getlimit)
        self.assertIsNone(read.getorderby)
        self.assertIsNone(read.getgroupby)
        self.assertIsNone(read.getjoin)

    def test_init(self):

        select, offset, limit, orderby, groupby, join = (i for i in range(6))

        read = Read(
            select=select, offset=offset, limit=limit, orderby=orderby,
            groupby=groupby, join=join
        )

        self.assertEqual(select, read.getselect)
        self.assertEqual(offset, read.getoffset)
        self.assertEqual(limit, read.getlimit)
        self.assertEqual(orderby, read.getorderby)
        self.assertEqual(groupby, read.getgroupby)
        self.assertEqual(join, read.getjoin)

if __name__ == '__main__':
    main()
