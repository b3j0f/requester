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


class DriverTest(UTCase):

    def setUp(self):

        self.requests = []
        self.lkwargs = []

        class TestDriver(Driver):

            name = 'test'

            def process(self_, request, **kwargs):

                self.requests.append(request)
                self.lkwargs.append(kwargs)

                return request

        self.drivercls = TestDriver
        self.driver = TestDriver()

    def test_class_name(self):

        self.assertEqual(self.drivercls.name, 'test')

    def test_custom_name(self):

        self.assertEqual(self.drivercls(name='example').name, 'example')

    def test_kwargs(self):

        kwargs = {'bar': 'foo'}

        result = self.driver.process(request=True, **kwargs)

        self.assertEqual(self.requests, [True])
        self.assertEqual(self.lkwargs, [kwargs])

if __name__ == '__main__':
    main()
