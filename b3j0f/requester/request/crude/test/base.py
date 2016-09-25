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

from ..base import CRUDE
from ...core import Request


class CRUDETest(UTCase):

    def test_init_defaul(self):

        crude = CRUDE()

        self.assertIsNone(crude.request)

    def test_init(self):

        request = Request()

        crude = CRUDE(request=request)

        self.assertIs(request, crude.request)

    def test__call__(self):

        requests = []

        class Driver(object):

            def process(self, request, **kwargs):

                requests.append(request)

        request = Request(driver=Driver())

        crude = CRUDE(request=request)
        crude()

        self.assertIn(request, requests)

        self.assertIn(crude, request.crudes)

if __name__ == '__main__':
    main()
