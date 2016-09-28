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

from ..utils import Driver, FunctionalDriver
from ...request.core import Request
from ...request.crude.base import CRUDE
from ...request.crude.create import Create
from ...request.crude.read import Read
from ...request.crude.update import Update
from ...request.crude.delete import Delete
from ...request.crude.exe import Exe


class FunctionalDriverTest(UTCase):

    def setUp(self):

        self.processed = {}

        def process(crude, count):

            def _process(request, **kwargs):

                self.processed.setdefault(crude.name, []).append(kwargs)

                request.ctx['count'] += 10 ** crude.value

                return request

            return [_process for _ in range(count)]

        self.process = process

    def test_default(self):

        kwargs = {}

        count = 1

        for crude in CRUDE.__members__.values():

            kwargs['{0}s'.format(crude.name.lower())] = self.process(
                crude, count
            )
            count += 1

        driver = FunctionalDriver(**kwargs)

        kwargs = {'foo': 'bar'}

        request = Request(
            ctx={'count': 0},
            crudes=[
                Create(None, None), Read(), Update(None, None), Delete(),
                Exe(None)
            ]
        )
        request.ctx['count'] = 0

        result = driver.process(request=request, **kwargs)

        self.assertEqual(result.ctx['count'], 543210)

        count = 1

        for crude in CRUDE.__members__:

            self.assertEqual(len(self.processed[crude]), count)

            processedkwargs = self.processed[crude]

            self.assertEqual(len(processedkwargs), count)

            for processedkwarg in processedkwargs:

                self.assertEqual(processedkwarg['foo'], kwargs['foo'])

            count += 1

if __name__ == '__main__':
    main()