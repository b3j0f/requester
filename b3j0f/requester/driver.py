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

"""Request system definition."""

__all__ = ['Driver']


from enum import IntEnum


class Driver(object):
    """In charge of accessing to data from a request.

    Methods to override are of choice:
    - generic: process
    - specific: create/read/update/delete/run."""

    def process(self, request):
        """Generic method to override in order to crud input data related to
        query, rtype and ctx.

        :param Model query:
        :param str rtype: request type (CREATE, READ, UPDATE, DELETE, RUN).
        :param dict ctx: request context execution.
        :param tuple data: data to process.
        :return: request result.
        """

        raise NotImplementedError()


class CompositeDriver(Driver):

    def __init__(self, drivers, *args, **kwargs):

        super(CompositeDriver, self).__init__(*args, **kwargs)

        self.drivers = drivers

    def process(self, request, rtype, data):

        result = None

        self._process_filter(request)

        if rtype == RTYPE.CREATE:
            pass

        elif rtype == RTYPE.READ:
            pass

        elif rtype == RTYPE.UPDATE:
            pass

        elif rtype == RTYPE.DELETE:
            pass

        elif rtype == RTYPE.RUN:
            pass

        return result

    def getdriver(self, name):

        mnames = name.split('.')

        return self.drivers.get(mnames[0])

    def _process_filter(expr, ctx, _lexpr=None, _ldriver=None):

        ctx = request._ctx

        driver = self.getdriver(expr.name)