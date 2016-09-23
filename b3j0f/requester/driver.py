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

from .request import Request

from enum import IntEnum


class RTYPE(IntEnum):

    CREATE = 1  #: creation request type name.
    READ = 2  #: reading request type name.
    UPDATE = 3  #: updating request type name.
    DELETE = 4  #: deletion request type name.
    RUN = 5  #: running request type name.


class Driver(object):
    """In charge of accessing to data from a request.

    Methods to override are of choice:
    - generic: process
    - specific: create/read/update/delete/run."""

    def process(self, request, data):
        """Generic method to override in order to crud input data related to
        filter, rtype and ctx.

        :param Model filter:
        :param str rtype: request type (CREATE, READ, UPDATE, DELETE, RUN).
        :param dict ctx: request context execution.
        :param tuple data: data to process.
        :return: request result.
        """

        raise NotImplementedError()

    def create(self, request, *assignments):
        """Create """

        return self.process(
            request=request, rtype=RTYPE.CREATE, data=assignments
        )

    def read(self, request, selection=None):
        """Read data.

        :param Request request:
        :param Selection selection: selection. All filtered data by default.
        """

        return self.process(request=request, rtype=RTYPE.READ, data=selection)

    def update(self, request, *assignments):

        return self.process(
            request=request, rtype=RTYPE.UPDATE, data=assignments
        )

    def delete(self, request, *models):

        return self.process(request=request, rtype=RTYPE.DELETE, data=models)

    def run(self, request, model):

        return self.process(request=request, rtype=RTYPE.RUN, data=model)

    def __getitem__(self, key):

        return Request(driver=self).__getitem__(key)

    def __setitem__(self, key, item):

        return Request(driver=self).__setitem__(key, item)

    def __delitem__(self, key):

        return Request(driver=self).__delitem__(key)

    def request(self, filter=None, ctx=None):

        return Request(driver=self, filter=filter, ctx=ctx)


class CompositeDriver(Driver):

    def __init__(self, drivers, *args, **kwargs):

        super(CompositeDriver, self).__init__(*args, **kwargs)

        self.drivers = drivers

    def process(self, request, rtype, data):

        result = None

        _process_filter(request)

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

    def _process_filter(model, ctx, _lmodel=None, _ldriver=None):

        ctx = request._ctx

        driver = self.getdriver(model.name)