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


class Driver(object):
    """In charge of accessing to data from a request.

    Methods to override are of choice:
    - generic: process
    - specific: create/read/update/delete/exe."""

    name = None  # driver name

    def __init__(self, name=None, *args, **kwargs):

        super(Driver, self).__init__(*args, **kwargs)

        if name is not None:
            self.name = name

    def process(self, request, explain=False, **kwargs):
        """Generic method to override in order to cruder input data related to
        query, rtype and ctx.

        :param Request request: request to process.
        :param bool explain: give additional information about the request
            execution.
        :param dict kwargs: additional parameters specific to the driver.
        :return: request.
        """

        raise NotImplementedError()


class CustomDriver(Driver):
    """Driver with fine grained implementation of cruder functions.

    This driver uses at most five functions for five respective cruder types.

    The processing execute the right function for all request cruder objects.

    Functions must takes in parameters a 'cruder' object, a 'request' object and
    kwargs for specific driver uses (like explain for example).
    Function result must be a request."""

    def __init__(
            self, create=None, read=None, update=None, delete=None, exe=None,
            *args, **kwargs
    ):
        """
        :param create: creation function.
        :param read: reading function.
        :param update: updating function.
        :param delete: deletion function.
        :param exe: exening function.
        """

        super(CustomDriver, self).__init__(*args, **kwargs)

        self.create = create
        self.read = read
        self.update = update
        self.delete = delete
        self.exe = exe

    def process(self, request, **kwargs):

        result = request

        for cruder in request.crudes:

            crudename = type(cruder).__name__.lower()

            func = getattr(self, crudename)

            if func is not None:
                result = func(cruder=cruder, request=result, **kwargs)

        return result
