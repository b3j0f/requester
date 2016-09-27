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

__all__ = ['FunctionalDriver']

from .base import Driver
from ..request.core import Request


class FunctionalDriver(Driver):
    """Driver with fine grained implementation of crude functions.

    This driver uses at most five set of functions for five respective crude
    types.

    The processing execute the right function for all request crude objects.

    Functions must takes in parameters a 'crude' object, a 'request' object and
    kwargs for specific driver uses (like explain for example).
    Function result must be a request."""

    def __init__(
            self,
            creates=None, reads=None, updates=None, deletes=None, exes=None,
            *args, **kwargs
    ):
        """
        :param list creates: creation functions.
        :param list reads: reading functions.
        :param list updates: updating functions.
        :param list deletes: deletion functions.
        :param list exes: execution functions.
        """

        super(FunctionalDriver, self).__init__(*args, **kwargs)

        self.creates = creates or []
        self.reads = reads or []
        self.updates = updates or []
        self.deletes = deletes or []
        self.exes = exes or []

    def process(self, request, **kwargs):

        result = request

        for crude in request.crudes:

            crudename = type(crude).__name__.lower()

            funcs = getattr(self, '{0}s'.format(crudename))

            for func in funcs:
                result = func(crude=crude, request=result, **kwargs)

        return result
