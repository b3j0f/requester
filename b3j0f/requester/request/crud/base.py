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

"""base crud module."""

__all__ = ['CRUD']

from ..expr import BaseElement

from enum import IntEnum, unique


@unique
class CRUD(IntEnum):

    CREATE = 1
    READ = 2
    UPDATE = 3
    DELETE = 4


class CRUDElement(BaseElement):
    """Base crud operation.

    Can be associated to a request."""

    __slots__ = ['request', 'result'] + BaseElement.__slots__

    def __init__(self, request=None, result=None, *args, **kwargs):
        """
        :param b3j0f.requester.Request request:
        :param result: result of this crud processing.
        """

        super(CRUDElement, self).__init__(*args, **kwargs)

        self.request = request
        self.result = result

    def __call__(self):
        """Execute this CRUD element.

        :return: this execuion result."""

        if self.request is None:
            raise RuntimeError(
                'Impossible to execute this without associate it to a request.'
            )

        else:
            return self.request.processcrud(self)
