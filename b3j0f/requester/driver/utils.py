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

"""Python utilities driver module."""

from six import iteritems

from ..request.crud.base import CRUDElement
from ..request.crud.create import Create
from ..request.crud.read import Read
from ..request.crud.update import Update
from ..request.crud.delete import Delete
from ..request.expr import Function

__all__ = ['FunctionChooser', 'SEPARATOR']


class FunctionChooser(object):

    def getfunction(self, name):

        raise NotImplementedError()


SEPARATOR = '.'  #: name separator.


def getnames(name):
    """get resource names from input name.

    :param str name: resource name from where get hierarchy of names.
    """

    return name.split(SEPARATOR)


def getchildren(elt):
    """Get children element from the request tree.

    :param BaseElement elt: element from where get children.
    :rtype: list
    """
    result = []

    if isinstance(elt, Function):

        result = elt.params

    elif isinstance(elt, CRUDElement):

        result.append(elt.query)

        if isinstance(elt, (Create, Update)):

            result.append(elt.name)

            for name, value in iteritems(elt.values):
                result.append(name)
                result.append(value)

        elif isinstance(elt, Read):

            result += elt.select()
            result += elt.orderby()
            result.append(elt.join())
            result.append(elt.limit())
            result.append(elt.offset())
            result.append(elt.groupby())

        elif isinstance(elt, Delete):

            result += elt.names()

    return result
