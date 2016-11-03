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

"""create module."""

from .base import CRUDElement

__all__ = ['Create']


class Create(CRUDElement):

    __slots__ = ['name', 'values'] + CRUDElement.__slots__

    def __init__(self, values, name='', *args, **kwargs):
        """
        :param str name: model name.
        :param dict values: model value.
        """
        super(Create, self).__init__(*args, **kwargs)

        self.name = name
        self.values = values

    def set(self, key, value):
        """Set create value by field.

        :param str key: field name.
        :param value: field value.
        :return: self.
        :rtype: Create
        """
        self.values[key] = value

        return self

    def __repr__(self):

        result = 'CREATE '

        if self.name:
            result += '{0}:'.format(repr(self.name))

        result += '{0} '.format(repr(self.values))

        if self.query:
            result += 'WHERE {0} '.format(repr(self.query))

        if self.dparams:
            result += 'WITH {0} '.format(repr(self.dparams))

        if self.alias:
            result += 'AS {0}'.format(self.alias)

        if result[-1] == ' ':
            result = result[:-1]

        return result
