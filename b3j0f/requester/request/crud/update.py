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

"""update module."""

from .base import CRUDElement

__all__ = ['Update']


class Update(CRUDElement):

    __slots__ = ['name', 'values', 'upsert'] + CRUDElement.__slots__

    def __init__(self, values, name='', upsert=False, *args, **kwargs):
        """
        :param str name: model name.
        :param dict values: values to update.
        :param bool upsert: if True (False by default), tries to create an
            the related object if the query is empty.
        """

        super(Update, self).__init__(*args, **kwargs)

        self.name = name
        self.values = values
        self.upsert = upsert

    def __repr__(self):

        result = 'UPDATE '

        if self.name:
            result += '{0}:'.format(repr(self.name))

        result += repr(self.values)

        if self.upsert:
            result += ' (upsert)'

        if self.query:
            result += ' where ({0})'.format(repr(self.query))

        if self.alias:
            result += ' as {0}'.format(self.alias)

        return result
