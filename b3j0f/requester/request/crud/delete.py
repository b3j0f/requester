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

"""delete module."""

from six import iteritems

from .base import CRUDElement

__all__ = ['Delete']


class Delete(CRUDElement):

    __slots__ = ['_names'] + CRUDElement.__slots__

    def __init__(self, names=None, *args, **kwargs):
        """
        :param list names: model names to delete.
        """
        super(Delete, self).__init__(*args, **kwargs)

        self._names = () if names is None else names

    def names(self, *values):

        if values:
            self._names = values
            result = self

        else:
            result = self._names

        return result

    def __repr__(self):

        if self.names:
            names = [repr(name) for name in self._names]
            names = ', '.join(names)

        else:
            names = 'ALL'

        result = 'DELETE {0} '.format(names)

        if self.query:
            result += 'WHERE {0} '.format(repr(self.query))

        if self.dparams:
            result += 'WITH '

            dparams = []
            for name, value in iteritems(self.dparams):
                dparam = '{0}'.format(name)

                if value is not True:
                    dparam += ': {0}'.format(value)

                dparams.append(dparam)

            result += '{0} '.format(', '.join(dparams))

        if self.alias:
            result += 'AS {0}'.format(self.alias)

        if result[-1] == ' ':
            result = result[:-1]

        return result
