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

"""base module."""

__all__ = ['BaseElement']

from uuid import uuid4


class BaseElement(object):

    __slots__ = ['alias', 'uuid']

    def __init__(self, alias=None, uuid=None, *args, **kwargs):
        """
        :param str alias: alias name. Default is None.
        """

        super(BaseElement, self).__init__(*args, **kwargs)

        self.alias = alias
        self.uuid = str(uuid or uuid4())

    def as_(self, alias):
        """Set alias value.

        :param str alias: alias to use.
        :rtype: BaseElement
        :return: this."""

        self.alias = alias

        return self

    @property
    def ctxname(self):
        """Get ctx name to store result execution.

        :rtype: str"""

        return self.alias or getattr(self, 'name', str(self.uuid))

    def __eq__(self, other):

        return isinstance(other, BaseElement) and other.ctxname == self.ctxname
