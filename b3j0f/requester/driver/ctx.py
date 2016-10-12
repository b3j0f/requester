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

"""Context module."""

from collections import Hashable, Iterable

from six import iteritems

from ..request.base import BaseElement
from ..request.crud.join import Join, applyjoin

__all__ = ['Context', 'getctxname']


def getctxname(obj):
    """Get context name from input obj.

    :param obj: object from where get a context name.
    :return: obj context name.
    :rtype: str
    """
    result = obj

    if obj is not None:

        if isinstance(obj, BaseElement):
            result = obj.ctxname

        elif not isinstance(obj, Hashable):

            if isinstance(obj, Iterable):
                keys = sorted(list(obj))

                if isinstance(obj, dict):
                    _obj = [
                        (key, getctxname(obj[key])) for key in keys
                    ]

                else:
                    _obj = [getctxname(key) for key in keys]

                result = hash(str(_obj))

            else:
                result = id(obj)

    return result


class Context(dict):
    """Request execution context."""

    def __getitem__(self, key):

        ctxname = getctxname(key)

        return super(Context, self).__getitem__(ctxname)

    def __setitem__(self, key, value):

        ctxname = getctxname(key)

        return super(Context, self).__setitem__(ctxname, value)

    def __delitem__(self, key):

        ctxname = getctxname(key)

        return super(Context, self).__delitem__(ctxname)

    def __contains__(self, key):

        ctxname = getctxname(key)

        return super(Context, self).__contains__(ctxname)

    def get(self, key, default=None):

        ctxname = getctxname(key)

        return super(Context, self).get(ctxname, default)

    def pop(self, key, default=None):

        ctxname = getctxname(key)

        return super(Context, self).pop(ctxname, default)

    def setdefault(self, key, default):

        ctxname = getctxname(key)

        return super(Context, self).setdefault(ctxname, default)

    def fill(self, ctx, join=Join.FULL):
        """Fill this content with ctx data not in this data.

        :param Context ctx: ctx context from where get items."""

        if isinstance(ctx, Context):
            for key, value in iteritems(ctx):

                if key in list(self):
                    self[key] = applyjoin(self[key], value)

                else:
                    self[key] = ctx[key]

                dotkey = '.{0}'.format(key)

                for key in list(self):
                    if key.endswith(dotkey):
                        self[key] = applyjoin(self[key], value)

        return self
