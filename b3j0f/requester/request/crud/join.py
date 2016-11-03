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

"""join execution module."""

from enum import IntEnum, unique

from ..expr import Function, Expression

from b3j0f.utils.version import OrderedDict

from six import iteritems, string_types

__all__ = [
    'innerjoin', 'leftjoin', 'rightjoin',
    'crossjoin', 'selfjoin', 'naturaljoin',
    'applyjoin'
]


@unique
class JoinKind(IntEnum):
    """Join kind."""

    INNER = 0  #: inner join.
    LEFT = 1  #: left join.
    LEFTEX = 2  #: left exclusive join.
    RIGHT = 3  #: right exclusive join.
    RIGHTEX = 4  #: right exclusive join.
    FULL = 5  #: full join.
    FULLEX = 6  #: full exclusive join.
    CROSS = 7  #: cross join.
    SELF = 8  #: self join.
    NATURAL = 9  #: natural join.
    UNION = 10  #: union join.


class Join(object):
    """In charge of managing Join."""

    DEFAULT_KIND = 'CROSS'

    __slots__ = ['scope', 'query', 'select', 'kind']

    def __init__(
            self, scope=None, query=None, select=None, kind=DEFAULT_KIND,
            *args, **kwargs
    ):
        """
        :param list scope: scope names. By default, an automatic discovering is
            processed.
        :param Expression query: condition query which apply the join.
        :param list select: list of field names to keep.
        :param str kind: join kind. Default is CROSS.
        """
        super(Join, self).__init__(*args, **kwargs)

        self.scope = scope
        self.query = query
        self.select = select
        self.kind = kind

    def __call__(self, ctx):
        """Apply join query input ctx."""

        datasets = []

        for sco in self.scope:
            name = None

            if isinstance(sco, Function):
                name = sco()

            elif isinstance(sco, Expression):
                name = sco.name

            elif isinstance(sco, string_types):
                name = sco

            else:
                raise TypeError(
                    'Wrong scope type {0}. {1} expected.'.format(
                        sco, string_types + (Expression, )
                    )
                )

            if name in ctx:
                datasets.append(ctx[name])

        return applyjoin(kind=self.kind, query=self.query, ctx=ctx)

    def __repr__(self):

        result = '({0} JOIN {1}'.format(self.kind, self.scope)

        if self.query:
            result += 'ON {0}, '.format(self.query)

        if self.select:
            result += 'USING {0}, '.format(self.select)

        result += ')'

        return result


def applyjoin(kind, query, ctx):
    """Apply join query input ctx."""

    result = None

    joinprocess = getjoin(kind)

    query = self.query

    datasets = getdatasets(expr=query, ctx=ctx)

    names = list(datasets)

    indexes = OrderedDict([(name, 0) for name in names])

    maxpos = len(names) - 1

    namepos = 0

    while True:

        items = OrderedDict()

        name = names[namepos]

        index = indexes[name]

        items[name] = datasets[name][index]

        if namepos == maxpos:

            resjoin = joinprocess(items=items, query=query)

            if resjoin is not None:
                result.append(resjoin)

            index = (index + 1) % len(datasets[name])
            indexes[name] = index

            if index == 0:

                for namepos in range(namepos - 1, -1, -1):

                    name = names[namepos]

                    index = (indexes[name] + 1) % len(datasets[name])

                    indexes[name] = index

                    if index != 0:
                        break

                else:
                    break

        namepos = (namepos + 1) % len(namepos)

    return result


def check(item, query):
    """Check if input query match input items.

    :param OrderedDict items: items where check input query.
    :param Expression query: join query expression.
    :rtype: bool
    """
    return True


def innerjoin(items, query):
    """Apply inner join.

    :param OrderedDict items:
    :param Expression query: where filter.
    :rtype: dict
    """
    result = None

    if check(items):
        result = crossjoin(items=items, query=query)

    return result


def crossjoin(items, query, policy):
    """Apply a cross join on input items and query.

    :param list items: items for several set of data.
    :param Expression query: filtering expression.
    """
    return dict(
        list(iteritems(item)) for item in items
        if check(item=item, query=query)
    )


def selfjoin(items, query):

    return crossjoin(items=[items[0], items[0]], query=query)


def naturaljoin(items, query):

    result = None

    intersection = set()

    for item in items:

        intersection &= set(item)

    if intersection:

        result = {}

        for item in items:
            result.update(item)

    return result


def indexjoin(items, query, index=0):

    return items[index]


def leftjoin(items, query):

    return indexjoin(items=items, query=query)


def rightjoin(items, query):

    return indexjoin(items=items, query=query, index=-1)


def getdatasets(expr, ctx):

    result = {}

    if isinstance(expr, Expression):

            if isinstance(expr, Function):

                for param in expr.params:

                    presult = getdatasets(param, ctx)
                    result.update(presult)

            else:
                name = expr.name.split('.')[0]
                result[name] = ctx[name]

    return result


_JOINBYNAME = {
    JoinKind.INNER.name: innerjoin,
    JoinKind.LEFT.name: leftjoin,
    JoinKind.RIGHT.name: rightjoin,
    JoinKind.CROSS.name: crossjoin,
    JoinKind.SELF.name: selfjoin,
    JoinKind.NATURAL.name: naturaljoin
}
