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

from copy import deepcopy

from enum import IntEnum, unique

from ..expr import Function, Expression
from ..consts import FuncName

from b3j0f.utils.version import OrderedDict

from six import iteritems

__all__ = [
    'innerjoin', 'leftjoin', 'leftexjoin', 'rightjoin', 'rightexjoin',
    'fulljoin', 'fullexjoin', 'crossjoin', 'selfjoin', 'naturaljoin',
    'unionjoin', 'applyjoin'
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

    __slots__ = ['on', 'using', 'kind']

    def __init__(
            self, on=None, using=None, kind=DEFAULT_KIND, *args, **kwargs
    ):
        """
        :param Expression on: condition on which apply the join.
        :param list using: list of field names to keep.
        :param str kind: join kind. Default is CROSS.
        """
        super(Join, self).__init__(*args, **kwargs)

        self.on = on
        self.using = using
        self.kind = kind

    def __call__(self, ctx):
        """Apply join on input ctx."""

        return applyjoin(kind=self.kind, on=self.on, ctx=ctx)

    def __repr__(self):

        result = 'Join('

        if self.on:
            result += 'on={0}, '.format(self.join)

        if self.using:
            result += 'using={0}, '.format(self.using)

        if self.kind:
            result += 'kind={0}'.format(self.kind)

        result += ')'

        return result


def applyjoin(kind, on, ctx):
    """Apply join on input ctx."""

    result = None

    joinprocess = getjoin(kind)

    on = self.on

    datasets = getdatasets(expr=on, ctx=ctx)

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

            resjoin = joinprocess(items=items, on=on)

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


def checkon(items, on):
    """Check if input on match input items.

    :param OrderedDict items: items where check input on.
    :param Expression on: join on expression.
    :rtype: bool
    """
    return True


def innerjoin(items, on):
    """Apply inner join.

    :param OrderedDict items:
    :param Expression on: where filter.
    :rtype: dict
    """
    result = None

    if checkon(items):
        result = crossjoin(items=items, on=on)

    return result


def crossjoin(items, on):

    return dict(list(iteritems(item)) for item in items)


def selfjoin(items, on):

    return crossjoin(items, on)


def naturaljoin(items, on):

    result = None

    intersection = set()

    for item in items:

        intersection &= set(item)

    if intersection:

        result = {}

        for item in items:
            result.update(item)

    return result


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


def innerjoin(lfield, rfield, litems, ritems):

    result = []

    for litem in litems:

        if lfield in litem:
            lvalue = litem[lfield]

            for ritem in ritems:
                if rfield in ritem:
                    rvalue = ritem[rfield]

                    if rvalue == lvalue:

                        item = deepcopy(litem)
                        item.update(ritem)
                        result.append(item)

    return result


def leftjoin(lfield, rfield, litems, ritems):

    return litems


def leftexjoin(lfield, rfield, litems, ritems):

    result = []

    for litem in litems:
        if lfield in litem:
            lvalue = litem[lfield]

            for ritem in ritems:
                if rfield in ritem:
                    rvalue = ritem[rfield]

                    if rvalue != lvalue:
                        item = deepcopy(litem)
                        item.update(ritem)
                        result.append(item)

    return result


def rightjoin(lfield, rfield, litems, ritems):

    return ritems


def rightexjoin(lfield, rfield, litems, ritems):

    return leftexjoin(rfield, lfield, ritems, litems)


def fulljoin(lfield, rfield, litems, ritems):
    """Apply full join on litems and rtimes.

    :param list litems:
    :param list ritmes:
    :return: new list of items.
    :rtype: list"""

    result = []

    for litem in litems:

        for ritem in ritems:
            pass

    return litems + [item for item in ritems if item not in litems]


def fullexjoin(lfield, rfield, litems, ritems):

    return leftexjoin(litems, ritems) + rightexjoin(litems, ritems)


def crossjoin(lfield, rfield, litems, ritems):

    result = []

    for litem in litems:

        for ritems in ritems:
            item = deepcopy(litems)
            item.update(ritem)
            result.append(item)

    return result


def selfjoin(lfield, rfield, litems, ritems):

    return crossjoin(lfield, rfield, litems, litems)


def naturaljoin(lfield, rfield, litems, ritems):

    result = []

    for litem in litems:

        lkeys = set(litem)

        for ritem in ritems:

            rkeys = set(ritem)

            intersection = lkeys & rkeys

            if intersection:

                item = {}

                for key in intersection:

                    if litem[key] != ritem[key]:

                        break

                    item[key] = litem[key]

                else:
                    item.update(litem)
                    item.update(ritem)
                    result.append(item)

    return result


def unionjoin(lfield, rfield, litems, ritems):

    return litems + ritems


_JOINBYNAME = {
    JoinKind.INNER.name: innerjoin,
    JoinKind.LEFT.name: leftjoin,
    JoinKind.LEFTEX.name: leftexjoin,
    JoinKind.RIGHT.name: rightjoin,
    JoinKind.RIGHTEX.name: rightexjoin,
    JoinKind.FULL.name: fulljoin,
    JoinKind.FULLEX.name: fullexjoin,
    JoinKind.CROSS.name: crossjoin,
    JoinKind.SELF.name: selfjoin,
    JoinKind.NATURAL.name: naturaljoin,
    JoinKind.UNION.name: unionjoin
}


def applyjoin(on, ctx, litems, ritems, join=JoinKind.INNER.name):

    if isinstance(join, Join):
        join = join.name

    func = _JOINBYNAME[join]

    return func(None, None, litems, ritems)


def applyjoinonquery(join, query, ctx):

    result = []

    if isinstance(join, Function):

        if join.name == (FuncName.AND.value, FuncName.OR.value):

            if join.name == FuncName.AND.value:
                result = [[]]

            for param in join.params:
                exprquery = applyjoinonquery(param, query, ctx)

            if join.name == FuncName.AND.value:
                result[-1] += exprquery

            else:
                result.append(exprquery)

        elif join.name == (FuncName.EQ.value, FuncName.NEQ.value):

            if type(join.params[1]) is Expression:

                names = join.params[1].name

                val2cmp = None

                for ran in len(names):

                    name = '.'.join(names[:ran])

                    if name in ctx:

                        data = ctx[name]

                        prop = '.'.join(names[ran:])

                        val2cmp = [item[prop] for item in data if prop in item]

                        if join.name == FuncName.EQ.value:
                            funcname = FuncName.IN.value

                        else:
                            funcname = FuncName.NIN.value

                        result = [
                            Function(
                                name=funcname, params=[join.params[0], val2cmp]
                            )
                        ]

    return result
