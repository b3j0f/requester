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

"""Python driver module."""

from .base import Driver

from operator import (
    lt, le, eq, ne, ge, gt, not_, truth, is_, is_not, abs, add, floordiv,
    truediv, invert, mod, mul, neg, or_, pow, rshift, lshift, sub,
    xor, concat, countOf, indexOf, repeat, sequenceIncludes, iadd, iand,
    getitem, setitem, delitem, getslice, setslice, delslice, iconcat,
    ifloordiv, ilshift, imod, imul, ior, ipow, irepeat, irshift, isub,
    itruediv, ixor, contains
)

from re import match

from .ctx import Context
from .transaction import State

from ..request.macro import FuncName
from ..request.expr import Expression, Function
from ..request.crud.create import Create
from ..request.crud.read import Read
from ..request.crud.join import applyjoin, Join
from ..request.crud.update import Update
from ..request.crud.delete import Delete

from random import random

from soundex import getInstance

from md5 import md5

from time import time
from datetime import datetime

from six import iteritems

__all__ = [
    'PyDriver', 'processcrud', 'processquery',
    'create', 'read', 'update', 'delete', 'applyfunction', 'FunctionChooser'
]

soundex = getInstance().soundex

DATETIMEFORMAT = '%Y-%m-%d %H:%M:%S'


class PyDriver(Driver):
    """In charge of accessing to data from a list of dictionaries or objects."""

    version = '0.1'
    name = 'py'  # driver name

    def __init__(self, values=None, *args, **kwargs):
        """
        :param list values: list of data. Data are dictionaries. Default is [].
        """

        super(PyDriver, self).__init__(*args, **kwargs)

        self.values = [] if values is None else values

    def _process(self, transaction, **kwargs):

        if kwargs:
            raise ValueError(
                'Driver {0} does not support additional arguments {1}'.format(
                    self, kwargs
                )
            )

        if transaction.state is not State.COMMITTING:
            return

        for crud in transaction.cruds:

            self._processquery(query=crud.query, ctx=transaction.ctx)

            processcrud(ctx=transaction.ctx, items=self.values, crud=crud)

        return transaction

    def _processquery(self, query, ctx):

        if query in ctx:
            return ctx[query]

        else:
            ctx[query] = query


def create(items, create, ctx=None):
    """Apply input Create element to items.

    :param list items: items to process with input Create.
    :param Create create: data to add to input items.
    :return: created item.
    :rtype: list"""

    values = {}

    for key in list(create.values):

        value = processquery(items=create.values[key], query=name, ctx=ctx)
        values[key] = value

    items.append(values)

    return items


def read(items, read, ctx=None):
    """Return application of input Read to items.

    :param list items: items to read.
    :param Read read: read resource to apply on items.
    :return: read list.
    :rtype: list
    """

    result = list(items)

    if read.select():
        result = []
        for item in list(items):
            fitem = {}
            for sel in read.select():
                if sel in item:
                    fitem[sel] = item[sel]
            result.append(fitem)

    if read.offset():
        result = result[read.offset():]

    if read.limit():
        result = result[:read.limit()]

    if read.orderby():
        for orderby in read.orderby():
            result.sort(key=lambda item: item.get(orderby))

    if read.groupby():
        raise NotImplementedError()
        groupbyresult = {}
        _groupbyresult = []
        for groupby in read.groupby():
            if _groupbyresult:
                for item in _groupbyresult:
                    pass
            _groupbyresult = {groupby: []}

            for res in result:
                if groupby in res:
                    groupbyresult[groupby] = res.pop(groupby)

            #FIX: do the same for sub groupby...

    if read.join() not in ('FULL', None):
        raise NotImplementedError(
            'read function does not support join {0}'.format(
                read.join()
            )
        )

    return result


def update(items, update, ctx=None):
    """Apply update to items.

    :param list items: items to update.
    :param Update update: update rule.
    :return: updated items.
    :rtype: list"""

    values = {}

    for key in list(update.values):

        value = processquery(items=items, query=update.values[key], ctx=ctx)
        values[key] = value

    for item in items:
        for name, value in iteritems(values):

            if callable(value):
                value(name, item)

            else:
                item[name] = value

    return items


def delete(items, delete, ctx=None):
    """Apply deletion rule to items.

    :param list items: items to modify.
    :param Delete delete: deletion rule.
    :rtype: list
    :return: modified/deleted items."""

    if delete.names:

        names = [
            processquery(query=name, ctx=ctx, items=items)
            for name in delete.names
        ]

        for name in names:
            for item in items:
                if name in item:
                    del item[name]

    else:
        items[:] = []

    return items


def processcrud(items, crud, ctx=None):
    """Apply the right rule on input items.

    :param list items: items to process.
    :param CRUDElement crud: crud rule to apply.
    :param Context ctx: context where to store the processing result. Default is
        None.

    :rtype: list
    :return: list"""

    if ctx is None:
        ctx = Context()

    if isinstance(crud, Create):
        processresult = create(items=items, create=crud, ctx=ctx)

    elif isinstance(crud, Read):
        processresult = read(items=items, read=crud, ctx=ctx)

    elif isinstance(crud, Update):
        processresult = update(items=items, update=crud, ctx=ctx)

    elif isinstance(crud, Delete):
        processresult = delete(items=items, delete=crud, ctx=ctx)

    ctx[crud] = processresult

    return items


_OPERATORS_BY_NAME = {
    FuncName.LT.value: lt,
    FuncName.LE.value: le,
    FuncName.EQ.value: eq,
    FuncName.NE.value: ne,
    FuncName.GE.value: ge,
    FuncName.GT.value: gt,
    FuncName.NOT.value: not_,
    FuncName.TRUTH.value: truth,
    FuncName.IS.value: is_,
    FuncName.ISNOT.value: is_not,
    FuncName.ABS.value: abs,
    FuncName.ADD.value: add,
    FuncName.FLOORDIV.value: floordiv,
    FuncName.DIV.value: truediv,
    FuncName.INDEX.value: indexOf,
    FuncName.INVERT.value: invert,
    FuncName.MOD.value: mod,
    FuncName.LIKE.value: match,
    FuncName.MUL.value: mul,
    FuncName.NEG.value: neg,
    FuncName.OR.value: or_,
    FuncName.POW.value: pow,
    FuncName.RSHIFT.value: rshift,
    FuncName.LSHIFT.value: lshift,
    FuncName.SUB.value: sub,
    FuncName.XOR.value: xor,
    FuncName.CONCAT.value: concat,
    FuncName.COUNTOF.value: countOf,
    FuncName.REPEAT.value: repeat,
    FuncName.INCLUDE.value: sequenceIncludes,
    FuncName.IADD.value: iadd,
    FuncName.IAND.value: iand,
    FuncName.IOR.value: ior,
    FuncName.IXOR.value: ixor,
    FuncName.GETITEM.value: getitem,
    FuncName.SETITEM.value: setitem,
    FuncName.DELITEM.value: delitem,
    FuncName.GETSLICE.value: getslice,
    FuncName.SETSLICE.value: setslice,
    FuncName.DELSLICE.value: delslice,
    FuncName.ICONCAT.value: iconcat,
    FuncName.IDIV.value: itruediv,
    FuncName.IFLOORDIV.value: ifloordiv,
    FuncName.ILSHIFT.value: ilshift,
    FuncName.IMOD.value: imod,
    FuncName.IMUL.value: imul,
    FuncName.IPOW.value: ipow,
    FuncName.IREPEAT.value: irepeat,
    FuncName.IRSHIFT.value: irshift,
    FuncName.ISUB.value: isub,
    FuncName.COUNT.value: len,
    FuncName.LENGTH.value: len,
    FuncName.AVG.value: lambda v: sum(v) / (len(v) or 1),
    FuncName.MEAN.value: lambda v: sum(v) / (len(v) or 1),
    FuncName.MAX.value: max,
    FuncName.MIN.value: min,
    FuncName.SUM.value: sum,
    FuncName.EXISTS.value: contains,
    FuncName.ISNULL.value: lambda data: data is None,
    FuncName.BETWEEN.value: lambda data, inf, sup: inf <= data <= sup,
    FuncName.IN.value: contains,
    FuncName.HAVING.value: contains,
    FuncName.UNION.value: lambda seq1, seq2: set(seq1) + set(seq2),
    FuncName.INTERSECT.value: lambda seq1, seq2: set(seq1) & set(seq2),
    FuncName.ALL.value: all,
    FuncName.ANY.value: any,
    FuncName.VERSION.value: PyDriver.version,
    FuncName.CONCAT.value: str.__add__,
    FuncName.ICONCAT.value: str.__add__,
    FuncName.REPLACE.value: str.replace,
    FuncName.SOUNDEX.value: soundex,
    FuncName.SUBSTRING.value: lambda data, start, end=None: data[start:end],
    FuncName.LEFT.value: lambda data, count: str[:-count],
    FuncName.RIGHT.value: lambda data, count: str[count:],
    FuncName.REVERSE.value: reversed,
    FuncName.TRIM.value: str.strip,
    FuncName.LTRIM.value: str.lstrip,
    FuncName.RTRIM.value: str.rstrip,
    FuncName.LPAD.value: str.ljust,
    FuncName.RPAD.value: str.rjust,
    FuncName.UPPER.value: str.upper,
    FuncName.LOWER.value: str.lower,
    FuncName.UCASE.value: str.upper,
    FuncName.LCASE.value: str.lower,
    FuncName.LOCATE.value: lambda val, data, *args: str.find(
        data, val, *args
    ) + 1,
    FuncName.INSTR.value: lambda val, data, *args: str.find(
        data, val, *args
    ) + 1,
    FuncName.RAND.value: random,
    FuncName.ROUND.value: round,
    FuncName.MD5.value: lambda data: md5(data).digest(),
    FuncName.NOW.value: lambda: datetime.now().strftime(DATETIMEFORMAT),
    FuncName.SEC_TO_TIME.value: lambda date: datetime.fromtimestamp(date).strftime(DATETIMEFORMAT),
    FuncName.DATEDIFF.value: lambda date1, date2: datetime.strpformat(date1, DATETIMEFORMAT) - datetime.strpformat(date2, DATETIMEFORMAT),
    FuncName.MONTH.value: lambda date=None: datetime.strpformat(date, DATETIMEFORMAT).month,
    FuncName.YEAR.value: lambda date=None: datetime.strpformat(date, DATETIMEFORMAT).year,
}


def processquery(query, items, ctx=None):
    """Process input query related to items and ctx."""

    result = query

    if ctx is None:
        ctx = Context()

    if query not in ctx:  # is query calculated already ?

        if isinstance(query, Function):

            isor = query.name == FuncName.OR

            if isor:
                result = []

            pqueries = []
            for param in query.params:

                fitems = list(items) if isor else items

                pquery = processquery(query=param, items=fitems, ctx=ctx)
                pqueries.append(pquery)

                if isor:
                    result += pquery

            if query.name not in [FuncName.AND, FuncName.OR]:

                result = applyfunction(
                    query=query, ctx=ctx, params=params, fparams=pqueryies
                )

        elif isinstance(query, Expression):
            result = items

    ctx[query] = result

    return result


_CONDFUNCTIONNAMES = [
    FuncName.LT.value,
    FuncName.LE.value,
    FuncName.EQ.value,
    FuncName.NE.value,
    FuncName.GE.value,
    FuncName.GT.value,
    FuncName.NOT.value,
    FuncName.TRUTH.value,
    FuncName.IS.value,
    FuncName.ISNOT.value,
    FuncName.COUNT.value,
    FuncName.LENGTH.value,
    FuncName.AVG.value,
    FuncName.MEAN.value,
    FuncName.MAX.value,
    FuncName.MIN.value,
    FuncName.SUM.value,
    FuncName.EXISTS.value,
    FuncName.ISNULL.value,
    FuncName.BETWEEN.value,
    FuncName.IN.value,
    FuncName.HAVING.value,
    FuncName.UNION.value,
    FuncName.INTERSECT.value,
    FuncName.ALL.value,
    FuncName.ANY.value,
    FuncName.COUNTOF.value,
    FuncName.LIKE.value,
    FuncName.INCLUDE.value,
    FuncName.SOUNDEX.value,
]


def condoperator(operator):

    def result(query, params, fparams, ctx):

        return [
            item for item in fparams[0]
            if operator(item[params[0].name], *fparams[1:])
        ]

    return result


for name in _CONDFUNCTIONNAMES:
    _OPERATORS_BY_NAME[name] = condoperator(_OPERATORS_BY_NAME[name])


class FunctionChooser(object):

    def __init__(self, functionsbyname, *args, **kwargs):

        super(FunctionChooser, self).__init__(*args, **kwargs)

        self.functionsbyname = functionsbyname

    def applyfunction(self, query, ctx, params, fparams=None):

        try:
            function = self.functionsbyname[query.name]

            if fparams is None:
                fparams = [ctx.get(param, param) for param in params]

            return function(query=query, params=params, fparams=fparams, ctx=ctx)

        except KeyError:
            raise NotImplementedError(
                'Function {0} is not implented by {1}'.format(query, self)
            )

    def __repr__(self):

        return 'FunctionChooser({0})'.format(self.functionsbyname)


_PYFUNCTIONCHOOSER = FunctionChooser(functionsbyname=_OPERATORS_BY_NAME)


def applyfunction(query, ctx, params, fparams):

    return _PYFUNCTIONCHOOSER.applyfunction(query, ctx, params, fparams)
