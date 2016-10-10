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

from b3j0f.utils.path import lookup

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

from ..request.base import BaseElement
from ..request.consts import FuncName, CONDITIONS
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

from .utils import FunctionChooser

__all__ = [
    'PyDriver', 'processcrud', 'processquery',
    'create', 'read', 'update', 'delete', 'applyfunction', 'FunctionChooser'
]

soundex = getInstance().soundex

DATETIMEFORMAT = '%Y-%m-%d %H:%M:%S'


class CustomDriver(Driver):

    def __init__(self, functionchooser, *args, **kwargs):

        super(CustomDriver, self).__init__(*args, **kwargs)

        self.functionchooser = functionchooser

    def _process(self, transaction, **kwargs):

        result = transaction

        if transaction.state is State.COMMITTING:

            for crud in transaction.cruds:

                self.processcrud(ctx=transaction.ctx, crud=crud, **kwargs)

        return result

    def processcrud(self, crud, ctx=None, **kwargs):
        """Apply the right rule.

        :param CRUDElement crud: crud rule to apply.
        :param Context ctx: context where to store the processing result.
            Default is None.

        :rtype: list
        :return: list"""

        if ctx is None:
            ctx = Context()

        result = items

        if crud in ctx:
            result = ctx[crud]

        else:
            if isinstance(crud, Create):
                result = self.processcreate(create=crud, ctx=ctx, **kwargs)

            elif isinstance(crud, Read):
                result = self.processread(read=crud, ctx=ctx, **kwargs)

            elif isinstance(crud, Update):
                result = self.processupdate(update=crud, ctx=ctx, **kwargs)

            elif isinstance(crud, Delete):
                result = self.processdelete(delete=crud, ctx=ctx, **kwargs)

            ctx[crud] = result

        return result

    def processquery(self, query, ctx=None, **kwargs):
        """Process input query related to items and ctx."""

        result = query

        if ctx is None:
            ctx = Context()

        if query in ctx:  # is query calculated already ?
            result = ctx[query]

        elif isinstance(query, BaseElement):

            if isinstance(query, Function):

                self.processfunction(function=query, ctx=ctx, **kwargs)

            elif isinstance(query, Expression):

                self.processexpr(epr=query, ctx=ctx, **kwargs)

            elif isinstance(query, CRUDElement):

                result = self.processcrud(crud=query, ctx=ctx, **kwargs)

            ctx[query] = result

        return result

    def processfunction(self, function, ctx=None, **kwargs):

        isor = query.name == FuncName.OR

        if isor:
            result = []

        pqueries = []

        for param in query.params:
            fitems = list(items) if isor else items
            pquery = self.processquery(
                query=param, items=fitems, ctx=ctx, **kwargs
            )
            pqueries.append(pquery)

            if isor:
                result += pquery

        if query.name not in [FuncName.AND.value, FuncName.OR.value]:

            function = self.getfunction(name=query.name)

            if function is None:
                raise NotImplementedError(
                    'Function {0} is not implented by {1}'.format(query, self)
                )

            if fparams is None:
                fparams = [ctx.get(param, param) for param in query.params]

            if fparams:
                result = function(query=query, params=params, ctx=ctx)

        return result

    def processexpr(self, expr, items, ctx=None):

        result = []

        for item in items:
            try:
                getsubitem(item, query.name, error=True)

            except KeyError:
                pass

            else:
                result.append(item)

        if not result:
            try:
                result = lookup(query.name)

            except ImportError:
                items[:] = result
                pass

        else:
            items[:] = result

    def processcreate(self, create, ctx=None, **kwargs):
        """Apply input Create element to items.

        :param Create create: data to add to input items.
        :return: created item.
        :rtype: list"""

        if ctx is None:
            ctx = Context()

        items = self.processquery(query=create.query, ctx=ctx, **kwargs)

        values = {}

        for key, query in iteritems(create.values):

            value = self.processquery(query=query, ctx=ctx, **kwargs)
            values[key] = value

        items.append(values)

        return items

    def processread(self, read, ctx=None, **kwargs):
        """Return application of input Read to items.

        :param list items: items to read.
        :param Read read: read resource to apply on items.
        :return: read list.
        :rtype: list
        """

        if ctx is None:
            ctx = Context()

        items = self.processquery(query=read.query, ctx=ctx, **kwargs)

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

    def processupdate(self, update, ctx=None, **kwargs):
        """Apply update to items.

        :param Update update: update rule.
        :return: updated items.
        :rtype: list"""

        if ctx is None:
            ctx = Context()

        items = self.processquery(query=update.query, ctx=ctx, **kwargs)

        values = {}

        for key in list(update.values):

            value = self.processquery(
                items=items, query=update.values[key], ctx=ctx, **kwargs
            )
            values[key] = value

        for item in items:
            for name, value in iteritems(values):

                if callable(value):
                    value(name, item)

                else:
                    item[name] = value

        return items

    def processdelete(self, delete, ctx=None, **kwargs):
        """Apply deletion rule to items.

        :param list items: items to modify.
        :param Delete delete: deletion rule.
        :rtype: list
        :return: modified/deleted items."""

        if ctx is None:
            ctx = Context()

        items = processquery(query=delete.query, ctx=ctx, **kwargs)

        if delete.names:

            names = [
                self.processquery(query=name, ctx=ctx, items=items, **kwargs)
                for name in delete.names
            ]

            for name in names:
                for item in items:
                    if name in item:
                        del item[name]

        else:
            items[:] = []

        return items


class PyDriver(CustomDriver):
    """In charge of accessing to data from a list of dictionaries or objects."""

    version = '0.1'
    name = 'py'  # driver name

    def __init__(self, values=None, *args, **kwargs):
        """
        :param list values: list of data. Data are dictionaries. Default is [].
        """

        super(PyDriver, self).__init__(*args, **kwargs)

        self.values = [] if values is None else values


_GLOBAL_PYDRIVER = PyDriver()


def processcreate(items, create, ctx=None):
    """Apply input Create element to items.

    :param list items: items to process with input Create.
    :param Create create: data to add to input items.
    :return: created item.
    :rtype: list"""

    return _GLOBAL_PYDRIVER.processcreate(create=create, ctx=ctx, items=items)


def processread(items, read, ctx=None):
    """Return application of input Read to items.

    :param list items: items to read.
    :param Read read: read resource to apply on items.
    :return: read list.
    :rtype: list
    """

    return _GLOBAL_PYDRIVER.processread(read=read, ctx=ctx, items=items)


def processupdate(update, ctx=None):

    return _GLOBAL_PYDRIVER.processupdate(update=update, ctx=ctx)


def processdelete(delete, ctx=None):

    return _GLOBAL_PYDRIVER.processdelete(delete=delete, ctx=ctx)


def processcrud(crud, ctx=None):

    return _GLOBAL_PYDRIVER.processcrud(crud=crud, ctx=ctx)


def exists(query, item, name, fparams, ctx):

    result = True

    names = name.split('.')

    val = item

    for name in names:
        if isinstance(val, dict):

            if name in val:
                val = val[name]
                continue

        elif hasattr(val, name):
            val = getattr(val, name)
            continue

        result = False
        break

    return result


def isnull(query, item, name, fparams, ctx):

    try:
        val = getsubitem(item, name, error=True)

    except KeyError:
        return False

    else:
        return val is None


def _namedelt(operator):

    def result(query, item, name, fparams, ctx):

        subitem = getsubitem(item, name)

        return operator(subitem, *fparams[1:])

    return result


def all_(query, item, name, fparams, ctx):

    items = fparams[2]

    result = len(items) > 0

    operator = query.params[1]

    if isinstance(operator, Expression):
        operator = operator.name

    func = _OPERATORS_BY_NAME[operator]

    for _item in items:
        if not func(query.params[0], item, name, [items, _item], ctx):
            result = False
            break

    return result


def any_(query, item, name, fparams, ctx):

    items = fparams[2]

    result = False

    operator = query.params[1]

    if isinstance(operator, Expression):
        operator = operator.name

    func = _OPERATORS_BY_NAME[operator]

    for _item in items:
        if func(query.params[0], item, name, [items, _item], ctx):
            result = True
            break

    return result


_OPERATORS_BY_NAME = {
    FuncName.LT.value: _namedelt(lt),
    FuncName.LE.value: _namedelt(le),
    FuncName.EQ.value: _namedelt(eq),
    FuncName.NE.value: _namedelt(ne),
    FuncName.GE.value: _namedelt(ge),
    FuncName.GT.value: _namedelt(gt),
    FuncName.NOT.value: not_,
    FuncName.TRUTH.value: truth,
    FuncName.IS.value: _namedelt(is_),
    FuncName.ISNOT.value: _namedelt(is_not),
    FuncName.ABS.value: abs,
    FuncName.ADD.value: add,
    FuncName.FLOORDIV.value: floordiv,
    FuncName.DIV.value: truediv,
    FuncName.INDEX.value: indexOf,
    FuncName.INVERT.value: invert,
    FuncName.MOD.value: mod,
    FuncName.LIKE.value: _namedelt(lambda val, msg: match(msg, val)),
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
    FuncName.EXISTS.value: exists,
    FuncName.NEXISTS.value:
        lambda query, item, name, *_: not exists(query, item, name, *_),
    FuncName.ISNULL.value: isnull,
    FuncName.BETWEEN.value: _namedelt(lambda val, inf, sup: inf <= val <= sup),
    FuncName.IN.value: _namedelt(lambda x, y: contains(y, x)),
    FuncName.HAVING.value: lambda query, item, name, fparams, ctx: fparams[0],
    FuncName.UNION.value: lambda seq1, seq2: set(seq1) + set(seq2),
    FuncName.INTERSECT.value: lambda seq1, seq2: set(seq1) & set(seq2),
    FuncName.ALL.value: all_,
    FuncName.ANY.value: any_,
    FuncName.SOME.value: any_,
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

    return _GLOBAL_PYDRIVER.processquery(query=query, ctx=ctx)


def getsubitem(item, name, error=False):

    names = name.split('.')

    result = item

    for name in names:
        try:
            result = result[name]

        except KeyError:
            if error:
                raise

            else:
                result = None
                break

    return result

def condoperator(operator):

    def result(query, fparams, ctx):

        items = fparams[0]

        name = query.params[0].name

        return [
            item for item in fparams[0]
            if operator(query, item, name, fparams, ctx)
        ]

    return result


_ENRICHEDOPERATORSBYNAME = {}


for condition in CONDITIONS:
    _ENRICHEDOPERATORSBYNAME[condition] = condoperator(_OPERATORS_BY_NAME[condition])


class PyFunctionChooser(FunctionChooser):

    def getfunction(self, name, *args, **kwargs):

        if name in _ENRICHEDOPERATORSBYNAME:
            return _ENRICHEDOPERATORSBYNAME[name]

        else:
            try:
                return lookup(name)

            except ImportError:
                pass

FUNCTIONCHOOSER = PyFunctionChooser()


def applyfunction(query, ctx, fparams=None, functionchooser=FUNCTIONCHOOSER):

    try:
        function = functionchooser.getfunction(name=query.name)

        if fparams is None:
            fparams = [ctx.get(param, param) for param in query.params]

        if fparams:
            return function(query=query, fparams=fparams, ctx=ctx)

    except KeyError:
        raise NotImplementedError(
            'Function {0} is not implented by {1}'.format(query, self)
        )

    def __repr__(self):

        return 'FunctionChooser({0})'.format(self.functionsbyname)
