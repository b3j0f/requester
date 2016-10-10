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

from functools import wraps

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
    'processcreate', 'processread', 'processupdate', 'processdelete',
    'applyfunction', 'FunctionChooser'
]

VERSION = '0.1'  #: python driver version.

soundex = getInstance().soundex

DATETIMEFORMAT = '%Y-%m-%d %H:%M:%S'


def processcreate(items, create, ctx=None, **kwargs):
    """Apply input Create element to items.

    :param list items: items to process with input Create.
    :param Create create: data to add to input items.
    :return: created item.
    :rtype: list"""

    return _GLOBALPYDRIVER.processcreate(
        create=create, ctx=ctx, items=items, **kwargs
    )


def processread(items, read, ctx=None, **kwargs):
    """Return application of input Read to items.

    :param list items: items to read.
    :param Read read: read resource to apply on items.
    :return: read list.
    :rtype: list
    """

    return _GLOBALPYDRIVER.processread(
        read=read, ctx=ctx, items=items, **kwargs
    )


def processupdate(items, update, ctx=None, **kwargs):

    return _GLOBALPYDRIVER.processupdate(
        items=items, update=update, ctx=ctx, **kwargs
    )


def processdelete(items, delete, ctx=None, **kwargs):

    return _GLOBALPYDRIVER.processdelete(
        items=items, delete=delete, ctx=ctx, **kwargs
    )


def processcrud(items, crud, ctx=None, **kwargs):

    return _GLOBALPYDRIVER.processcrud(
        items=items, crud=crud, ctx=ctx, **kwargs
    )


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
    FuncName.VERSION.value: VERSION,
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


def processquery(query, items, ctx=None, **kwargs):

    return _GLOBALPYDRIVER.processquery(
        query=query, ctx=ctx, items=items, **kwargs
    )


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

    @wraps(operator)
    def result(function, params, ctx):

        items = params[0]

        name = function.params[0].name

        return [
            item for item in params[0]
            if operator(function, item, name, params, ctx)
        ]

    return result


_ENRICHEDOPERATORSBYNAME = {}


for condition in CONDITIONS:
    _condoperator = condoperator(_OPERATORS_BY_NAME[condition.value])
    _condoperator.__name__ = condition.name
    _ENRICHEDOPERATORSBYNAME[condition.value] = _condoperator


class PyFunctionChooser(object):

    def get(self, name):

        if name in _ENRICHEDOPERATORSBYNAME:
            return _ENRICHEDOPERATORSBYNAME[name]

        else:
            try:
                return lookup(name)

            except ImportError:
                pass


class PyDriver(Driver):
    """In charge of accessing to data from a list of dictionaries or objects."""

    name = 'py'  # driver name

    def __init__(
            self, items=None, funcchooser=PyFunctionChooser(), *args, **kwargs
    ):
        """
        :param list items: list of data. Data are dictionaries. Default is [].
        """

        super(PyDriver, self).__init__(*args, **kwargs)

        self.items = [] if items is None else items
        self.funcchooser = funcchooser

    def _process(self, transaction, **kwargs):

        result = transaction

        if kwargs:
            raise ValueError(
                'Driver {0} does not support additional arguments {1}'.format(
                    self, kwargs
                )
            )

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

        result = kwargs.setdefault('items', self.items)

        if ctx is None:
            ctx = Context()

        if crud in ctx:  # is crud already calculated
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

            else:
                raise TypeError('{0} is not a crud element.'.format(crud))

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

                result = self.processfunction(function=query, ctx=ctx, **kwargs)

            elif isinstance(query, Expression):

                result = self.processexpr(expr=query, ctx=ctx, **kwargs)

            elif isinstance(query, CRUDElement):

                result = self.processcrud(crud=query, ctx=ctx, items=items)

            ctx[query] = result

        return result

    def processfunction(self, function, ctx=None, **kwargs):

        items = kwargs.pop('items', self.items)

        func = self.getfunction(function=function, ctx=ctx)

        isor = function.name == FuncName.OR

        result = []

        params = []

        for param in function.params:

            fitems = list(items) if isor else items

            param = self.processquery(
                query=param, ctx=ctx, items=fitems, **kwargs
            )

            if isor:
                result += param

            else:
                params.append(param)

        if func is None:

            if function.name not in [FuncName.AND.value, FuncName.OR.value]:

                raise NotImplementedError(
                        'Function {0} is not implented by {1}'.format(
                            function, self
                        )
                    )

        else:
            return func(function=function, ctx=ctx, params=params, **kwargs)

    def processexpr(self, expr, ctx=None, **kwargs):

        result = []

        if ctx is None:
            ctx = Context()

        items = kwargs.setdefault('items', self.items)

        for item in items:
            try:
                getsubitem(item, expr.name, error=True)

            except KeyError:
                pass

            else:
                result.append(item)
        print(result)
        if not result:  # if not item match, try to lookup the expression
            try:
                result = lookup(expr.name)

            except ImportError:
                items[:] = result

        else:
            items[:] = result

        ctx[expr] = result

        return result

    def processcreate(self, create, ctx=None, **kwargs):
        """Apply input Create element to items.

        :param Create create: data to add to input items.
        :return: created item.
        :rtype: list"""

        result = items = kwargs.setdefault('items', self.items)

        if ctx is None:
            ctx = Context()

        if create.query is not None:
            result = self.processquery(query=create.query, ctx=ctx, **kwargs)

        values = create.values

        for key, query in iteritems(create.values):

            value = self.processquery(query=query, ctx=ctx, **kwargs)
            values[key] = value

        result.append(values)

        return result

    def processread(self, read, ctx=None, **kwargs):
        """Return application of input Read to items.

        :param list items: items to read.
        :param Read read: read resource to apply on items.
        :return: read list.
        :rtype: list
        """

        result = items = kwargs.setdefault('items', self.items)

        if ctx is None:
            ctx = Context()

        if read.query is not None:
            result = self.processquery(query=read.query, ctx=ctx, **kwargs)

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

        result = kwargs.setdefault('items', self.items)

        if update.query is not None:
            result = self.processquery(query=update.query, ctx=ctx, **kwargs)

        values = {}

        for key in list(update.values):

            value = self.processquery(
                query=update.values[key], ctx=ctx, **kwargs
            )
            values[key] = value

        for item in result:
            for name, value in iteritems(values):

                if callable(value):
                    value(name, item)

                else:
                    item[name] = value

        return result

    def processdelete(self, delete, ctx=None, **kwargs):
        """Apply deletion rule to items.

        :param list items: items to modify.
        :param Delete delete: deletion rule.
        :rtype: list
        :return: modified/deleted items."""

        result = kwargs.setdefault('items', self.items)

        if ctx is None:
            ctx = Context()

        if delete.query is not None:
            result = self.processquery(query=delete.query, ctx=ctx, **kwargs)

        if delete.names:

            names = [
                self.processquery(query=name, ctx=ctx, **kwargs)
                for name in delete.names
            ]

            for name in names:
                for item in result:
                    if name in item:
                        del item[name]

        else:
            result[:] = []

        return result

    def getfunction(self, function, ctx=None):

        return ctx.get(function, self.funcchooser.get(function.name))

_GLOBALPYDRIVER = PyDriver()
