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

__all__ = ['PyDriver', 'processcrude', 'create', 'read', 'update', 'delete', 'exe']

from .base import Driver

from operator import (
    lt, le, eq, ne, ge, gt, not_, truth, is_, is_not, abs, add, div, floordiv,
    index, inv, invert, mod, mul, neg, or_, pow, rshift, lshift, sub, truediv,
    xor, concat, countOf, indexOf, repeat, sequenceIncludes, iadd, iand,
    getitem, setitem, delitem, getslice, setslice, delslice, iconcat,
    idiv, ifloordiv, ilshift, imod, imul, ior, ipow, irepeat, irshift, isub,
    itruediv, ixor
)

from re import match

from ..request.expr import FuncName
from ..request.crude.create import Create
from ..request.crude.read import Read
from ..request.crude.update import Update
from ..request.crude.delete import Delete
from ..request.crude.exe import Exe


_OPERTORS_BY_NAME = {
    FuncName.LT.value: lt,
    FuncName.LE.value: le,
    FuncName.EQ.value: eq,
    FuncName.NE.value: ne,
    FuncName.GT.value: gt,
    FuncName.GE.value: ge,
    FuncName.NOT.value: not_,
    FuncName.TRUTH.value: truth,
    FuncName.IS.value: is_,
    FuncName.ISNOT.value: is_not,
    FuncName.ABS.value: abs,
    FuncName.ADD.value: add,
    FuncName.DIV.value: truediv,
    FuncName.INDEX.value: index,
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
    FuncName.IOR.value: ior,
    FuncName.IPOW.value: ipow,
    FuncName.IREPEAT.value: irepeat,
    FuncName.IRSHIFT.value: irshift,
    FuncName.ISUB.value: isub,
    FuncName.IXOR.value: ixor
}

class PyDriver(Driver):
    """In charge of accessing to data from a list of dictionaries or objects."""

    name = 'py'  # driver name

    def __init__(self, values, *args, **kwargs):
        """
        :param list values: list of data. Data are dictionaries.
        """

        super(PyDriver, self).__init__(*args, **kwargs)

        self.values = values

    def process(self, request, **kwargs):
        """Generic method to override in order to crude input data related to
        request and kwargs.

        :param Request request: request to process.
        :param bool explain: give additional information about the request
            execution.
        :param dict kwargs: additional parameters specific to the driver.
        :return: request.
        :rtype: Request
        """

        if kwargs:
            raise ValueError(
                'Driver {0} does not support additional arguments {1}'.format(
                    self, kwargs
                )
            )

        for crude in request.crudes:
            processcrude(request=request, items=self.values, crude=crude)

        return request


def create(items, create):
    """Apply input Create element to items.

    :param list items: items to process with input Create.
    :param Create create: data to add to input items.
    :return: created item.
    :rtype: list"""

    items.append(create.values)

    return items


def read(items, read):
    """Return application of input Read to items.

    :param list items: items to read.
    :param Read read: read resource to apply on items.
    :return: read list.
    :rtype: list
    """

    result = items

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
            'Driver {0} does not support join {1}'.format(
                self, read.join()
            )
        )

    items[:] = result

    return result


def update(items, update):
    """Apply update to items.

    :param list items: items to update.
    :param Update update: update rule.
    :return: updated items.
    :rtype: list"""

    result = []

    for item in items:
        if update.name in item:
            item[update.name] = update.values
            result.append(item)

    return result


def delete(items, delete):
    """Apply deletion rule to items.

    :param list items: items to modify.
    :param Delete delete: deletion rule.
    :rtype: list
    :return: modified/deleted items."""

    result = []

    if delete.names:
        for name in delete.names:
            for item in items:
                if name in item:
                    del item[name]
                    result.append(item)

    else:
        result = items
        items[:] = []

    return result


def exe(items, exe):
    """Execute exe on input items.

    :param list items: items to process.
    :param Exe exe: execution rule to process on items. Name must match items
        key and value must be a function.
    :rtype: list
    :return: list of (item, execution result)."""

    result = []

    for item in values:
        if exe.name in item:
            func = item[exe.name]
            funcresult = func(*params)
            result.append([item, result])

    return result

def processcrude(request, items, crude):
    """Apply the right rule on input items.

    :param list items: items to process.
    :param CRUDEElement crude: crude rule to apply.
    :rtype: list
    :return: list"""

    if isinstance(crude, Create):
        processresult = create(items=items, create=crude)

    elif isinstance(crude, Read):
        processresult = read(items=items, read=crude)

    elif isinstance(crude, Update):
        processresult = update(items=items, update=crude)

    elif isinstance(crude, Delete):
        processresult = delete(items=items, delete=crude)

    elif isinstance(crude, Exe):
        processresult = exe(items=items, exe=crude)

    request.ctx[crude] = processresult

    return items
