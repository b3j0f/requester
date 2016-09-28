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

__all__ = ['PyDriver']

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

        result = request

        values = self.values

        # apply request query before

        for crude in request.crudes:

            if isinstance(crude, Create):
                values.append(crude.value)

            elif isinstance(crude, Read):
                cruderesult = []
                for item in self.values:
                    if crude.select:
                        fitem = {}
                        for select in crude.select:
                            if select in item:
                                fitem[select] = item[select]

                    else:
                        fitem = item

                if crude.offset:
                    cruderesult = cruderesult[crude.offset:]

                if crude.limit:
                    cruderesult = cruderesult[:crude.limit]

                if crude.orderby:
                    for orderby in crude.orderby:
                        cruderesult.sort(key=lambda item: item.get(orderby))

                if crude.groupby:
                    groupbyresult = {}
                    _groupbyresult = []
                    for groupby in crude.groupby:
                        if _groupbyresult:
                            for item in _groupbyresult:
                                pass
                        _groupbyresult = {groupby: []}

                        for res in cruderesult:
                            if groupby in res:
                                groupbyresult[groupby] = res.pop(groupby)

                        # do the same for sub groupby

                if crude.join not in ('FULL', None):
                    raise NotImplementedError(
                        'Driver {0} does not support join {1}'.format(
                            self, crude.join
                        )
                    )

            elif isinstance(crude, Update):
                for item in values:
                    if crude.name in item:
                        item[crude.name] = crude.values

            elif isinstance(crude, Delete):
                if not crude.names:
                    self.values = [
                        item for item in self.valus if item not in values
                    ]

                else:
                    for name in crude.names:
                        for item in values:
                            if name in item:
                                del item[name]

            elif isinstance(crude, Exe):
                for item in values:
                    if crude.name in item:
                        item[crude.name](*crude.params)

        return result
