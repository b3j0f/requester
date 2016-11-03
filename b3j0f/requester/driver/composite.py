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

"""Module which specifices a composite of drivers.

Pathing algorithm:
- Deep pathing

Let an base element such as a tree of nodes.

The path is recursive with a stack of couple of (drivers, node). The goal is to
aggregate same drivers in the tree with a parent which has a lot of child node
with same drivers.

From a node, we have drivers:

1- case 0 (stack is empty): we put the drivers and the node in the stack.

2- if the current node has no drivers, we add current node to the stack in
    order to be updated by next nodes if a system exists among children and
    might execute upper nodes.

3- if current node has drivers, we look for the last item in the stack (parent
    node):

    - parent node has not drivers: we look for the grand parent node:

        - grand parent does not exist or has different drivers: parent drivers
            become currend drivers in order to remove node without drivers
            between node with drivers.

        - grand parent node has same drivers than current: we remove the parent
            node in order to keep a chain of same drivers.

        The algorithm ensures that grand parent node has drivers !

    - parent node has drivers:

        - parent node drivers not equal to current drivers: add current node
            in the stack with current drivers.

        - parent node drivers equal to current drivers: do nothing in order to
            add a node in the same driver execution.
"""

from inspect import getmembers

from b3j0f.schema import Schema, data2schema

from six import iteritems

from copy import deepcopy

from .base import Driver
from .utils import getnames, getchildren, getsubitem
from .ctx import Context
from ..request.consts import FuncName
from ..request.base import BaseElement
from ..request.crud.base import CRUDElement
from ..request.crud.read import Read
from ..request.expr import Expression, Function

__all__ = ['DriverComposite']


class DriverComposite(Driver):
    """In charge of distributing a request to several drivers.

    Driver parameters are :

    .. csv-table::

        :header: name, type, description

        - discovery, bool (False), enable driver discovery from queries.
        - many, bool (False), disable multi driver execution for one query.
        - besteffort, bool (False), enable discovery and many.
        - maxdepth, int (3), max depth data search in the driver tree.
        - explain, bool (False), return query information processsing in ctx.
    """

    def __init__(self, drivers, *args, **kwargs):
        """
        :param list drivers: drivers to use.
        """
        super(DriverComposite, self).__init__(*args, **kwargs)

        self.drivers = {}
        self.sdrivers = {}

        for driver in drivers:
            if isinstance(driver, Schema):
                sdriver = driver

            else:
                sdriver = data2schema(driver, name=driver.name, _force=True)

            self.drivers[driver.name] = driver
            self.sdrivers[driver.name] = sdriver

    def getdrivers(self, name, maxdepth=3, discovery=False, many=False):
        """Get a list of drivers corresponding with input model name.

        :param str name: data name to identify such as a driver model.
        :param bool discovery: if True (False by default), try to find drivers
            where name match with driver models.
        :param bool many: if False (default), raise a ValueError if more than
            one driver is found.
        :return: list of couples of driver with model where name match a
            driver/model name.
        :rtype: list
        :raises: ValueError if name is not a standard query name and no driver
            is found.
        """
        result = []

        if name in self.supportedfunctions:
            return result

        names = getnames(name)

        rootname = names[0]

        if discovery:

            tmpelts = []  # list of couple of driver/model.
            elts = []  # list of couple of driver/model.

            for depth in range(maxdepth + 1):

                if elts:
                    tmpelts = []

                    for driver, model in list(elts):
                        if hasattr(model, rootname):
                            tmpelts.append((driver, getattr(model, rootname)))

                        elif isinstance(model, Schema):
                            for mname, submodel in iteritems(
                                model.getschemas()
                            ):
                                tmpelts.append((driver, submodel))

                        else:
                            for name, member in getmembers(model):
                                if name[0] != '_':
                                    tmpelts.append((driver, member))

                    elts = tmpelts

                else:
                    if rootname in self.drivers:
                        result = [self.drivers[rootname]]
                        break

                    else:
                        elts = [
                            (self.drivers[name], self.sdrivers[name])
                            for name in self.drivers
                        ]

            if not result:
                if elts:
                    for name in names[1:]:
                        elts = [
                            (elt[0], getattr(elt[1], name)) for elt in elts
                            if hasattr(elt[1], name)
                        ]
                        if not elts:
                            break

                    else:
                        result = [item[0] for item in elts]

        else:
            if rootname in self.drivers:
                result = [self.drivers[rootname]]

            else:
                raise ValueError(
                    '{0} is not handled by {1}'.format(name, self)
                )

        if result:
            if many and len(result) > 1:
                raise ValueError(
                    'Too many drivers found for elt {0}: {1}.'.format(
                        elt, result
                    )
                )

        else:
            raise ValueError('No driver found for processing {0}'.format(name))

        return result

    def _process(self, transaction, **kwargs):

        for crud in transaction.cruds:
            self.processdeeply(elt=crud, transaction=transaction, **kwargs)

        return transaction

    def processdeeply(self, elt, transaction, _elts=None, **kwargs):
        """Parse input elt and return its evaluation."""
        result = elt

        # process specific driver parameters
        discovery = kwargs.get('discovery', False)
        many = kwargs.get('many', False)
        besteffort = kwargs.get('besteffort')
        maxdepth = kwargs.get('maxdepth', 3)

        if besteffort is not None:
            many = discovery = besteffort

        ctx = transaction.ctx

        if elt in ctx:
            result = ctx[elt]

        elif isinstance(elt, BaseElement):
            # get driver and model

            # default case, elt is a crudelement or elt is a standard query
            if isinstance(elt, CRUDElement):
                drivers = []

            else:
                drivers = self.getdrivers(
                    name=elt.name, maxdepth=maxdepth, discovery=discovery,
                    many=many
                )

            # fill elts
            if _elts is None:
                _elts = [[drivers, elt]]

            elif drivers:
                olddrivers = _elts[-1][0]

                if not olddrivers:
                    if len(_elts) == 1 or _elts[-2][0] != drivers:
                        _elts[-1][0] = drivers

                    else:
                        _elts.pop()

                elif olddrivers != drivers:
                        _elts.append([drivers, elt])

            elif _elts[-1][0]:
                _elts.append([drivers, elt])

            children = getchildren(elt)

            isor = isinstance(elt, Function) and elt.name == FuncName.OR.value

            for child in children:

                if isor:
                    ctx = Context(transaction.ctx)
                    ftransaction = transaction.open(ctx=ctx)

                else:
                    ftransaction = transaction

                self.processdeeply(
                    elt=child, transaction=ftransaction, _elts=_elts, **kwargs
                )

                if isor:
                    transaction.ctx.fill(ftransaction.ctx)

            if _elts[-1][1].ctxname == elt.ctxname:
                drivers, _ = _elts.pop()

                if drivers:

                    if isinstance(elt, Expression):
                        crud = Read(query=elt)

                    else:
                        crud = elt

                        # apply join queries if given
                        if isinstance(elt, Read) and elt.join is not None:

                            joinqueries = transformjoinquery(
                                ctx=ctx,
                                query=elt.join.query
                            )

                            if joinqueries:

                                crud = crud.copy(ctx=ctx)

                                if len(joinqueries) == 1:
                                    if elt.query is None:
                                        crud.query = joinqueries[0]

                                    else:
                                        crud.query = Function(
                                            name=FuncName.AND.value,
                                            params=(
                                                joinqueries[0], elt.query
                                            )
                                        )

                                else:
                                    params = tuple(joinqueries) + (elt.query,)
                                    crud.query = Function(
                                        name=FuncName.OR.value,
                                        params=params
                                    )

                    crudcopy = crud.copy()

                    threads = []

                    for driver in drivers:

                        updatename(elt=crudcopy, dname=driver.name)

                        dparams = deepcopy(kwargs)
                        dparams.pop('async', None)

                        transaction.ctx.setdefault(elt, [])
                        transaction.ctx.setdefault(driver.name, [])

                        ftransaction = transaction.open(
                            driver=driver, cruds=[crudcopy]
                        )

                        def callback(transaction, **kwargs):
                            driverres = transaction.ctx[crudcopy]
                            transaction.ctx[driver.name] += driverres
                            transaction.ctx[elt] += driverres

                        if len(drivers) == 1:
                            result = ftransaction.commit(
                                callback=callback, **dparams
                            )

                        else:
                            thread = ftransaction.commit(
                                async=True, callback=callback, **dparams
                            )
                            threads.append(thread)

                        for thread in threads:
                            thread.join()

                    result = ctx[elt]

                else:
                    raise ValueError(
                        'No driver found to process {0}'.format(elt)
                    )

        return result

    def __repr__(self):
        """Driver representation with drivers."""
        return 'CompositeDriver({0}, {1})'.format(self.name, self.drivers)


def updatename(elt, dname):
    """Rename elt in order to be specific to input driver name.

    :param Expression elt: expression to rename with its children.
    :param str dname: driver name.
    """

    if isinstance(elt, Expression):

        if elt.name.startswith(dname):
            elt.name = elt.name[len(dname) + 1:]

    children = getchildren(elt)

    for child in children:
        updatename(elt=child, dname=dname)


def transformjoinquery(ctx, query):
    """Transform join query to a set of queries.

    :param Context ctx: execution context.
    :param Expression query.
    :rtype: list"""
    result = [query]

    if query is not None:

        if isinstance(query, Function):

            if query.name in (FuncName.EQ.value, FuncName.NEQ.value):

                if type(query.params[1]) is Expression:

                    pnames = query.params[1].split('.')
                    pctxname = pnames[0]

                    if pctxname in ctx:

                        items = ctx[pctxname]

                        subname = '.'.join(pnames[1:])
                        values = [
                            getsubitem(item=item, name=subname)
                            for item in items
                        ]

                        if query.name == FuncName.EQ.value:
                            name = FuncName.IN.value

                        else:
                            name = FuncName.NIN.value

                        newexpr = Function(
                            name=name, params=(query.params[0], values)
                        )

                        result = [newexpr]

            elif query.name in (FuncName.AND.value, FuncName.OR.value):

                result = [[]] if query.name == FuncName.AND.value else []

                for param in query.params:

                    presult = transformjoinquery(ctx=ctx, query=param)

                if query.name == FuncName.AND.value:

                    result[0] += presult

                else:
                    result.append(presult)

    return result
