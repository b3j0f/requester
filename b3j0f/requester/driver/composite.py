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

"""Module which specifices a composite of drivers."""

from b3j0f.schema import Schema, data2schema

from six import iteritems

from .py import PyDriver
from .utils import getnames
from .ctx import Context
from ..request.consts import FuncName
from ..request.base import BaseElement
from ..request.crud.base import CRUDElement
from ..request.crud.create import Create
from ..request.crud.delete import Delete
from ..request.crud.read import Read
from ..request.crud.update import Update
from ..request.expr import Expression, Function

__all__ = ['DriverComposite']


class DriverComposite(PyDriver):
    """In charge of distributing a request to several drivers."""

    def __init__(self, drivers, default=None, *args, **kwargs):
        """
        :param list drivers: drivers to use.
        :param Driver default: default driver to use if no driver found.
        """
        super(DriverComposite, self).__init__(*args, **kwargs)

        self.drivers = {}

        for driver in drivers:
            if not isinstance(driver, Schema):
                driver = data2schema(driver, name=driver.name, _force=True)

            self.drivers[driver.name] = driver

        self.default = default

    def getdrivers(self, name, maxdepth=3):
        """Get a list of drivers and models which correspond to the input name.

        :param str name: data name to retrieve.
        :return: list of couples of driver with model where name match a
            driver/model name.
        :rtype: list
        :raises: ValueError if no driver found.
        """
        result = []

        driver = None
        model = None

        names = getnames(name)

        rootname = names[0]

        tmpelts = []  # list of couple of driver/model
        elts = []  # list of couple of driver/model

        for depth in range(maxdepth):

            if tmpelts:
                for driver, model in list(tmpelts):
                    if hasattr(model, rootname):
                        elts.append((driver, getattr(model, rootname)))
                        tmpelts.remove((driver, model))

                    else:
                        for name, schema in iteritems(model.getschemas()):
                            tmpelts.append((driver, schema))

                    tmpelts.remove((driver, model))

            else:
                if rootname in self.drivers:
                    driver = self.drivers[rootname]
                    break

                else:
                    tmpelts = [
                        (driver, driver) for driver in self.drivers.values()
                    ]

        if elts:
            for name in names:

                elts = [
                    (elt[0], getattr(elt[1], name)) for elt in elts
                    if hasattr(elt[1], name)
                ]
                if not elts:
                    break

        if not elts:
            raise ValueError('No driver found for {0}'.format(name))

        return result

    def _process(self, transaction, **kwargs):

        for crud in transaction.cruds:
            self.processdeeply(elt=crud, transaction=transaction, **kwargs)

        return transaction

    def processdeeply(self, elt, transaction, _elts=None, **kwargs):
        """Parse input elt and return its evaluation."""
        result = elt

        ctx = transaction.ctx

        if elt in ctx:
            result = ctx[elt]

        elif isinstance(elt, BaseElement):
            # get driver and model
            if isinstance(elt, CRUDElement) or FuncName.contains(elt.name):
                driver = model = None

            else:
                # do something
                driverswmodel = self.getdrivers(elt.name)

                if len(driverswmodel) > 1:
                    raise ValueError(
                        'Too many drivers found for elt {0}. {1}'.format(
                            elt, driverswmodel
                        )
                    )

                driver, model = driverswmodel

            # fill elts
            if _elts is None:
                _elts = [[driver, model, elt]]

            elif driver is not None:
                olddriver = _elts[-1][0]

                if olddriver is None:
                    _elts[-1][0] = driver

                elif olddriver != driver:
                        _elts.append((driver, model, elt))

            if isinstance(elt, Function):

                isor = elt.name == FuncName.OR.value

                if isor:
                    ctx = Context(transaction.ctx)
                    ftransaction = transaction.open(ctx=ctx)

                else:
                    ftransaction == transaction

                for param in elt.params:
                    self.processdeeply(
                        elt=param, transaction=ftransaction, _elts=_elts,
                        **kwargs
                    )

                    if isor:
                        transaction.ctx.fill(ftransaction.ctx)

            elif isinstance(elt, CRUDElement):

                self.processdeeply(
                    elt=elt.query, transaction=transaction, _elts=_elts,
                    **kwargs
                )

                if isinstance(elt, (Create, Update)):
                    self.processdeeply(
                        elt=elt.name, transaction=transaction, _elts=_elts,
                        **kwargs
                    )
                    for name, value in iteritems(elt.values):
                        self.processdeeply(
                            elt=name, transaction=transaction, _elts=_elts,
                            **kwargs
                        )
                        self.processdeeply(
                            elt=value, transaction=transaction, _elts=_elts,
                            **kwargs
                        )

                elif isinstance(elt, Read):
                    for select in elt.select():
                        self.processdeeply(
                            elt=select, transaction=transaction, _elts=_elts,
                            **kwargs
                        )

                    for groupby in elt.groupby():
                        self.processdeeply(
                            elt=groupby, transaction=transaction, _elts=_elts,
                            **kwargs
                        )

                    for orderby in elt.orderby():
                        self.processdeeply(
                            elt=orderby, transaction=transaction, _elts=_elts,
                            **kwargs
                        )

                    self.processdeeply(
                        elt=elt.join(), transaction=transaction, _elts=_elts,
                        **kwargs
                    )
                    self.processdeeply(
                        elt=elt.limit(), transaction=transaction, _elts=_elts,
                        **kwargs
                    )
                    self.processdeeply(
                        elt=elt.offset(), transaction=transaction, _elts=_elts,
                        **kwargs
                    )

                elif isinstance(elt, Delete):

                    for name in elt.names:
                        self.processdeeply(
                            elt=name, transaction=transaction, _elts=_elts,
                            **kwargs
                        )

            if _elts[-1][2] == elt:

                driver, model, _ = _elts.pop()

                if driver is None:
                    driver = self.default

                if driver is None:
                    raise ValueError(
                        'No driver found to process {0}'.format(elt)
                    )

                if isinstance(elt, Expression):
                    crud = Read(query=elt)

                else:
                    crud = elt

                crudcopy = crud.copy()
                updatename(elt=crudcopy, driver=driver)

                result = transaction.open(
                    driver=driver, cruds=[crudcopy]
                ).commit(**kwargs)

        ctx[elt] = result

        return result

    def __repr__(self):
        """Driver representation with drivers."""
        return 'CompositeDriver({0}, {1}, {2})'.format(
            self.name, self.drivers, self.default
        )


def updatename(elt, driver):
    """Rename elt in order to be specific to input driver."""
    for slot in elt.__slots__:

        subelt = getattr(elt, slot)

        if isinstance(subelt, Expression):
            if subelt.name.startswith(driver.name):
                subelt.name = subelt.name[len(driver.name) + 1:]

        elif isinstance(subelt, CRUDElement):

            subelt = updatename(elt=subelt, driver=driver)
            setattr(elt, slot, subelt)
