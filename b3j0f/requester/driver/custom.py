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

"""Driver generator module."""

from six import iteritems

from inspect import getmembers, isroutine

from copy import deepcopy

from b3j0f.annotation import Annotation
from b3j0f.schema import Schema, data2schema

from .base import Driver
from .py import processread
from .transaction import State, Transaction
from .utils import getchildren
from ..request.consts import FuncName, CONDITIONS
from ..request.expr import Expression, Function
from ..request.crud.base import CRUD, CRUDElement
from ..request.crud.create import Create
from ..request.crud.read import Read
from ..request.crud.update import Update

try:
    from threading import Thread

except ImportError:
    from dummy_threading import Thread

__all__ = [
    'CustomDriver',
    'func2crudprocessing', 'obj2driver',
    'CreateAnnotation', 'ReadAnnotation', 'UpdateAnnotation',
    'DeleteAnnotation', 'query2kwargs'
]


class CustomDriver(Driver):
    """Driver with fine grained implementation of crud functions.

    This driver uses at most five set of functions for five respective crud
    types.

    The processing execute the right function for all request crud objects.

    Functions must takes in parameters a 'crud' object, a 'request' object and
    kwargs for specific driver uses (like explain for example).

    Functions must return a request."""

    def __init__(
            self, creates=None, reads=None, updates=None, deletes=None,
            functions=None, *args, **kwargs
    ):
        """
        :param list creates: creation functions. Take in parameters a request,
            a crud operation and specific kwargs. Return created item
            with additional fields such as id.
        :param list reads: reading functions. Take in parameters a request,
            a crud operation and specific kwargs. Return read items.
        :param list updates: updating functions. Take in parameters a request,
            a crud operation and specific kwargs. Return updated items.
        :param list deletes: deletion functions. Take in parameters a request,
            a crud operation and specific kwargs. Return deleted items.
        :param dict functions: function to process for specific query functions
        .
        """

        super(CustomDriver, self).__init__(*args, **kwargs)

        self.creates = [] if creates is None else creates
        self.reads = [] if reads is None else reads
        self.updates = [] if updates is None else updates
        self.deletes = [] if deletes is None else deletes
        self.functions = {} if functions is None else functions

    def _process(self, transaction, **kwargs):

        result = transaction

        if transaction.state is State.COMMITTING:

            for crud in transaction.cruds:

                self.processquery(
                    query=crud.query, transaction=transaction, **kwargs
                )

                crudname = type(crud).__name__.lower()
                funcs = getattr(self, '{0}s'.format(crudname))

                if funcs:
                    transaction.ctx[crud] = []

                    for func in funcs:
                        fresult = func(crud=crud, transaction=result, **kwargs)

                        if isinstance(fresult, list):
                            transaction.ctx[crud] += fresult

                        elif isinstance(fresult, Transaction):
                            transaction.ctx[crud] += fresult.ctx[crud]

                        elif fresult is not None:
                            msg1 = 'Wrong func result {0}.'.format(func)
                            msg2 = 'Transaction, list or None expected.'
                            raise TypeError('{0} {1}'.format(msg1, msg2))

                else:
                    raise NotImplementedError(
                        'No implementation found for {0}'.format(crudname)
                    )

        return result

    def processquery(self, query, transaction, **kwargs):

        result = query

        children = getchildren(query)

        for child in children:
            self.processquery(query=child, transaction=transaction, **kwargs)

        if isinstance(query, Function):

            if query.name in self.supportedfunctions:
                pass

            elif query.name in self.functions:

                func = self.functions[query.name]
                result = func(
                    query=query, transaction=transaction, **kwargs
                )
                transaction.ctx[query] = result

            else:
                raise NotImplementedError(
                    '{0} is not implemented by {1}'.format(query, self)
                )

        return result


def func2crudprocessing(func, annotation):

    if not isinstance(func, Schema):
        func = data2schema(func)

    def _processing(crud, transaction, _func=func, **kwargs):

        if annotation.gateway:

            pnames = [param.name for param in _func.params]

            orfunckwargs = query2kwargs(
                query=crud.query, ctx=transaction.ctx, pnames=pnames
            )

            funcresult = []

            threads = []

            for funckwargs in orfunckwargs:

                funcvarargs = []

                for param in _func.params:

                    pname = param.name

                    if pname in transaction.ctx and pname not in funckwargs:

                        funckwargs[param.name] = transaction.ctx[param.name]

                if isinstance(crud, (Create, Update)):

                    funckwargs.update(crud.values)

                if len(orfunckwargs) == 1:

                    funcresult = _func(*funcvarargs, **funckwargs)

                else:
                    def target(funcresult=funcresult):
                        result = _func(*funcvarargs, **funckwargs)
                        funcresult += result

                    thread = Thread(target=target)
                    thread.start()
                    threads.append(thread)

            # TODO: remove duplicate data
            for thread in threads:
                thread.join()

            transaction.ctx[crud] = [] if funcresult is None else funcresult

            if isinstance(crud, Read):
                funcresult = processread(
                    read=crud,
                    items=funcresult,
                    ctx=transaction.ctx
                )

        else:
            fresult = _func(transaction=transaction, crud=crud, **kwargs)

            if fresult is None:

                funcresult = transaction.ctx[crud]

            else:
                funcresult = fresult.ctx[crud]

        transaction.ctx[crud] = funcresult

        return transaction

    return _processing


def query2kwargs(query, ctx, pnames):
    """Parse input query and return a list of kwargs for crud function.

    The list indicates

    :param Expression query: query to parse in order to get conditions.
    :param Context ctx: execution context.
    :param list pnames: parameter names to identify such as conditional
        expressions from the query.
    :return: list of kwargs where keys are parameter names and values are dict
        of tuple of operator name, parameters.
    :rtype: list
    """
    result = [{}]

    if isinstance(query, Expression):

        if query.name in pnames:

            for item in result:
                item[query.name] = ctx.get(query, {FuncName.EXISTS.value: []})

        if isinstance(query, Function):

            if query.name in CONDITIONS:

                isor = query.name == FuncName.OR.value

                if isor:
                    orresult = result
                    result = []

                if isor or query.name == FuncName.AND.value:

                    for param in query.params:

                        queryresult = query2kwargs(
                            query=param, ctx=ctx, pnames=pnames
                        )

                        fresult = deepcopy(orresult) if isor else result

                        for item in fresult:

                            for qitem in queryresult:

                                for qname, qconds in iteritems(qitem):

                                    if qname in item:
                                        item[qname].update(qconds)

                                    else:
                                        item[qname] = qconds

                        if isor:
                            result += fresult

                        else:
                            result = fresult

                elif query.params:
                    elt = query.params[0]

                    if not isinstance(elt, Expression):
                        raise TypeError(
                            'Wrong type {0} in {1}. Expr expected.'.format(
                                elt, query
                            )
                        )

                    if elt.name in pnames:

                        params = [
                            ctx.get(param, param) for param in query.params[1:]
                        ]

                        for item in result:
                            item.setdefault(elt.name, {})[query.name] = params

                else:
                    raise NotImplementedError(
                        'Parameters are missing on {0}.'.format(query)
                    )

    elif isinstance(query, CRUDElement):

        if query.ctxname in pnames:

            for item in result:
                item[query.ctxname] = ctx.get(query, query)

    return result


def obj2driver(
        obj, name=None,
        creates=None, reads=None, updates=None, deletes=None, functions=None
):
    """Convert an object to a driver.

    :param str name: driver name. obj type name by default.
    :param list creates: create function names to retrieve from the obj.
    :param list reads: read function names to retrieve from the obj.
    :param list updates: update function names to retrieve from the obj.
    :param list deletes: delete function names to retrieve from the obj.
    :param dict functions: fuctions by name. obj functions by default.
    :rtype: CustomDriver
    """

    if name is None:
        name = type(obj).__name__

    # build obj schema if necessary
    if not isinstance(obj, Schema):
        fobj = data2schema(obj, _force=True)

    else:
        fobj = obj

    if creates is None:
        creates = []

    if reads is None:
        reads = []

    if updates is None:
        updates = []

    if deletes is None:
        deletes = []

    if functions is None:
        functions = {}

    _locals = locals()

    # get data from crud annotations
    crudannotations = _CRUDAnnotation.get_annotations(obj, maxdepth=2)
    for crudannotation in crudannotations:

        for target in crudannotation.targets:

            otarget = getattr(obj, target.__name__)

            ftarget = func2crudprocessing(otarget, crudannotation)

            _locals['{0}s'.format(crudannotation.name)].append(ftarget)

    for name, member in getmembers(fobj):

        if isroutine(member):

            functions[name] = member

    return CustomDriver(
        name=name,
        creates=creates,
        reads=reads,
        updates=updates,
        deletes=deletes,
        functions=functions
    )


class _CRUDAnnotation(Annotation):
    """Base Annotation for annotating CRUD functions."""

    def __init__(self, crud, gateway=True, *args, **kwargs):
        """
        :param str crud: crud name.
        :param bool gateway: pass arguments from the context to the function.
        """

        super(_CRUDAnnotation, self).__init__(*args, **kwargs)

        if isinstance(crud, CRUD):
            crud = crud.name.lower()

        self.name = crud
        self.gateway = gateway


class CreateAnnotation(_CRUDAnnotation):
    """Annotate create functions."""

    def __init__(self, *args, **kwargs):

        super(CreateAnnotation, self).__init__(
            crud=CRUD.CREATE, *args, **kwargs
        )


class ReadAnnotation(_CRUDAnnotation):
    """Annotate read functions."""

    def __init__(self, *args, **kwargs):

        super(ReadAnnotation, self).__init__(
            crud=CRUD.READ, *args, **kwargs
        )


class UpdateAnnotation(_CRUDAnnotation):
    """Annotate update functions."""

    def __init__(self, *args, **kwargs):

        super(UpdateAnnotation, self).__init__(
            crud=CRUD.UPDATE, *args, **kwargs
        )


class DeleteAnnotation(_CRUDAnnotation):
    """Annotate delete functions."""

    def __init__(self, *args, **kwargs):

        super(DeleteAnnotation, self).__init__(
            crud=CRUD.DELETE, *args, **kwargs
        )
