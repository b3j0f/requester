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

from b3j0f.annotation import Annotation
from b3j0f.schema import Schema, data2schema
from b3j0f.schema.lang.python import FunctionSchema

from .base import Driver
from .py import processread
from ..request.crud.base import CRUD, CRUDElement
from ..request.crud.create import Create
from ..request.crud.read import Read
from ..request.crud.update import Update

__all__ = [
    'CustomDriver',
    'func2crudprocessing', 'obj2driver', 'DriverAnnotation',
    'CreateAnnotation', 'ReadAnnotation', 'UpdateAnnotation',
    'DeleteAnnotation'
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

    def _process(self, transaction, crud, **kwargs):

        result = transaction

        crudname = type(crud).__name__.lower()

        funcs = getattr(self, '{0}s'.format(crudname))

        if funcs:
            for func in funcs:
                result = func(crud=crud, transaction=result, **kwargs)

        else:
            raise NotImplementedError(
                'No implementation found for {0}'.format(crudname)
            )

        return result


def func2crudprocessing(func=None):

    if func is not None and not isinstance(func, Schema):
        func = data2schema(func)

    def _processing(crud, transaction, _func=func, **kwargs):

        funckwargs = {}
        funcvarargs = []

        if isinstance(crud, (Create, Update)):
            funckwargs.update(crud.values)
            # todo : specific _func args

        for param in _func.params:

            if param.name in transaction.ctx:

                funckwargs[param.name] = transaction.ctx[param.name]

        funcresult = list(_func(*funcvarargs, **funckwargs))

        if isinstance(crud, Read):
            processread(read=crud, items=funcresult)

        transaction.ctx[crud] = funcresult

        return transaction

    return _processing


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

    fname = type(obj).__name__ if name is None else name

    # build obj schema if necessary
    if not isinstance(obj, Schema):
        fobj = data2schema(obj)

    else:
        fobj = obj

    fcreates = []
    freads = []
    fupdates = []
    fdeletes = []

    _locals = locals()
    # start to get information from annotations
    crudannotations = set(
        _CRUDAnnotation.get_annotations(obj) +
        _CRUDAnnotation.get_annotations(fobj)
    )
    for crudannotation in crudannotations:
        for target in crudannotation.targets:
            targetname = target
            fobjtarget = getattr(fobj, targetname)
            _locals['f{0}s'.format(crudannotation.name)].append(fobjtarget)

    # then parse function parameters
    for crudname in (crudname.lower() for crudname in CRUD.__members__):

        cruds = _locals['{0}s'.format(crudname)]

        for crud in cruds:

            crudfunc = getattr(fobj, crud)

            fcrudfunc = func2crudprocessing(crudfunc)

            _locals['f{0}s'.format(crudname)].append(fcrudfunc)

    ffunctions = {} if functions is None else functions

    for schema in obj.getschemas():

        if isinstance(schema, FunctionSchema):
            ffunctions[schema.name] = schema

    return CustomDriver(
        name=fname,
        creates=fcreates,
        reads=freads,
        updates=fupdates,
        deletes=fdeletes,
        functions=ffunctions
    )


class DriverAnnotation(Annotation):
    """Generate a deriver from class content."""

    def __init__(
            self,
            name=None, creates=None, reads=None, updates=None, deletes=None,
            *args, **kwargs
    ):
        """
        :param str name: driver name to generate. Default target type name.
        :param list creates: create function names. Related functions must
            return created objects with additional fields such as the id.
        :param list reads: read function names. Related functions must return
            list of read objects.
        :param list updates: update function names. Related functions must
            return updated data.
        :param list deletes: delete function names. Related functions must
            return deleted objects."""

        super(DriverAnnotation, self).__init__(*args, **kwargs)

        self.name = name
        self.creates = creates
        self.reads = reads
        self.updates = updates
        self.deletes = deletes

    def getdriver(self, obj):
        """Get a driver corresponding to input target instance related
        to this attributes.

        :param obj: instance to transform to a functional driver.
        :rtype: CustomDriver"""

        kwargs = {'name': self.name}

        for crud in (crud.lower() for crud in CRUD.__members__):

            fcrud = '{0}s'.format(crud)

            funcnames = getattr(self, fcrud)

            for funcname in funcnames:
                func = getattr(obj, funcname)

                kwargs.setdefault(fcrud, []).append(func)

        return obj2driver(**kwargs)


class _CRUDAnnotation(Annotation):

    def __init__(self, crud, *args, **kwargs):
        """
        :param str crud: crud name.
        """

        super(_CRUDAnnotation, self).__init__(self)

        self.crud = crud.name if isinstance(crud, CRUDElement) else crud


class CreateAnnotation(_CRUDAnnotation):

    def __init__(self, crud=CRUD.READ, *args, **kwargs):

        super(CreateAnnotation, self).__init__(crud=crud, *args, **kwargs)


class ReadAnnotation(_CRUDAnnotation):

    def __init__(self, crud=CRUD.READ, *args, **kwargs):

        super(ReadAnnotation, self).__init__(crud=crud, *args, **kwargs)


class UpdateAnnotation(_CRUDAnnotation):

    def __init__(self, crud=CRUD.UPDATE, *args, **kwargs):

        super(UpdateAnnotation, self).__init__(crud=crud, *args, **kwargs)


class DeleteAnnotation(_CRUDAnnotation):

    def __init__(self, crud=CRUD.DELETE, *args, **kwargs):

        super(DeleteAnnotation, self).__init__(crud=crud, *args, **kwargs)
