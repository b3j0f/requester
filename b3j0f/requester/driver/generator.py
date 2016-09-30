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

"""Driver generator module."""

__all__ = [
    'FunctionalDriver',
    'func2crodprocessing', 'obj2driver', 'DriverAnnotation',
    'CreateAnnotation', 'ReadAnnotation', 'UpdateAnnotation', 'DeleteAnnotation'
]

from b3j0f.annotation import Annotation
from b3j0f.schema import data2schema, data2schemacls, Schema
from b3j0f.schema.lang.python import FunctionSchema

from six import string_types

from .base import Driver
from .py import read
from ..request.crude.base import CRUDE
from ..request.crude.create import Create
from ..request.crude.read import Read
from ..request.crude.update import Update
from ..request.crude.delete import Delete
from ..request.crude.exe import Exe


class FunctionalDriver(Driver):
    """Driver with fine grained implementation of crude functions.

    This driver uses at most five set of functions for five respective crude
    types.

    The processing execute the right function for all request crude objects.

    Functions must takes in parameters a 'crude' object, a 'request' object and
    kwargs for specific driver uses (like explain for example).

    Functions must return a request."""

    def __init__(
            self,
            creates=None, reads=None, updates=None, deletes=None, exes=None,
            *args, **kwargs
    ):
        """
        :param list creates: creation functions. Take in parameters a request,
            a crude operation and specific kwargs. Return created item
            with additional fields such as id.
        :param list reads: reading functions. Take in parameters a request,
            a crude operation and specific kwargs. Return read items.
        :param list updates: updating functions. Take in parameters a request,
            a crude operation and specific kwargs. Return updated items.
        :param list deletes: deletion functions. Take in parameters a request,
            a crude operation and specific kwargs. Return deleted items.
        :param list exes: execution functions. Take in parameters a request,
            a crude operation and specific kwargs. Return function result.
        """

        super(FunctionalDriver, self).__init__(*args, **kwargs)

        self.creates = [] if creates is None else creates
        self.reads = [] if reads is None else reads
        self.updates = [] if updates is None else updates
        self.deletes = [] if deletes is None else deletes
        self.exes = [] if exes is None else exes

    def process(self, request, **kwargs):

        result = request

        for crude in request.crudes:

            crudename = type(crude).__name__.lower()

            funcs = getattr(self, '{0}s'.format(crudename))

            if funcs:
                for func in funcs:
                    result = func(crude=crude, request=result, **kwargs)

            else:
                raise NotImplementedError(
                    'No implementation found for {0}'.format(crudename)
                )

        return result


def func2crudeprocessing(func=None, obj=None):

    if func is not None and not isinstance(func, Schema):
        func = data2schema(func)

    if obj is not None and not isinstance(obj, Schema):
        obj = data2schema(obj, _force=True)

    def _processing(crude, request, _func=func, _obj=obj, **kwargs):

        if _func is None:
            crudename = crude.name
            funcname = crudename.split('.')[-1]
            _func = getattr(_obj, funcname)
            schemafunc = getattr(type(_obj), funcname)

        else:
            schemafunc = _func

        funckwargs = {}
        funcvarargs = []

        if isinstance(crude, Exe):
            funcvarargs = crude.params

        elif isinstance(crude, (Create, Update)):
            funckwargs.update(crude.values)
            # todo : specific _func args

        for param in schemafunc.params:

            if param.name in request.ctx:

                funckwargs[param.name] = request.ctx[param.name]

        funcresult = list(_func(*funcvarargs, **funckwargs))

        if isinstance(crude, Read):
            read(read=crude, items=funcresult)

        request.ctx[crude] = funcresult

        return request

    return _processing


def obj2driver(
        obj,
        name=None,
        creates=None, reads=None, updates=None, deletes=None, exes=None
):
    """Convert an object to a driver.

    :param str name: driver name. obj type name by default.
    :param list creates: create function names to retrieve from the obj.
    :param list reads: read function names to retrieve from the obj.
    :param list updates: update function names to retrieve from the obj.
    :param list deletes: delete function names to retrieve from the obj.
    :param list exes: exe function names to retrieve from the obj.
    :rtype: FunctionalDriver
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
    fexes = []

    _locals = locals()
    # start to get information from annotations
    crudeannotations = set(
        _CRUDEAnnotation.get_annotations(obj) +
        _CRUDEAnnotation.get_annotations(fobj)
    )
    for crudeannotation in crudeannotations:
        for target in crudeannotation.targets:
            targetname = targetname
            fobjtarget = getattr(fobj, targetname)
            _locals['f{0}s'.format(crudeannotation.name)].append(fobjtarget)

    # then parse function parameters
    for crudename in (crudename.lower() for crudename in  CRUDE.__members__):

        crudes = _locals['{0}s'.format(crudename)]

        for crude in crudes:

            crudefunc = getattr(fobj, crude)

            fcrudefunc = func2crudeprocessing(crudefunc)

            _locals['f{0}s'.format(crudename)].append(fcrudefunc)

    # ensure fexe exist, otherwise, load all obj functions inside
    if not fexes:
        fexes = [func2crudeprocessing(obj=fobj)]

    return FunctionalDriver(
        name=fname,
        creates=fcreates,
        reads=freads,
        updates=fupdates,
        deletes=fdeletes,
        exes=fexes
    )


class DriverAnnotation(Annotation):
    """Generate a deriver from class content."""

    def __init__(
            self,
            name=None,
            creates=None, reads=None, updates=None, deletes=None, exes=None,
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
            return deleted objects.
        :param list exes: exe function names. Related functions must return
            function result.
        """

        super(DriverAnnotation, self).__init__(*args, **kwargs)

        self.name = name
        self.creates = creates
        self.reads = reads
        self.updates = updates
        self.deletes = deletes
        self.exes = exes

    def getdriver(self, obj):
        """Get a driver corresponding to input target instance related
        to this attributes.

        :param obj: instance to transform to a functional driver.
        :rtype: FunctionalDriver"""

        kwargs = {'name': self.name, 'obj': obj}

        for crude in (crude.lower() for crude in CRUDE.__members__):

            fcrude = '{0}s'.format(crude)

            funcnames = getattr(self, fcrude)

            for funcname in funcnames:
                func = getattr(obj, funcname)

                kwargs.setdefault(fcrude, []).append(func)

        return obj2driver(**kwargs)


class _CRUDEAnnotation(Annotation):

    def __init__(self, crude, *args, **kwargs):
        """
        :param str crude: crude name.
        """

        super(_CRUDEAnnotation, self).__init__(self)

        self.crude = crude.name if isinstance(crude, CRUDEElement) else crude


class CreateAnnotation(_CRUDEAnnotation):

    def __init__(self, crude=CRUDE.READ, *args, **kwargs):

        super(CreateAnnotation, self).__init__(crude=crude, *args, **kwargs)


class ReadAnnotation(_CRUDEAnnotation):

    def __init__(self, crude=CRUDE.READ, *args, **kwargs):

        super(ReadAnnotation, self).__init__(crude=crude, *args, **kwargs)


class UpdateAnnotation(_CRUDEAnnotation):

    def __init__(self, crude=CRUDE.UPDATE, *args, **kwargs):

        super(UpdateAnnotation, self).__init__(crude=crude, *args, **kwargs)


class DeleteAnnotation(_CRUDEAnnotation):

    def __init__(self, crude=CRUDE.DELETE, *args, **kwargs):

        super(DeleteAnnotation, self).__init__(crude=crude, *args, **kwargs)


class ExeAnnotation(_CRUDEAnnotation):

    def __init__(self, crude=CRUDE.EXE, *args, **kwargs):

        super(ExeAnnotation, self).__init__(crude=crude, *args, **kwargs)
