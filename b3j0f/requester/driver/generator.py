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
    'func2crodprocessing', 'obj2driver', 'DriverAnnotation',
    'CreateAnnotation', 'ReadAnnotation', 'UpdateAnnotation', 'DeleteAnnotation'
]

from b3j0f.annotation import Annotation
from b3j0f.schema import data2schema, Schema
from b3j0f.schema.lang.python import FunctionSchema

from .utils import FunctionalDriver

from six import string_types

from ..request.crude.base import CRUDE
from ..request.crude.create import Create
from ..request.crude.update import Update
from ..request.crude.exe import Exe

from .py import processcrude


def func2crudeprocessing(func, obj=None):

    if func is not None and not isinstance(func, Schema):
        func = data2schema(func)

    if obj is not None and not isinstance(obj, Schema):
        obj = data2schema(obj)

    def _processing(crude, request, func=func, obj=obj, **kwargs):

        if func is None:
            crudename = crude.name
            funcname = crudename.split('.')[-1]
            func = getattr(obj, funcname)

        funckwargs = {}
        funcvarargs = []

        if isinstance(crude, Exe):
            varargs = crude.params

        elif isinstance(crude, (Create, Update)):
            funckwargs.update(crude.values)
            # todo : specific func args

        for param in func.params:

            if param.name in request.ctx:

                funckwargs[param.name] = request.ctx[param.name]

        funcresult = list(func(*funcvarargs, **funckwargs))

        processcrude(request=request, items=funcresult, crude=crude)

        request.ctx[crude] = funcresult

        return request

    return _processing


def obj2driver(
        obj,
        name=None,
        creates=None, reads=None, updates=None, deletes=None, exes=None
):
    """Convert an object to a driver.

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
        fexes = [func2crudeprocessing(None, fobj)]

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
        :param creates: create function names.
        :type creates: list
        :param reads: read function names.
        :type reads: list
        :param updates: update function names.
        :type updates: list
        :param deletes: delete function names.
        :type deletes: list
        :param exes: exe function names.
        :type exes: list
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
