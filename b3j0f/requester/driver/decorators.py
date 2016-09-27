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

"""Driver decorators module."""

from b3j0f.annotation import Annotation
from b3j0f.schema import data2schema
from b3j0f.schema.lang.python import FunctionSchema

from .utils import FunctionalDriver

from six import string_types

from ..request.crude.base import CRUDE


__all__ = [
    'object2driver', 'CreateAnnotation', 'ReadAnnotation', 'UpdateAnnotation',
    'DeleteAnnotation'
]

def _func2processing(func, obj=None):

    def _processing(crude, request, func=func, obj=obj, **kwargs):

        if func is None:
            func = getattr(obj, crude.name)

        funckwargs = {}

        for param in func.params():

            if param.name in request.ctx:
                funckwargs[param.name] = request.ctx[param.name]

        try:
            crude.result = func(**funckwargs)

        except TypeError:
            funckwargs.update(kwargs)

            crude.result = func(**funckwargs)

        return request

    return _processing


def object2driver(
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
    for crudename in (crudename.lower() for crudename in  CRUD.__members__):

        crudes = _locals['{0}s'.format(crudename)]

        for crude in crudes:

            crudefunc = getattr(fobj, crude)

            fcrudefunc = _func2processing(crudefunc)

            _locals['f{0}s'.format(crudename)].append(fcrudefunc)

    # ensure fexe exist, otherwise, load all obj functions inside
    if not fexes:
        fexes = [_func2processing(None, fobj)]

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


class _CRUDEAnnotation(Annotation):

    def __init__(self, crude, *args, **kwargs):
        """
        :param str crude: crude name.
        """

        super(_CRUDEAnnotation, self).__init__(self)

        self.crude = crude.name if isinstance(crude, CRUD) else crude


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
