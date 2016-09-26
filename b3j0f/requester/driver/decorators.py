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

from .base import CustomDriver

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
        obj, name=None, create=None, read=None, update=None, delete=None, exe=None
):
    """Convert an object to a driver.

    """

    fname = type(obj).__name__ if name is None else name

    # build obj schema if necessary
    if not isinstance(obj, Schema):
        fobj = data2schema(obj)

    else:
        fobj = obj

    _locals = locals()
    # start to get information from annotations
    crudeannotations = set(
        _CRUDEAnnotation.get_annotations(obj) +
        _CRUDEAnnotation.get_annotations(fobj)
    )
    for crudeannotation in crudeannotations:
        for target in crudeannotation.targets:
            _locals['f{0}'.format(crudeannotation.name)] = target

    # then parse function parameters
    for crude in CRUD.__members__:

        fcrude = crude.lower()

        crudefunc = _locals[fcrude]

        if isinstance(crudefunc, string_types):
            crudefunc = getattr(fobj, crudefunc)

        finalname = 'f{0}'.format(fcrude)

        if crudefunc is not None:
            # ensure crudefunc is a schema
            if not isinstance(crudefunc, Schema):
                fcrudefunc = data2schema(crudefunc)

            else:
                fcrudefunc = crudefunc

            _locals[finalname] = _func2processing(fcrudefunc)

        else:
            _locals[finalname] = _locals.get(finalname)

    # ensure fexe exist, otherwise, load all obj functions inside
    if fexe is None:
        fexe = _func2processing(None, fobj)

    return CustomDriver(
        name=fname,
        create=fcreate,
        read=fread,
        update=fupdate,
        delete=fdelete,
        exe=fexe
    )


class DriverAnnotation(Annotation):
    """Generate a deriver from class content."""

    def __init__(
            self,
            name=None,
            create=None, read=None, update=None, delete=None, exe=None,
            *args, **kwargs
    ):
        """
        :param str name: driver name to generate. Default target type name.
        :param create: create function. If string, target function name.
        :type create: str or callable
        :param read: read function. If string, target function name.
        :type read: str or function
        :param update: update function. If string, target function name.
        :type update: str or function
        :param delete: delete function. If string, target function name.
        :type delete: str or function
        :param exe: exe function. If string, target function name.
        :type exe: str or function
        """

        super(DriverAnnotation, self).__init__(*args, **kwargs)

        self.name = name
        self.create = create
        self.read = read
        self.update = update
        self.delete = delete
        self.exe = exe

        self.drivers = []

    def _on_bind_target(self, target, ctx=None):

        driver = object2driver(
            target, name=self.name,
            create=self.create, read=self.read, update=self.update,
            delete=self.delete, exe=self.exe
        )

        self.drivers.append(driver)


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
