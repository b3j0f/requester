from b3j0f.annotation import Annotation
from b3j0f.schema import data2schema, FunctionSchema

from .base import CustomDriver

from ..request.crude.create import Create
from ..request.crude.read import Read
from ..request.crude.update import Update
from ..request.crude.delete import Delete
from ..request.crude.exe import Exe

from inspect import isclass

from enum34 import Enum


def func2processing(func):

    def _processing(cruder, request, func=func, **kwargs):

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
        obj, create=None, read=None, update=None, delete=None, exe=None
):
    """Convert an object to a driver."""

    if not isinstance(obj, Schema):
        fobj = data2schema(obj)

    else:
        fobj = obj

    _locals = locals()

    for crudr in ('create', 'read', 'update', 'delete', 'exe'):
        crudrfunc = _locals[crudr]

        finalname = 'f{0}'.format(crudr)

        if crudrfunc is not None:
            _locals[finalname] = func2processing(crudrfunc)

        else:
            _locals[finalname] = None

    return CustomDriver(
        create=fcreate,
        read=fread,
        update=fupdate,
        delete=fdelete,
        exe=fexe
    )


class ObjectDriver(CustomDriver):

    def __init__(self, impl, *args, **kwargs):
        """
        :param impl: driver implementation.
        :param create: create function.
        :param read: read function.
        :param update: update function.
        :param delete: delete function.
        :param exe: exe function.
        """

        super(ObjectDriver, self).__init__(*args, **kwargs)

        self.impl = impl



    def process(self, request, **kwargs):

        result = request

        for crude in request.crudes:

            crudename = type(crude).__name__.lower()

            func = getattr(self, crudename)

            if func is None:
                if crudename == CRUDE.EXE:
                    self.impl =


            if func is not None:
                result = self.processing(crude=crude, request=result, **kwargs)

            else:
                if

        return result

    def processing(self, crude, request, func, **kwargs):

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


class DriverAnnotation(Annotation):
    """Generate a deriver from class content."""

    def __init__(
            self, create=None, read=None, update=None, delete=None, exe=None,
            *args, **kwargs
    ):

        super(DriverAnnotation, self).__init__(*args, **kwargs)

        self.create = create
        self.read = read
        self.update = update
        self.delete = delete
        self.exe = exe

        self._drivers = {}

    def _on_bind_target(self, target, ctx=None):

        if not isinstance(target, Schema):
            ftarget = data2schema(target)

        else:
            ftarget = target

        driverkwargs = {}

        for crude in CRUDE.__members__:

            if getattr(self, crude) is not None:
                schemas[crude] = getattr(ftarget, crude)

        crudeannotations = (
            CRUDEAnnotation.get_annotations(target, mindepth=1) +
            CRUDEAnnotation.get_annotations(ftarget, mindepth=1)
        )

        for crudeannotation in crudeannotations:
            driverkwargs

    def process(self, request, **kwargs):

        for crude in request.crudes:

            if isinstance(crude, Create):
                for driver in self.drivers:



class CRUDEAnnotation(Annotation):

    def __init__(self, crude, *args, **kwargs):
        """
        :param
        """

        super(CRUDEAnnotation, self).__init__(self)

        self.crude = crude.name if isinstance(crude, CRUDE) else crude


class CreateAnnotation(CRUDEAnnotation):

    def __init__(self, crude=CRUDE.READ, *args, **kwargs):

        super(CreateAnnotation, self).__init__(crude=crude, *args, **kwargs)


class ReadAnnotation(CRUDEAnnotation):

    def __init__(self, crude=CRUDE.READ, *args, **kwargs):

        super(ReadAnnotation, self).__init__(crude=crude, *args, **kwargs)


class UpdateAnnotation(CRUDEAnnotation):

    def __init__(self, crude=CRUDE.UPDATE, *args, **kwargs):

        super(UpdateAnnotation, self).__init__(crude=crude, *args, **kwargs)


class DeleteAnnotation(CRUDEAnnotation):

    def __init__(self, crude=CRUDE.DELETE, *args, **kwargs):

        super(DeleteAnnotation, self).__init__(crude=crude, *args, **kwargs)


class ExeAnnotation(CRUDEAnnotation):

    def __init__(self, crude=CRUDE.EXE, *args, **kwargs):

        super(ExeAnnotation, self).__init__(crude=crude, *args, **kwargs)

