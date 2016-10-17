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

"""Main configuration package."""

from .version import __version__

from .driver.base import Driver
from .driver.custom import (
    obj2driver, CreateAnnotation, ReadAnnotation, UpdateAnnotation,
    DeleteAnnotation
)
from .driver.composite import DriverComposite
from .driver.py import PyDriver
from .driver.transaction import Transaction
from .driver.ctx import Context

from .request.crud.base import CRUDElement
from .request.crud.create import Create
from .request.crud.delete import Delete
from .request.crud.read import Cursor, Read
from .request.crud.update import Update
from .request.crud.join import Join
from .request.base import BaseElement
from .request.consts import FuncName, CONDITIONS
from .request.expr import Expression, Function

__all__ = [
    '__version__',
    'BaseElement', 'FuncName', 'CONDITIONS', 'Expression', 'Function',
    'CRUDElement', 'Create', 'Read', 'Cursor', 'Update', 'Delete', 'Join',
    'Driver', 'obj2driver', 'DriverComposite', 'PyDriver', 'Transaction',
    'Context', 'CreateAnnotation', 'ReadAnnotation', 'UpdateAnnotation',
    'DeleteAnnotation'
]
