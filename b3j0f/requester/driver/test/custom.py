#!/usr/bin/env python
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

"""conf file driver UTs."""

from b3j0f.utils.ut import UTCase

from unittest import main

from ..base import Driver
from ..custom import (
    func2crudprocessing, obj2driver, DriverAnnotation, CustomDriver
)
from ..ctx import Context
from ..transaction import Transaction
from ...request.crud.base import CRUD
from ...request.expr import Expression as E
from ...request.crud.create import Create
from ...request.crud.read import Read
from ...request.crud.update import Update
from ...request.crud.delete import Delete


class FunctionalDriverTest(UTCase):

    def setUp(self):

        self.processed = {}

        def process(crud, count):

            def _process(transaction, **kwargs):

                self.processed.setdefault(crud.name, []).append(kwargs)

                transaction.ctx['count'] += 10 ** crud.value

                return transaction

            return [_process for _ in range(count)]

        self.process = process

    def test_default(self):

        kwargs = {}

        count = 1

        for crud in CRUD.__members__.values():

            kwargs['{0}s'.format(crud.name.lower())] = self.process(
                crud, count
            )
            count += 1

        driver = CustomDriver(**kwargs)

        kwargs = {'foo': 'bar'}

        transaction = driver.open(
            ctx={'count': 0},
            cruds=[
                Create(None, None), Read(), Update(None, None), Delete()
            ]
        )
        transaction.ctx['count'] = 0

        result = driver.process(transaction=transaction, **kwargs)

        self.assertEqual(result.ctx['count'], 543210)

        count = 1

        for crud in CRUD.__members__:

            self.assertEqual(len(self.processed[crud]), count)

            processedkwargs = self.processed[crud]

            self.assertEqual(len(processedkwargs), count)

            for processedkwarg in processedkwargs:

                self.assertEqual(processedkwarg['foo'], kwargs['foo'])

            count += 1


class Func2CrudProcessingTest(UTCase):

    def test_function_create(self):

        def func(a, b):

            return [a + b]

        genfunc = func2crudprocessing(func)

        crud = Create(None, {'a': 1})

        transaction = Transaction(driver=None, ctx=Context({'b': 2}))

        _request = genfunc(crud=crud, transaction=transaction)

        self.assertIs(_request, transaction)
        self.assertEqual(_request.ctx[crud], [3])

    def test_function_read(self):

        def func(count):

            return [i for i in range(count)]

        genfunc = func2crudprocessing(func)

        crud = Read(offset=2, limit=2)

        transaction = Transaction(driver=None, ctx=Context({'count': 5}))

        _request = genfunc(crud=crud, transaction=transaction)

        self.assertIs(_request, transaction)
        self.assertEqual(_request.ctx[crud], [2, 3])

    def test_function_update(self):

        def func(a, b):

            return [a + b]

        genfunc = func2crudprocessing(func)

        crud = Update(None, {'a': 1})

        transaction = Transaction(driver=None, ctx=Context({'b': 2}))

        _request = genfunc(crud=crud, transaction=transaction)

        self.assertIs(_request, transaction)
        self.assertEqual(_request.ctx[crud], [3])

    def test_function_delete(self):

        def func():

            return []

        genfunc = func2crudprocessing(func)

        crud = Delete()

        transaction = Transaction(driver=None, ctx=Context({'b': 2}))

        _request = genfunc(crud=crud, transaction=transaction)

        self.assertIs(_request, transaction)
        self.assertEqual(_request.ctx[crud], [])

    def test_function_exe(self):

        def func(*params):

            return list(params)

        genfunc = func2crudprocessing(func)

        query = E.func(1, 2, 3)
        crud = Read(None)

        transaction = Transaction(driver=None, query=query, ctx=Context({'b': 2}))

        _request = genfunc(crud=crud, transaction=transaction)

        self.assertIs(_request, transaction)

        self.assertEqual(_request.ctx[crud], [1, 2, 3])

    def test_object(self):

        class Test(object):

            def test(self, *params):

                return list(params)

        query = E.test(1, 2, 3)

        transaction = Transaction(
            driver=None, query=query, ctx=Context({'b': 1})
        )

        exe = Read('test')

        test = Test()

        func = func2crudprocessing(obj=test)

        func(transaction=transaction, crud=exe)

        self.assertEqual(transaction.ctx[exe], [1, 2, 3])


class Obj2DriverTest(UTCase):
    pass

if __name__ == '__main__':
    main()
