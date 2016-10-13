#!/usr/bin/env python
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

"""conf file driver UTs."""

from unittest import main

from six import iteritems

from b3j0f.utils.ut import UTCase

from ..custom import (
    CreateAnnotation, ReadAnnotation, UpdateAnnotation, DeleteAnnotation,
    obj2driver, CustomDriver, query2kwargs
)
from ..ctx import Context
from ..custom import func2crudprocessing
from ..transaction import Transaction, State
from ...request.crud.base import CRUD
from ...request.crud.create import Create
from ...request.crud.delete import Delete
from ...request.crud.read import Read
from ...request.crud.update import Update
from ...request.expr import Expression


class CustomDriverTest(UTCase):

    def setUp(self):

        self.driver = CustomDriver()
        self.maxDiff = None

    def test_default(self):

        driver = CustomDriver()

        self.assertFalse(driver.creates)
        self.assertFalse(driver.reads)
        self.assertFalse(driver.updates)
        self.assertFalse(driver.deletes)
        self.assertFalse(driver.functions)

    def test_read(self):

        def read(crud, transaction, **kwargs):
            self.assertIsInstance(crud, Read)
            self.assertIs(transaction.state, State.COMMITTING)
            return [['read', transaction, kwargs]]

        driver = CustomDriver(reads=[read, read])

        transaction = driver.read()

        self.assertEqual(len(transaction.ctx), 1)

        for name, value in iteritems(transaction.ctx):
            item = ['read', transaction, {'async': False}]
            self.assertEqual(value, [item, item])

    def test_create(self):

        def create(crud, transaction, **kwargs):
            self.assertIsInstance(crud, Create)
            self.assertIs(transaction.state, State.COMMITTING)
            return [['create', transaction, kwargs]]

        driver = CustomDriver(creates=[create, create])

        transaction = driver.create(values={})

        self.assertEqual(len(transaction.ctx), 1)

        for name, value in iteritems(transaction.ctx):
            item = ['create', transaction, {'async': False}]
            self.assertEqual(value, [item, item])

    def test_update(self):

        def update(crud, transaction, **kwargs):
            self.assertIsInstance(crud, Update)
            self.assertIs(transaction.state, State.COMMITTING)
            return [['update', transaction, kwargs]]

        driver = CustomDriver(updates=[update, update])

        transaction = driver.update(values={})

        self.assertEqual(len(transaction.ctx), 1)

        for name, value in iteritems(transaction.ctx):
            item = ['update', transaction, {'async': False}]
            self.assertEqual(value, [item, item])

    def test_delete(self):

        def delete(crud, transaction, **kwargs):
            self.assertIsInstance(crud, Delete)
            self.assertIs(transaction.state, State.COMMITTING)
            return [['delete', transaction, kwargs]]

        driver = CustomDriver(deletes=[delete, delete])

        transaction = driver.delete()

        self.assertEqual(len(transaction.ctx), 1)

        for name, value in iteritems(transaction.ctx):
            item = ['delete', transaction, {'async': False}]
            self.assertEqual(value, [item, item])


class FunctionalDriverTest(UTCase):

    def setUp(self):

        self.processed = {}

        def process(crud, count):

            def cprocess(transaction, **kwargs):

                self.processed.setdefault(crud.name, []).append(kwargs)

                transaction.ctx['count'] += 10 ** crud.value

                return transaction

            return [cprocess for _ in range(count)]

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

        query = Expression.func(1, 2, 3)
        crud = Read(None)

        transaction = Transaction(
            driver=None, query=query, ctx=Context({'b': 2})
        )

        _request = genfunc(crud=crud, transaction=transaction)

        self.assertIs(_request, transaction)

        self.assertEqual(_request.ctx[crud], [1, 2, 3])

    def test_object(self):

        class Test(object):

            def test(self, *params):

                return list(params)

        query = Expression.test(1, 2, 3)

        transaction = Transaction(
            driver=None, query=query, ctx=Context({'b': 1})
        )

        exe = Read('test')

        test = Test()

        func = func2crudprocessing(obj=test)

        func(transaction=transaction, crud=exe)

        self.assertEqual(transaction.ctx[exe], [1, 2, 3])


class Obj2DriverTest(UTCase):

    def test_gateway(self):

        self_ = self

        class Test(object):

            def __init__(self, *args, **kwargs):

                super(Test, self).__init__(*args, **kwargs)

                self.kwargs = []

            @CreateAnnotation()
            @ReadAnnotation()
            def cr(self, a, **kwargs):

                self_.assertEqual(a, {'>': [2]})

                return [{'cr': kwargs}]

            @ReadAnnotation()
            @UpdateAnnotation()
            def ru(self, a, **kwargs):

                self_.assertEqual(a, {'>': [2]})

                return [{'ru': kwargs}]

            @UpdateAnnotation()
            @DeleteAnnotation()
            def ud(self, a, **kwargs):

                self_.assertEqual(a, {'>': [2]})

                return [{'ud': kwargs}]

            @DeleteAnnotation()
            @CreateAnnotation()
            def dc(self, a, **kwargs):

                self_.assertEqual(a, {'>': [2]})

                return [{'dc': kwargs}]

            def test(self, **kwargs):

                self.kwargs.append(kwargs)

                return 'test'

        test = Test()

        driver = obj2driver(test)

        query = (Expression.a > 2) & Expression.test(Expression.test())

        transaction = driver.create(values={}, query=query)

        self.assertEqual(len(transaction.ctx), 3)
        self.assertEqual(len(test.kwargs), 2)

        transaction = driver.read(query=query)

        self.assertEqual(len(transaction.ctx), 6)
        self.assertEqual(len(test.kwargs), 4)

        transaction = driver.update(values={}, query=query)

        self.assertEqual(len(transaction.ctx), 3)
        self.assertEqual(len(test.kwargs), 6)

        transaction = driver.delete(query=query)

        self.assertEqual(len(transaction.ctx), 3)
        self.assertEqual(len(test.kwargs), 8)

    def test_gateway_or(self):

        self_ = self

        class Test(object):

            def __init__(self, *args, **kwargs):

                super(Test, self).__init__(*args, **kwargs)

                self.kwargs = []

            @CreateAnnotation()
            @ReadAnnotation()
            def cr(self, a=None, b=None, **kwargs):

                if a is not None:
                    self_.assertEqual(a, {'>': [2]})

                if b is not None:
                    self_.assertEqual(b, {'>': [3]})

                return [{'cr': kwargs}]

            @ReadAnnotation()
            @UpdateAnnotation()
            def ru(self, a=None, b=None, **kwargs):

                if a is not None:
                    self_.assertEqual(a, {'>': [2]})

                if b is not None:
                    self_.assertEqual(b, {'>': [3]})

                return [{'ru': kwargs}]

            @UpdateAnnotation()
            @DeleteAnnotation()
            def ud(self, a=None, b=None, **kwargs):

                if a is not None:
                    self_.assertEqual(a, {'>': [2]})

                if b is not None:
                    self_.assertEqual(b, {'>': [3]})

                return [{'ud': kwargs}]

            @DeleteAnnotation()
            @CreateAnnotation()
            def dc(self, a=None, b=None, **kwargs):

                if a is not None:
                    self_.assertEqual(a, {'>': [2]})

                if b is not None:
                    self_.assertEqual(b, {'>': [3]})

                return [{'dc': kwargs}]

            def test(self, **kwargs):

                self.kwargs.append(kwargs)

                return 'test'

        test = Test()

        driver = obj2driver(test)

        query = (Expression.a > 2) & Expression.test() | (Expression.b > 3)

        transaction = driver.create(values={}, query=query)

        self.assertEqual(len(transaction.ctx), 2)
        self.assertEqual(len(test.kwargs), 1)

        transaction = driver.read(query=query)

        self.assertEqual(len(transaction.ctx), 3)
        self.assertEqual(len(test.kwargs), 2)

        transaction = driver.update(values={}, query=query)

        self.assertEqual(len(transaction.ctx), 2)
        self.assertEqual(len(test.kwargs), 3)

        transaction = driver.delete(query=query)

        self.assertEqual(len(transaction.ctx), 2)
        self.assertEqual(len(test.kwargs), 4)

    def test_notgateway(self):

        class Test(object):

            def __init__(self, *args, **kwargs):

                super(Test, self).__init__(*args, **kwargs)

                self.kwargs = []

            @CreateAnnotation(gateway=False)
            @ReadAnnotation(gateway=False)
            def cr(self, **kwargs):

                return

            @ReadAnnotation(gateway=False)
            @UpdateAnnotation(gateway=False)
            def ru(self, **kwargs):

                return

            @UpdateAnnotation(gateway=False)
            @DeleteAnnotation(gateway=False)
            def ud(self, **kwargs):

                return

            @DeleteAnnotation(gateway=False)
            @CreateAnnotation(gateway=False)
            def dc(self, **kwargs):

                return

            def test(self, **kwargs):

                self.kwargs.append(kwargs)

                return 'test'

        test = Test()

        driver = obj2driver(test)

        query = Expression.a > Expression.test(Expression.test())

        transaction = driver.create(values={}, query=query)

        self.assertEqual(len(transaction.ctx), 3)
        self.assertEqual(len(test.kwargs), 2)

        transaction = driver.read(query=query)

        self.assertEqual(len(transaction.ctx), 3)
        self.assertEqual(len(test.kwargs), 4)

        transaction = driver.update(values={}, query=query)

        self.assertEqual(len(transaction.ctx), 3)
        self.assertEqual(len(test.kwargs), 6)

        transaction = driver.delete(query=query)

        self.assertEqual(len(transaction.ctx), 3)
        self.assertEqual(len(test.kwargs), 8)


class Query2KwargsTest(UTCase):

    def test_default(self):

        allkwargs = query2kwargs(query=None, ctx=Context(), pnames=[])

        self.assertEqual(allkwargs, [{}])

    def test_expr(self):

        allkwargs = query2kwargs(query=Expression.a, ctx=Context(), pnames=[])

        self.assertEqual(allkwargs, [{}])

    def test_expr_a(self):

        allkwargs = query2kwargs(
            query=Expression.a, ctx=Context(), pnames=['a']
        )

        self.assertEqual(allkwargs, [{'a': {'exists': []}}])

    def test_expr_a_b(self):

        allkwargs = query2kwargs(
            query=Expression.a & Expression.b & Expression.c,
            ctx=Context(), pnames=['a', 'b']
        )

        self.assertEqual(
            allkwargs,
            [{'a': {'exists': []}, 'b': {'exists': []}}]
        )

    def test_and(self):

        allkwargs = query2kwargs(
            query=(Expression.a > 2) & (Expression.a < 3),
            ctx=Context(),
            pnames=['a']
        )

        self.assertEqual(
            allkwargs,
            [{'a': {'>': [2], '<': [3]}}]
        )

    def test_or(self):

        allkwargs = query2kwargs(
            query=(Expression.a > 2) | (Expression.a < 3),
            ctx=Context(),
            pnames=['a']
        )

        self.assertEqual(
            allkwargs,
            [{'a': {'>': [2]}}, {'a': {'<': [3]}}]
        )

    def test_or_and(self):

        query = (Expression.a > 2) | ((Expression.a < 3) & (Expression.a == 4))

        allkwargs = query2kwargs(
            query=query,
            ctx=Context(),
            pnames=['a']
        )

        self.assertEqual(
            allkwargs,
            [{'a': {'>': [2]}}, {'a': {'<': [3], '==': [4]}}]
        )

if __name__ == '__main__':
    main()
