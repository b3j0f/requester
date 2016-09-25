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

from ..expr import Expression, Function, FuncName

from numbers import Number

from six import iteritems


class FuncNameTest(UTCase):

    def test_contains(self):

        for member in FuncName.__members__.values():

            self.assertTrue(FuncName.contains(member.value))


class ExpressionTest(UTCase):

    def test_init(self):

        name = 'test'

        expr = Expression(name=name)

        self.assertEqual(name, expr.name)
        self.assertIsNone(expr.alias)

    def test_getattr(self):

        expr = Expression.A.B.C

        self.assertEqual(expr.name, 'A.B.C')

    def _assertfunc(self, func, funcname, *params):

        self.assertIsInstance(func, Function)
        self.assertEqual(funcname.value, func.name)
        self.assertEqual(list(params), func.params)

    def _asserttype(self, expr, method, other=None):

        self.assertRaises(TypeError, getattr(expr, method), other)

    def test__and__(self):

        expr0, expr1 = Expression(name=''), Expression(name='')

        func = expr0 & expr1

        self._assertfunc(func, FuncName.AND, expr0, expr1)

    def test__or__(self):

        expr0, expr1 = Expression(name=''), Expression(name='')

        func = expr0 | expr1

        self._assertfunc(func, FuncName.OR, expr0, expr1)

    def test__gt__(self):

        expr = Expression(name='')

        func = expr > 1

        self._assertfunc(func, FuncName.GT, expr, 1)

        self._asserttype(expr, '__gt__')

    def test__ge__(self):

        expr = Expression(name='')

        func = expr >= 1

        self._assertfunc(func, FuncName.GE, expr, 1)
        self._asserttype(expr, '__ge__')

    def test__lt__(self):

        expr = Expression(name='')

        func = expr < 1

        self._assertfunc(func, FuncName.LT, expr, 1)
        self._asserttype(expr, '__lt__')

    def test__le__(self):

        expr = Expression(name='')

        func = expr <= 1

        self._assertfunc(func, FuncName.LE, expr, 1)
        self._asserttype(expr, '__le__')

    def test__eq__(self):

        expr0, expr1 = Expression(name=''), Expression(name='')

        func = expr0 == expr1

        self._assertfunc(func, FuncName.EQ, expr0, expr1)

    def test__ne__(self):

        expr0, expr1 = Expression(name=''), Expression(name='')

        func = expr0 != expr1

        self._assertfunc(func, FuncName.NE, expr0, expr1)

    def test__add__(self):

        expr = Expression(name='')

        func = expr + 1

        self._assertfunc(func, FuncName.ADD, expr, 1)
        self._asserttype(expr, '__add__')

    def test__sub__(self):

        expr = Expression(name='')

        func = expr - 1

        self._assertfunc(func, FuncName.SUB, expr, 1)
        self._asserttype(expr, '__sub__')

    def test__mul__(self):

        expr = Expression(name='')

        func = expr * 1

        self._assertfunc(func, FuncName.MUL, expr, 1)
        self._asserttype(expr, '__mul__')

    def test__truediv__(self):

        expr = Expression(name='')

        func = expr / 1

        self._assertfunc(func, FuncName.DIV, expr, 1)
        self._asserttype(expr, '__truediv__')

    def test__invert__(self):

        expr = Expression(name='')

        func = ~expr

        self._assertfunc(func, FuncName.INVERT, expr)

    def test__neg__(self):

        expr = Expression(name='')

        func = -expr

        self._assertfunc(func, FuncName.NEG, expr)

    def test__abs__(self):

        expr = Expression(name='')

        func = abs(expr)

        self._assertfunc(func, FuncName.ABS, expr)

    def test__mod__(self):

        expr = Expression(name='')

        func = expr % r''

        self._assertfunc(func, FuncName.LIKE, expr, '')
        self._asserttype(expr, '__mod__')

    def test__pow__(self):

        expr = Expression(name='')

        func = expr ** 1

        self._assertfunc(func, FuncName.POW, expr, 1)
        self._asserttype(expr, '__pow__')

    def test__radd__(self):

        expr = Expression(name='')

        func = 1 + expr

        self._assertfunc(func, FuncName.ADD, 1, expr)
        self._asserttype(expr, '__radd__')

    def test__rsub__(self):

        expr = Expression(name='')

        func = 1 - expr

        self._assertfunc(func, FuncName.SUB, 1, expr)
        self._asserttype(expr, '__rsub__')

    def test__rmul__(self):

        expr = Expression(name='')

        func = 1 * expr

        self._assertfunc(func, FuncName.MUL, 1, expr)
        self._asserttype(expr, '__rmul__')

    def test__rtruediv__(self):

        expr = Expression(name='')

        func = 1 / expr

        self._assertfunc(func, FuncName.DIV, 1, expr)
        self._asserttype(expr, '__rtruediv__')

    def test__rmod__(self):

        expr = Expression(name='')

        func = expr % expr

        self._assertfunc(func, FuncName.LIKE, expr, expr)
        self._asserttype(expr, '__rmod__')

    def test__rpow__(self):

        expr = Expression(name='')

        func = 1 ** expr

        self._assertfunc(func, FuncName.POW, 1, expr)
        self._asserttype(expr, '__rpow__')

    def test__getslice__(self):

        expr = Expression(name='')

        func = expr[1:2:3]

        self._assertfunc(func, FuncName.GETSLICE, expr, 1, 2, 3)
        self._asserttype(expr, '__getslice__')

    def test__setslice__(self):

        expr = Expression(name='')

        func = expr.__setslice__(1, 2, [])

        self._assertfunc(func, FuncName.SETSLICE, expr, 1, 2, [])
        self._asserttype(expr, '__setslice__')

    def test__delslice__(self):

        expr = Expression(name='')

        func = expr.__delslice__(1, 2)

        self._assertfunc(func, FuncName.DELSLICE, expr, 1, 2)
        self._asserttype(expr, '__delslice__')

    def test_copy(self):

        expr = Expression(name='name', alias='alias')

        cexpr = expr.copy()

        self.assertIsNot(expr, cexpr)
        self.assertEqual(expr.name, cexpr.name)
        self.assertEqual(expr.alias, cexpr.alias)


class FunctionTest(UTCase):

    def test_init_default(self):

        func = Function(name='')

        self.assertEqual(func.params, [])

    def test_init(self):

        params = [1, 2]

        func = Function(name='name', params=params)

        self.assertEqual(func.params, params)

    def test_call(self):

        params = [1, 2]

        func = Function(name='name')(1, 2)

        self.assertEqual(func.params, params)

if __name__ == '__main__':
    main()
