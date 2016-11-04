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

"""Expression module."""

from __future__ import division

from numbers import Number

from six import add_metaclass, string_types

from .base import BaseElement

from .consts import CONDITIONS, FuncName


__all__ = ['Expression', 'Function', 'MetaExpression']


class MetaExpression(type):
    """Meta class for function."""

    def __getattr__(self, key):
        """Instanciate a new cls expression for not existing attribute."""

        if key[-1] == '_':
            key = key[:-1]

        return self(name=key)


@add_metaclass(MetaExpression)
class Expression(BaseElement):
    """An expression is a reference to a data set.

    It has a name which is interpreted by drivers.

    Examples:

    - Expression('wheel'): expression named 'wheel'.
    - Expression('car.wheel'): expression 'wheel' from expression 'car'.
    - Expression('wheel', alias='wh'): expression aliased 'wh'.

    A simpler construction is possible with the __getattr__ (cls) method:

    Expression('human.eye') equals:

    - Expression.human.eye
    - Expression('human').eye

    If you want to use a name which equals to an Expression attribute name, use
    the suffix '_'...

    For example:

    .. code-block:: python

        assert Expression.human.name_.name == 'human.name'
        assert Expression.bar.as__.name == 'bar.as_'"""

    __slots__ = ['name'] + BaseElement.__slots__

    def __init__(self, name, *args, **kwargs):
        """
        :param str name: model name.
        """

        super(Expression, self).__init__(*args, **kwargs)

        self.name = name

    def __getattr__(self, key):
        """Generate a new expression where name is the concatenation of this
        name and key with a dot.

        If the key is one of this attribute name or reserved word, such as
        'name' or 'is', then you have to add '_' at the end of key.

        Example:

        - assert Expression.A.name == 'A.name'
        - assert Expression.A.name_.name == 'A.name'
        - assert Expression.A.is_ == 'A.is'

        :param str key: key to concat to this name. If you want to generate an
            expression where end name is the same as self attribute name, you
            can.

        :rtype: Expression
        """

        if key[-1] == '_':
            key = key[:-1]

        return type(self)(name='{0}.{1}'.format(self.name, key))

    def __and__(self, other):

        return Function(FuncName.AND)(self, other)

    def __or__(self, other):

        return Function(FuncName.OR)(self, other)

    def __xor__(self, other):

        return Function(FuncName.XOR)(self, other)

    def _checktype(self, other, *types):

        if not isinstance(other, types + (Expression,)):
            raise TypeError(
                'Wrong type {0}. {1}, Expression expected.'.format(
                    other, types
                )
            )

    def __mod__(self, other):
        """Regex comparison operator."""

        self._checktype(other, string_types)

        return Function(FuncName.LIKE)(self, other)

    def __gt__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.GT)(self, other)

    def __ge__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.GE)(self, other)

    def __lt__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.LT)(self, other)

    def __le__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.LE)(self, other)

    def __eq__(self, other):

        return Function(FuncName.EQ)(self, other)

    def __ne__(self, other):

        return Function(FuncName.NE)(self, other)

    def __add__(self, other):

        if isinstance(other, string_types):
            funcname = FuncName.CONCAT

        else:
            self._checktype(other, Number)
            funcname = FuncName.ADD

        return Function(funcname)(self, other)

    def __iadd__(self, other):

        if isinstance(other, string_types):
            funcname = FuncName.ICONCAT

        else:
            self._checktype(other, Number)
            funcname = FuncName.IADD

        return Function(funcname)(self, other)

    def __sub__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.SUB)(self, other)

    def __isub__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.ISUB)(self, other)

    def __mul__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.MUL)(self, other)

    def __imul__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.IMUL)(self, other)

    def __floordiv__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.FLOORDIV)(self, other)

    def __ifloordiv__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.IFLOORDIV)(self, other)

    def __div__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.DIV)(self, other)

    def __idiv__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.IDIV)(self, other)

    def __truediv__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.DIV)(self, other)

    def __itruediv__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.IDIV)(self, other)

    def __invert__(self):

        return Function(FuncName.INVERT)(self)

    def __neg__(self):

        return Function(FuncName.NEG)(self)

    def __abs__(self):

        return Function(FuncName.ABS)(self)

    def __contains__(self, other):

        return Function(FuncName.IN)(other, self)

    def __pow__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.POW)(self, other)

    def __ipow__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.IPOW)(self, other)

    def __rand__(self, other):

        return Function(FuncName.AND)(other, self)

    def __ror__(self, other):

        return Function(FuncName.OR)(other, self)

    def __rxor__(self, other):

        return Function(FuncName.XOR)(other, self)

    def __radd__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.ADD)(other, self)

    def __rsub__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.SUB)(other, self)

    def __rmul__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.MUL)(other, self)

    def __rdiv__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.DIV)(other, self)

    def __rfloordiv__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.FLOORDIV)(other, self)

    def __rtruediv__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.DIV)(other, self)

    def __rmod__(self, other):

        self._checktype(other, string_types)

        return Function(FuncName.LIKE)(other, self)

    def __rpow__(self, other):

        self._checktype(other, Number)

        return Function(FuncName.POW)(other, self)

    def __getitem__(self, key):

        if isinstance(key, slice):
            return self.__getslice__(key.start, key.stop, key.step)

        return Function(FuncName.GETITEM)(self, key)

    def __getslice__(self, start, stop, step):

        if start is not None:
            self._checktype(start, Number)

        if stop is not None:
            self._checktype(stop, Number)

        if stop is not None:
            self._checktype(stop, Number)

        return Function(FuncName.GETSLICE)(self, start, stop, step)

    def __setslice__(self, start, stop, value):

        if start is not None:
            self._checktype(start, Number)

        if stop is not None:
            self._checktype(stop, Number)

        self._checktype(value, list, tuple)

        return Function(FuncName.SETSLICE)(self, start, stop, value)

    def __delslice__(self, start, step):

        if start is not None:
            self._checktype(start, Number)

        if step is not None:
            self._checktype(step, Number)

        return Function(FuncName.DELSLICE)(self, start, step)

    def __setitem__(self, key, value):

        return Function(FuncName.SETITEM)(self, key, value)

    def __delitem__(self, key):

        return Function(FuncName.DELITEM)(self, key)

    def __rshift__(self, value):

        return Function(FuncName.RSHIFT)(self, value)

    def __irshift__(self, value):

        return Function(FuncName.IRSHIFT)(self, value)

    def __rrshift__(self, value):

        return Function(FuncName.RSHIFT)(value, self)

    def __lshift__(self, value):

        return Function(FuncName.LSHIFT)(self, value)

    def __ilshift__(self, value):

        return Function(FuncName.ILSHIFT)(self, value)

    def __rlshift__(self, value):

        return Function(FuncName.LSHIFT)(value, self)

    def __call__(self, *params):
        """Return a function where name is self name and params are varargs."""
        return Function(name=self.name, params=params)

    @property
    def ctxname(self):
        """Get ctx name to store result execution.

        :rtype: str
        """
        return self.alias or self.name or self.uuid

    def __repr__(self):

        result = '{0}'.format(self.name)

        if self.alias:
            result = '{0} as {1}'.format(result, self.alias)

        return result


class Function(Expression):
    """A function is an expression with parameters called 'params'.

    The property 'isfunc' permits to inform if this model is a function or
    not.

    Examples:

    - func = Function('count') => function 'count'.
    - func = Expression('A') < 2 => Function('<')(Expression('A'), 2).
    - func = Function('A', params=[2]) => function A and param equal 2.
    - func = Function('A')(2, 3) => function A from S1 and params 2 and 3.
    """

    __slots__ = ['params'] + Expression.__slots__

    def __init__(self, name, params=None, *args, **kwargs):
        """
        :param list params: model params in case of function.
        """

        name = name.value if isinstance(name, FuncName) else name

        super(Function, self).__init__(name=name, *args, **kwargs)

        self.params = [] if params is None else params

    def __call__(self, *params):
        """Transform this model into a function with parameters.

        If self is OR or AND function

        :param list params: parameters to use.
        """
        self.params = list(params)

        return self

    def optimize(self):
        """Aggregate AND/OR functions.

        Examples:

        - AND(AND(A, B), C) => AND(A, B, C)
        - OR(OR(A, B), C) => OR(A, B, C)

        :rtype: Function
        :return: this.
        """
        params = []

        if self.name in (FuncName.AND.value, FuncName.OR.value):

            for param in self.params:

                if isinstance(param, Function):
                    function = param.optimize()

                    if function.name == self.name:
                        params += function.params

                    else:
                        params.append(function)

                else:
                    params.append(param)

        self.params = params

        return self

    @property
    def ctxname(self):
        """Get ctx name to store result execution.

        :rtype: str
        """
        result = self.alias or self.uuid

        return result

    def __repr__(self):

        if len(self.params) > 1:

            if self.name in CONDITIONS:
                tojoin = ' {0} '.format(self.name)
                sparams = tojoin.join([repr(expr) for expr in self.params])

                result = '({0})'.format(sparams)

            else:
                sparams = ', '.join([repr(expr) for expr in self.params])
                result = '{0}({1})'.format(self.name, sparams)

        elif self.params:
            result = '{0}({1})'.format(self.name, repr(self.params[0]))

        else:
            result = '{0}()'.format(self.name)

        if self.alias:
            result += '{0} as {1}'.format(result, self.alias)

        return result
