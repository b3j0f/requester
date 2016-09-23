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

"""Model module."""

__all__ = ['Model']


class Model(object):
    """A model is an object accessible from a system.

    It provides properties:
    - name: model name. Its sens is given by the request manager.
    - params: not None if this model is a function instanciation.

    Examples:

    - Model('A') => model named A
    - Model('A') < 2 <=> Model('&')(Model('A'), 2)
    - Model('S1.A') => model A from model S1.
    """

    AND = '&'
    OR = '|'
    EQ = '=='
    NE = '!='
    GT = '>'
    GE = '>='
    LT = '<'
    LE = '<='
    ADD = '+'
    SUB = '-'
    DIV = '/'
    MUL = '*'
    POW = '**'
    CONTAINS = '_contains'
    NCONTAINS = '_ncontains'
    LIKE = '_like'
    MOD = '%'
    COUNT = '_count'
    NEG = '_neg'
    ABS = '_abs'
    INVERT = '~'
    EXISTS = '_exists'


    def __init__(self, name, params=None, *args, **kwargs):
        """
        :param str name: model name.
        :param tuple params: model params in case of function.
        """

        super(Model, self).__init__(*args, **kwargs)

        self.model = model
        self.params = params

    def exists(self):

        return Model(Model.EXISTS)(self)

    def __call__(self, *params):
        """Transform this model into a function with parameters.

        :param tuple params: parameters to use."""

        self.params = []

        if self.name in (Model.AND, Model.OR):
            for param in self.params:
                if isinstance(param, Model) and param.name == self.name:
                    self.params += param.params

                else:
                    self.params.append(param)

        self.params = tuple(self.params)

        return self

    def __and__(self, other):

        return Model(Model.AND)(self, other)

    def __or__(self, other):

        return Model(Model.OR)(self, other)

    def __gt__(self, other):

        return Model(Model.GT)(self, other)

    def __ge__(self, other):

        return Model(Model.GE)(self, other)

    def __lt__(self, other):

        return Model(Model.LT)(self, other)

    def __le__(self, other):

        return Model(Model.LE)(self, other)

    def __eq__(self, other):

        return Model(Model.EQ)(self, other)

    def __ne__(self, other):

        return Model(Model.NE)(self, other)

    def __len__(self, other):

        return Model(Model.COUNT)(self, other)

    def __add__(self, other):

        return Model(Model.ADD)(self, other)

    def __sub__(self, other):

        return Model(Model.SUB)(self, other)

    def __mul__(self, other):

        return Model(Model.MUL)(self, other)

    def __div__(self, other):

        return Model(Model.DIV)(self, other)

    def __invert__(self):

        return Model(Model.INVERT)(self)

    def __contains__(self, other):

        return Model(Model.CONTAINS)(self, other)

    def __mod__(self, other):

        return Model(Model.MOD)(self, other)
