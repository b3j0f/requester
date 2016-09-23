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

"""Request module."""

__all__ = ['Request']

from .model import Model
from .assign import Assignment
from .select import Selection


class Request(object):
    """CRUD/runnable object bound to a driver in order to access to data.

    It is instanciated from a driver."""

    def __init__(self, driver, ctx=None, filter=None, *args, **kwargs):
        """
        :param Driver driver: driver driving the request.
        """

        super(Request, self).__init__(*args, **kwargs)

        self._driver = driver
        self._ctx = ctx
        self._filter = filter

    def ctx(self, value):

        self._ctx = ctx

        return self

    def filter(self, model):

        self._filter = self._filter & model

        return self

    def orfilter(self, model):

        self._filter = self._filter | model

        return self

    def __getitem__(self, key):

        return Selection(request=self, select=key)()

    def __setitem__(self, key, value):

        if isinstance(key, (string_types, Model)):
            assignments = [Assignment(model=key, value=value)]

        else:
            if isinstance(value, tuple):
                assignments = [
                    Assignment(
                        model=model,
                        value=(
                            value[index] if isinstance(value, tuple) else value
                        )
                    )
                    for index, model in enumerate(assignments)
                ]

        return self.update(*assignments)

    def __delitem__(self, key):

        models = [key] if isinstance(key, (string_types, Model)) else key

        return self.delete(*key)

    def __iadd__(self, other):

        assignments = [other] if isinstance(other, Assignment) else other

        return self.create(*assignments)

    def __isub__(self, other):

        models = [other] if isinstance(other, (string_types, Model)) else other

        return self.delete(*models)

    def __iand__(self, other):

        return self.filter(other)

    def __ior__(self, other):

        return self.orfilter(other)

    def create(self, *assignments):

        return self.driver.create(request=self, *assignments)

    def read(self, selection=None):
        """Read input expressions.

        :param Selection selection: selection. All filtered data by default.
        :rtype: Cursor
        """

        return self.driver.read(request=self, selection)

    def update(self, *assignments):
        """Apply input assignments.

        :param tuple assignments: assignments to apply.
        """

        return self.driver.update(request=self, *assignments)

    def delete(self, *models):
        """Delete input models.

        :param tuple models: models to delete.
        :return: number of deleted models.
        """

        return self.driver.delete(request=self, *models)

    def run(self, model):
        """Run input model.

        :param Model model: model to run. Must contains params.
        :return: model result.
        """

        return  self.driver.run(request=self, model=model)

    def groupby(self, value):

        return Selection(request=self, groupby=value)

    def sort(self, value):

        return Selection(request=self, sort=value)

    def select(self, value):

        return Selection(request=self, select=value)

    def skip(self, value):

        return Selection(request=self, skip=value)

    def limit(self, value):

        return Selection(request=self, limit=value)

    def join(self, value):

        return Selection(request=self, join=value)
