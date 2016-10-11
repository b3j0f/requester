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

"""join execution module."""

from copy import deepcopy

from enum import IntEnum, unique

__all__ = [
    'innerjoin', 'leftjoin', 'leftexjoin', 'rightjoin', 'rightexjoin',
    'fulljoin', 'fullexjoin', 'crossjoin', 'selfjoin', 'naturaljoin',
    'unionjoin', 'applyjoin'
]


@unique
class Join(IntEnum):

    INNER = 0  #: inner join.
    LEFT = 1  #: left join.
    LEFTEX = 2  #: left exclusive join.
    RIGHT = 3  #: right exclusive join.
    RIGHTEX = 4  #: right exclusive join.
    FULL = 5  #: full join.
    FULLEX = 6  #: full exclusive join.
    CROSS = 7  #: cross join.
    SELF = 8  #: self join.
    NATURAL = 9  #: natural join.
    UNION = 10  #: union join.


def innerjoin(litems, ritems):

    return [item for item in litems if item in ritems]


def leftjoin(litems, ritems):

    return litems


def leftexjoin(litems, ritems):

    return [item for item in litems if item not in ritems]


def rightjoin(litems, ritems):

    return ritems


def rightexjoin(litems, ritems):

    return [item for item in ritems if item not in litems]


def fulljoin(litems, ritems):
    """Apply full join on litems and rtimes.

    :param list litems:
    :param list ritmes:
    :return: new list of items.
    :rtype: list"""

    return litems + [item for item in ritems if item not in litems]


def fullexjoin(litems, ritems):

    return leftexjoin(litems, ritems) + rightexjoin(litems, ritems)


def crossjoin(litems, ritems):

    return [(litem, ritem) for litem in litems for ritem in ritems]


def selfjoin(litems, ritems):

    return crossjoin(litems, litems)


def naturaljoin(litems, ritems):

    result = []

    for litem in litems:

        for ritem in ritems:

            issame = False

            for key in ritem:

                if key in litem:
                    if litem[key] == ritem[key]:
                        issame = True

                    else:
                        break

            else:
                if issame:
                    item = deepcopy(litem)
                    item.update(deepcopy(ritem))
                    result.append(item)

    return result


def unionjoin(litems, ritems):

    return litems + ritems


_JOINBYNAME = {
    Join.INNER.name: innerjoin,
    Join.LEFT.name: leftjoin,
    Join.LEFTEX.name: leftexjoin,
    Join.RIGHT.name: rightjoin,
    Join.RIGHTEX.name: rightexjoin,
    Join.FULL.name: fulljoin,
    Join.FULLEX.name: fullexjoin,
    Join.CROSS.name: crossjoin,
    Join.SELF.name: selfjoin,
    Join.NATURAL.name: naturaljoin,
    Join.UNION.name: unionjoin
}


def applyjoin(litems, ritems, join=Join.FULL):

    if isinstance(join, Join):
        join = join.name

    func = _JOINBYNAME[join]

    return func(litems, ritems)
