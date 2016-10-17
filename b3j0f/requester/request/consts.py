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

"""Macro module."""

from enum import Enum, unique

__all__ = ['FuncName', 'CONDITIONS']


@unique
class FuncName(Enum):
    """Default function names which might be supported by drivers."""

    AND = '&'
    IAND = '&='
    OR = '|'
    IOR = '|='
    XOR = '^'
    IXOR = '^='
    TRUTH = 'truth'
    IS = 'is'
    ISNOT = 'isnot'
    NOT = '!'
    EQ = '=='
    NE = '!='
    GT = '>'
    GE = '>='
    LT = '<'
    LE = '<='
    ADD = '+'
    IADD = '+='
    SUB = '-'
    ISUB = '-='
    DIV = '/'
    IDIV = '/='
    FLOORDIV = '//'
    IFLOORDIV = '//='
    MUL = '*'
    IMUL = '*='
    POW = '**'
    IPOW = '**='
    LIKE = '%%'
    RSHIFT = '>>'
    IRSHIFT = '>>='
    LSHIFT = '<<'
    ILSHIFT = '<<='
    MOD = '%'
    IMOD = '%='
    NEG = 'neg'
    ABS = 'abs'
    INVERT = '~'
    EXISTS = 'exists'
    NEXISTS = 'nexists'
    GETSLICE = 'getslice'
    SETSLICE = 'setslice'
    DELSLICE = 'delslice'
    GETITEM = 'getitem'
    SETITEM = 'setitem'
    DELITEM = 'delitem'

    # remainders functions are not supported by the Expression methods
    ISNULL = 'isnull'
    BETWEEN = 'between'
    IN = 'in'
    NIN = 'nin'

    # selection operators  TODO: might be migrated to the Read object...
    HAVING = 'having'
    UNION = 'union'
    INTERSECT = 'intersect'

    # request comparison
    ALL = 'all'
    ANY = 'any'
    SOME = 'some'

    # DB operations
    OPTIMIZE = 'optimize'
    VERSION = 'version'

    # aggregation operations
    AVG = 'avg'
    COUNT = 'count'
    MEAN = 'mean'
    MAX = 'max'
    MIN = 'min'
    SUM = 'sum'

    # string operations
    CONCAT = 'concat'
    ICONCAT = 'iconcat'
    LENGTH = 'length'
    REPLACE = 'replace'
    SOUNDEX = 'soundex'
    SUBSTR = 'substr'
    SUBSTRING = 'substring'
    LEFT = 'left'
    RIGHT = 'right'
    REVERSE = 'reverse'
    TRIM = 'trim'
    LTRIM = 'ltrim'
    RTRIM = 'rtrim'
    LPAD = 'lpad'
    RPAD = 'rpad'
    UPPER = 'upper'
    LOWER = 'lower'
    UCASE = 'ucase'
    LCASE = 'lcase'
    LOCATE = 'locate'
    INSTR = 'instr'

    # mathematical operations
    RAND = 'rand'
    ROUND = 'round'
    MD5 = 'md5'

    # datetime operations
    NOW = 'now'
    SEC_TO_TIME = 'sec_to_time'
    DATEDIFF = 'datediff'
    MONTH = 'month'
    YEAR = 'year'

    # array operations
    INDEX = 'index'
    REPEAT = 'repeat'
    IREPEAT = 'irepeat'
    COUNTOF = 'countof'
    INCLUDE = 'include'

    # additional operations
    CAST = 'cast'
    CONVERT = 'convert'
    GROUPCONCAT = 'groupconcat'

    @staticmethod
    def contains(value):
        """True iif input value is a value of FuncName.

        :param str value: funcname value to check.
        :rtype: bool
        """
        result = False

        for member in FuncName.__members__.values():
            if member.value == value:
                result = True
                break

        return result


CONDITIONS = [  #: expression conditions.

    FuncName.OR.value,
    FuncName.AND.value,

    FuncName.IS.value,
    FuncName.ISNOT.value,
    FuncName.NOT.value,
    FuncName.EQ.value,
    FuncName.NE.value,

    # numerical functions
    FuncName.GT.value,
    FuncName.GE.value,
    FuncName.LT.value,
    FuncName.LE.value,

    FuncName.LIKE.value,

    FuncName.EXISTS.value,
    FuncName.NEXISTS.value,

    # remainders functions are not supported by the Expression methods
    FuncName.ISNULL.value,
    FuncName.BETWEEN.value,
    FuncName.IN.value,

    # selection operators  TODO: might be migrated to the Read object...
    FuncName.HAVING.value,

    # request comparison
    FuncName.ALL.value,
    FuncName.ANY.value,
    FuncName.SOME.value
]
