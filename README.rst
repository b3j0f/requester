Description
-----------

Abstract data request library powered by the python syntax.

.. image:: https://img.shields.io/pypi/l/b3j0f.requester.svg
   :target: https://pypi.python.org/pypi/b3j0f.requester/
   :alt: License

.. image:: https://img.shields.io/pypi/status/b3j0f.requester.svg
   :target: https://pypi.python.org/pypi/b3j0f.requester/
   :alt: Development Status

.. image:: https://img.shields.io/pypi/v/b3j0f.requester.svg
   :target: https://pypi.python.org/pypi/b3j0f.requester/
   :alt: Latest release

.. image:: https://img.shields.io/pypi/pyversions/b3j0f.requester.svg
   :target: https://pypi.python.org/pypi/b3j0f.requester/
   :alt: Supported Python versions

.. image:: https://img.shields.io/pypi/implementation/b3j0f.requester.svg
   :target: https://pypi.python.org/pypi/b3j0f.requester/
   :alt: Supported Python implementations

.. image:: https://img.shields.io/pypi/wheel/b3j0f.requester.svg
   :target: https://travis-ci.org/b3j0f/requester
   :alt: Download format

.. image:: https://travis-ci.org/b3j0f/requester.svg?branch=master
   :target: https://travis-ci.org/b3j0f/requester
   :alt: Build status

.. image:: https://coveralls.io/repos/b3j0f/requester/badge.png
   :target: https://coveralls.io/r/b3j0f/requester
   :alt: Code test coverage

.. image:: https://img.shields.io/pypi/dm/b3j0f.requester.svg
   :target: https://pypi.python.org/pypi/b3j0f.requester/
   :alt: Downloads

.. image:: https://readthedocs.org/projects/b3j0frequester/badge/?version=master
   :target: https://readthedocs.org/projects/b3j0frequester/?badge=master
   :alt: Documentation Status

.. image:: https://landscape.io/github/b3j0f/requester/master/landscape.svg?style=flat
   :target: https://landscape.io/github/b3j0f/requester/master
   :alt: Code Health

Links
-----

- `Homepage`_
- `PyPI`_
- `Documentation`_

Installation
------------

pip install b3j0f.requester

Features
--------

Examples
--------

Refers to a data
~~~~~~~~~~~~~~~~

.. code-block:: python

   from b3j0f.requester import Expression as E, Function as F

   # ways to refers to the field 'user.id'.
   E.user.id
   E('user.id')
   E('user').id

   # ways to refers to the function 'count' on the data 'user'.
   F.count(E.user)
   F('count')(E.user)
   F('count', params=[E.user])

   # In a multi system use, a system is seen such as a data:
   # access to users from a system administration.
   E.Administration.user
   E('Administration.user')
   E('Administration').user

Create data from a system
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

   from b3j0f.requester import Driver, Creation as C

   class MyDriver(Driver):
      """implement your own driver"""

   driver = MyDriver()

   req = driver.request()

   # create data of type A
   req.create(C.A({'id': 1, 'name': 'test'}))
   req.create(C('A', {'id': 1, 'name': 'test'}))


Read data from a system
~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

   from b3j0f.requester import Read as R

   req &= E.A.id == E.B.id & F.now > E.B.timestamp
   result = req.sort(E.A.id).groupby(E.A.name).join('FULL').select('*')[:10]  # get max 10 data, sorted by A.id and grouped by A.name

   for data in result:  # display A and B
      print(data['A'], data['B'])

   # another way to get same data
   read = R(limit=10, groupby=E.A.name, join='FULL', sort=E.A.name)
   data = req.read(read)

Update data from a system
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

   from b3j0f.requester import Update as U

   # update a user
   req.update(U.user({'name': 'john'}))
   req.update(U('user', {'name': 'john'}))
   req.user = {'name': 'john'}
   req['user'] = {'name': 'john'}

Delete data from a system
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

   from b3j0f.requester import Delete as D

   # delete a user
   req.delete(D.user)
   req.delete(D('user'))

Create data
~~~~~~~~~~~

.. code-block:: python

   from b3j0f.requester import Assignment as A

   req.create(A.A({'id': F.now, 'name': F.concat}))

Perspectives
------------

- wait feedbacks during 6 months before passing it to a stable version.
- Cython implementation.

Donation
--------

.. image:: https://liberapay.com/assets/widgets/donate.svg
   :target: https://liberapay.com/b3j0f/donate
   :alt: I'm grateful for gifts, but don't have a specific funding goal.

.. _Homepage: https://github.com/b3j0f/requester
.. _Documentation: http://b3j0frequester.readthedocs.org/en/master/
.. _PyPI: https://pypi.python.org/pypi/b3j0f.requester/
.. _annotation: https://github.com/b3j0f/annotation
