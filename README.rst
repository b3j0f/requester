Description
-----------

Abstract data request library powered by the python syntax and reflective concerns.

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

This library aims to access to system data from a generic and python API.

Reflective concerns permit to not consider only data access with four create/read/update/delete operations but with a more one which is a service execution. Therefore, the main acronym of this library is CRUDE

In a minimal case, there are 6 concepts to know:

- Driver: in charge of accessing data.
- Expression and Function: refers to data models and system functions.
- Transaction: refers to data access transaction.
- Context: execution context such as a dict where keys are expressions, and values are system data.

Let a data models containing a table 'user' where fields are 'name' and 'age'.

A filter about users of age at least 10 is:

.. code-block:: python

   Expression.user.age > 10

A selection of number of users is:

.. code-block:: python

   Function.count(Expression.user)

Now, imagine you have two systems, called respectivelly Administration and Club. You might want to get users who have the same name and are at least twenty years old, in both systems like that:

.. code-block:: python

   (Expression.Administration.user.name == Expression.Club.user.name) & (Expression.user.age >= 20)

Therefore, all python operators are overriden by the object Expression in order to let you requests in a pythonic way.

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

   from b3j0f.requester import Driver

   class MyDriver(Driver):
      """implement your own driver..."""

   driver = MyDriver()

   # ways to create data from the request manager
   driver.create(name='C.user', values={'name': 'john'})
   driver.create(name=E.C.user, values={'name': 'john'})

   # create several data at once with method chaining and transaction
   with driver.open() as transaction:
      """transaction.create(...).update(...)"""

   The with ensure the transaction is commited or rollbacked in case of any error.

   trans = driver.open()

   # it is also possible to create a hierarchy of transaction with trans.open()

   trans.create('C.user', {'name': 'john'}).create(E.C.user, {'name': 'paul'}).process(Create('C.user', {'name': 'david'}), Create(E.C.user, {'name': 'thomas'})).commit()

   # create transaction with autocommit and with an historical context
   # autocommit and ctx can be changed at runtime
   trans = driver.open(autocommit=True, ctx=Context())

Read data from a system
~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

   from b3j0f.requester import Read as R, Join as J

   # get a read resource with specific offset
   crud = driver.read(offset=5)

   # add filters
   crud &= (E.A.id == E.B.id) & (F.now > E.B.timestamp)
   # same as
   crud.where(query)
   # and with a "or"
   crud.orwhere(query); crud |= query

   # method chaining and max 10 data, sorted by A.id and grouped by A.name
   result = crud.sortby(E.A.id).groupby(E.A.name).join('FULL').select()[:10]

   for data in result:  # display A and B
      print(data['A'], data['B'])

   # or get the result via a callback
   crud(async=True, callback=lambda result: None)

   # read data with a Read object
   read = R(limit=10, groupby=E.A.name, join=J.FULL, sort=E.A.name)
   result = trans.process(read).ctx[read]  # get context request which contain all data from systems and a transaction with autocommit

   # read data from the driver with default parameters
   AandB = driver['A', 'B']

Update data from a system
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

   from b3j0f.requester import Update as U

   # udpate data from the driver
   driver.update(name='user', values={'name': 'john'})
   driver.update(name=E.user, values={'name': 'john'})
   driver.update(name=E.user, values={'name': 'john'})
   driver[E.user] = {'name': 'john'}
   driver['user'] = {'name': 'john'}

   # update data from the transaction
   trans.update(name=E.user, values={'name': 'john'})
   trans.update('user', {'name': 'john'})
   trans['user'] = {'name': 'john'}
   trans[E.user] = {'name': 'john'}
   trans.process(U(name='user', values={'name': 'john'}))
   trans.process(U(name=E.user, values={'name': 'john'}))

Delete data from a system
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

   from b3j0f.requester import Delete as D

   # delete a user from a driver
   driver.delete(names=['user'], query=query)
   driver.delete(names=[E.user], query=query)
   del driver['user']
   del driver[E.user]

   # delete a user from a transaction
   trans.delete(names=[D.user], query=query)
   trans.delete(names=['user'], query=query)
   del trans['user']
   del trans[E.user]
   trans.process(names=[D('user')], query=query)
   trans.process(names=[D(E.user)], query=query)

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
