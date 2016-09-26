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

- Driver: in charge of access to data.
- RequestManager: create request and apply them on a driver.
- Request: in charge of defining CRUDE operations.
- Expression: refers to data models.
- Function: refers to system functions.

Queries and CRUDE operations are done with Expression and Function objects.

Let a data models containing a table 'user' where fields are 'name' and 'age'.

A filter about users of age at least 10 is:

.. code-block:: python

   Expression.user.age > 10

A selection of number of users is:

.. code-block:: python

   Function.count(Expression.user)

Now, imagine you have two systems, called respectivelly Administration and Club. You might want to get users who have the same name and are at least twenty years old, in both systems like that:

.. code-block:: python

   Expression.Administration.user.name == Expression.Club.user.name & Expression.user.age >= 20

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

   from b3j0f.requester import RequestManager, Driver

   class MyDriver(Driver):
      """implement your own driver..."""

   requestmanager = RequestManager(driver=MyDriver())

   # ways to create data from the request manager
   requestmanager.create('C.user', {'name': 'john'})
   requestmanager.create(E.C.user, {'name': 'john'})

   # create several data at once with method chaining
   req = requestmanager.request()

   req.create('C.user', {'name': 'john'}).create(E.C.user, {'name': 'paul'}).process(Create('C.user', {'name': 'david'}), Create(E.C.user, {'name': 'thomas'})).commit()

   # create data from an historical request
   req = requestmanager.request(req)

Read data from a system
~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

   from b3j0f.requester import Read as R, Jointure as J

   # add queries
   req &= E.A.id == E.B.id & F.now > E.B.timestamp

   # read data from the request manager
   result = requestmanager.read((E.A, E.B), limit=10, jointure='FULL', groupby=E.A.name, sortby=E.A.id)

   for data in result:  # display A and B
      print(data['A'], data['B'])

   # read data from the request
   result = req.read((E.A, E.B), limit=10, jointure='FULL', groupby=E.A.name, sortby=E.A.id)

   # read data with method chaining
   result = req.sortby(E.A.id).groupby(E.A.name).jointure('FULL').select()[:10]  # get max 10 data, sorted by A.id and grouped by A.name

   # read data with a Read object
   read = R(limit=10, groupby=E.A.name, jointure=J.FULL, sort=E.A.name)
   result = req.processcrude(read).ctx[read.select]  # get context request which contain all data from systems

   # read data from the request manager with default parameters
   AandB = requestmanager['A', 'B']

   # read data from the request with default parameters
   AandB = requestmanager['A', 'B']

Update data from a system
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

   from b3j0f.requester import Update as U

   # udpate data from the request manager
   requestmanager.update('user', {'name': 'john'})
   requestmanager.update(E.user, {'name': 'john'})
   requestmanager.update(E.user, {'name': 'john'})
   requestmanager[E.user] = {'name': 'john'}
   requestmanager['user'] = {'name': 'john'}

   # update data from the request
   req.update(E.user, {'name': 'john'})
   req.update('user', {'name': 'john'})
   req['user'] = {'name': 'john'}
   req[E.user] = {'name': 'john'}
   req.process(U('user', {'name': 'john'}))
   req.process(U(E.user, {'name': 'john'}))

Delete data from a system
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

   from b3j0f.requester import Delete as D

   # delete a user from a requestmanager
   requestmanager.delete('user')
   requestmanager.delete(E.user)
   del requestmanager['user']
   del requestmanager[E.user]

   # delete a user from a request
   req.delete(D.user)
   req.delete('user')
   del req['user']
   del req[E.user]
   req.process(D('user'))
   req.process(D(E.user))

Run a service with parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

   from b3j0f.requester import Exe

   # let a system 'Kitchen' providing the service 'cook' with ingredients such as parameters...

   # run this service from a request manager
   requestmanager.run('Kitchen.cook', 'apple', 'pear')
   requestmanager.run(E.user.service, *E.Kitchen.fruits)
   requestmanager('Kitchen.cook', 'apple', 'pear')
   requestmanager(E.Kitchen.cook, *E.Kitchen.fruits)

   # run this service from a request
   req.run('Kitchen.cook', 'apple', 'pear')
   req.run(E.user.service, *E.Kitchen.fruits)
   req('Kitchen.cook', 'apple', 'pear')
   req(E.Kitchen.cook, *E.Kitchen.fruits)
   req.process(Exe('Kitchen.fruits', 'apple', 'pear'))
   req.process(Exe(E.Kitchen.fruits, params=['apple', 'pear']))

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
