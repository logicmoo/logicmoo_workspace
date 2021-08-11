# Calling Pengines from Python

One of the advantages of Pengines is that it allows seamless access to a Prolog engine from other programming libraries. This can be useful when building large applications where not everyone is familiar with Prolog.

The [pengines](https://pypi.org/project/pengines/) library on PyPi provides a library for accessing a Pengines service from Python. It can be installed in the usual way:

  ==
  pip install pengines
  ==

Once you have installed this, and have a Pengines service running, you can write code like this:

  ==
  query = "member(X, [1,2,3])"
  pengine.ask(query)
  print(pengine.currentQuery.availProofs)
  ==
  
You can combine this with the [prologterms](https://pypi.org/project/prologterms/) package on PyPi to write more prolog-esque Python.

For complete documentation, including examples of running a server and writing programs from within Python, see the [PythonPengines on GitHub](https://github.com/ian-andrich/PythonPengines)

Examples of use:

 - [sparqlprog-python](https://github.com/cmungall/sparqlprog-python)
