.. image:: https://img.shields.io/github/license/Trundle/rexplector.svg
   :target: https://tldrlegal.com/l/apache2

==========
Rexplector
==========

A crude hack to visualize how OpenJDK matches regular expressions. The output
will be an HTML file that allows stepping through the matching process.

.. image:: https://raw.githubusercontent.com/Trundle/rexplector/master/docs/report.png


How to run
==========

::

   ./gradlew run

The generated trace will be written to a file ``report.html`` in the current
working directory.

Note that running Rexplector was only tested with OpenJDK 8, 9 and 10.


How does it work?
=================

OpenJDK matches regular expressions by transforming the regular expression into
an object tree. Every object in the tree represents a construct of the regular
expression (more or less). These objects are all derived from a common base
class (``Pattern.Node``) and override the ``match`` method for more specialised
behaviour.

Rexplector itself consists out of two parts: a runner (see the ``Runner`` class
in ``src/main/java``) and a tracer. The runner is a minimal Java application
that compiles a regular expressen and then matches a given input against the
compiled expression. The tracer then executes the runner in a subprocess and
attaches to it via the Java Debug Interface (JDI in short). Every invocation of
a ``match`` method is then intercepted and the arguments as well as the caller
are recorded.

After the runner subprocess exited, a report with a visualization of the
calltree is written to a file. The visualization is done with `vue.js
<http://visjs.org/>`_.


License
=======

Rexplector is released under the Apache License, Version 2.0 (see the
``LICENSE`` filer or http://www.apache.org/licenses/LICENSE-2.0.html).
