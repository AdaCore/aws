.. _Resources:

*********
Resources
*********

.. index:: Resources
.. index:: Self dependant

`AWS` support embedded resources. It means that it is possible to build
a fully self dependent executable. This is useful when distributing a
server. The server program contains the code but also the images (`PNG`,
`JPEG`, `GIF`), the templates, the `HTML` pages... more
generally any file the Web Server must serve to clients.

.. _Building_resources:

Building resources
==================

.. index:: Building resources

To embbed the files into the executable you must build a resource
tree. This task is greatly simplified using :file:`AWSRes` tool. For
example let's say that you want to build a simple server with a single
page containing some text and one PNG image. The text is handled
directly in the callback procedure and contain a reference to the
image :file:`logo.png`. To build the resource tree::

  $ awsres logo.png

This will create a set of packages whose root is the unit `res` by
default. The resource tree is created. See :ref:`awsres_tool` for the
complete AWS's usage description.

`awsres` can also compress the resource files. This can be done
by using `awsres`'s `-z` option. Compressed resources are
handled transparently. If the Web client supports compression the
resource is sent as-is otherwise a decompression stream will be
created for the resource to be decompressed on-the-fly while sending it.

.. _Using_resources:

Using resources
===============

.. index:: Using resources

This is really the simplest step. The resource tree must be linked
with your executable, to do so you just have to 'with' the
resource tree root into one of your program unit. This will ensure
that the resource tree will be compiled and linked into the
executable. `AWS` and `Templates_Parser know` about resource
files and will pick them up if available.

Note that this is transparent to users. It is possible to build the
very same server based on standard files or resources files. The only
change in the code is to 'with' or not the resource tree.

Note that `AWS` supports only a single resource tree. If more
than one resource tree is included into a program only one will be
seen.

.. _Stream_resources:

Stream resources
================

.. index:: Stream resources

Users can build a response directly from a stream. In this case the
callback answer is built using `AWS.Response.Stream`. It creates a
resource object whose operations have been inherited from
`AWS.Resource.Stream.Stream_Type` and redefined by the user. So
the `Read` operation can dynamically create the result stream
data, the `End_Of_File` operation must returns `True` when the
stream data is out and so on. This feature is useful to let users completely
create and control dynamically `AWS`'s response content.

See :ref:`AWS.Resources.Streams`.

.. _awsres_tool:

awsres tool
===========

.. index:: awsres

`AWSRes` is a tool to build resource files. It creates a root package
named :file:`res` by default and a child package for each resource
file::

  Usage: awsres [-hopqrRuz] file1/dir1 [-uz] [file2/dir2...]

*-a*
  packages are named after the actual filenames

*-h*
  Display help message.

*-o*
  Specify the output directory, by default it is the current directory.

*-p name*
  Append the specified prefix to the resource names.

*-q*
  Quiet mode.

*-R*
  Activate recursive behavior. In this mode :file:`awsres` will parse
  recursively all subdirectories. If a directory is specified on the
  command line then all files in this directory and sub-directories
  will be added. If a file (possibly a pattern) is specificed on the
  command line then only files matching in directroy and
  sub-directories will be added.

*-r name*
  Set the root unit name. Default is `res`.

*-u*
  Add following files as uncompressed resources.

*-z*
  Add following files as compressed resources.
