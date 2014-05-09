nfs: Allegro NFS Server for Microsoft Windows in Common Lisp
============================================================

Table of contents
-----------------

 * Description
 * Author
 * Author comments
 * Documentation
 * Platforms
 * Dependencies
 * Installation
 * Configuration
 * Licence
 * Notes
 * Franz Inc. Open Source Info

Description
-----------

Allegro NFS Server for Microsoft Windows in Common Lisp

Authors
-------

Ahmon Dancy, Franz Inc.
Elliott Johnson, Franz Inc.
Kevin Layer, Franz Inc.

Author comments
---------------

Allegro® NFS Server for Windows® was inspired by our dissatisfaction
with current free and commercial NFS Servers available on the market
and the incredible technical difficulties we faced in configuring them
on Windows.

Platforms
----------

Microsoft Windows XP and newer (including Windows 8).

Dependencies
------------

Cygwin (GNU make).  Allegro Common Lisp 9.0 (32-bit, non-SMP).

Installation
------------

To build:

    make all

To install:

    make install

The install step expects that cygwin has C:\ mounted as /c.  It is
also possible to build an installer via:

    make installer

This will produce an _nsi_ file that can be used to install.

Configuration
-------------

See the [nfs documentation](http://www.nfsforwindows.com/home) for
more information on how to configure nfs.

Documentation
-------------

Once your exports are configured it's possible to mount them.  Please
consult your platforms documentation on how to mount remote nfs partitions.

Debugging
---------

Interactive debugging of server:

   :ld load.cl

Then, for debugging:

   (debugmain) ;; main.cl

Or, without debugging:

   (setf *configfile* "nfs.cfg")
   (read-nfs-cfg *configfile*)
   (startem)

*******************************************************************************

   :cd d:/src/nfs50/
   (load "load.cl")
   (setf *configfile* "nfs.cfg")
   (read-nfs-cfg *configfile*)
   (startem)

   (prof:start-profiler)

   (prof:stop-profiler)
   (defun doit (file)
      (with-open-file (*standard-output* file :direction :output
		       :if-exists :supersede)
        (prof:show-flat-profile)
        (prof:show-call-graph)
        #+ignore (prof:disassemble-profile 'excl::g-read-vector-2)))
   (doit "y:/nfs.82brc5")
   (doit "y:/nfs.81")

   (prof:show-flat-profile)
   (prof:show-call-graph)

License
-------

The nfs source code is licensed under the terms of the 
[Lisp Lesser GNU Public License](http://opensource.franz.com/preamble.html), 
known as the LLGPL. The LLGPL consists of a preamble and the LGPL. Where these 
conflict, the preamble takes precedence.  This project is referenced in the 
preamble as the LIBRARY.

Notes
-----

See the following files that are part of this project:

 * access-control.txt - info on controlling access to the nfs server
 * notes.txt - implementation notes
 * release-notes.txt - info about past and present nfs releases.
 * rfc1014.txt - XDR: External Data Representation Standard.
 * rfc1050.txt - rpc: Remote Procedure Call.
 * rfc1094.txt - NFS: Network File System Protocol Specification
 * rfc1813.txt - nfs version 3 protocol standard.
 * rfc1833.txt - Binding Protocols for ONC RPC Version 2
 * TODO.txt - old and new todo information.

Franz Open Source Info
----------------------

This project's homepage is <https://github.com/franzinc/nfs/>.
