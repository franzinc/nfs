
## Allegro NFS Server for Windows

## Table of contents

 * [Introduction](#introduction)
 * [Installation](#installation)
 * [Configuration](#configuration)
 * [Release Notes](#release-notes)
 * [Implementation Notes](#implementation-notes)

## Introduction

The repo contains the source code for Allegro® NFS Server for Microsoft
Windows®, written in Allegro Common Lisp.  The author is Ahmon Dancy,
with help from Elliott Johnson, Kevin Layer, and Jeremiah Rothschild.

It was inspired by our dissatisfaction with current free and
commercial NFS Servers for Windows available on the market, and the
incredible technical difficulties we faced in configuring them on
Windows.

It runs on Microsoft Windows 7 and later (including Windows 8 and
Server 2008 and 2012).

This source code is licensed under the terms of the [Lisp Lesser GNU
Public License](http://opensource.franz.com/preamble.html), known as
the LLGPL. The LLGPL consists of a preamble and the LGPL. Where these
conflict, the preamble takes precedence.  This project is referenced
in the preamble as the LIBRARY.

## Installation

This software is open source, however some of the tools needed to
build it are not.  You can purchase a supported binary copy of this
software or download an evaluation version from
[nfsforwindows.com](http://nfsforwindows.com).

To build this software, you need Cygwin (for GNU make) and Allegro
Common Lisp 10.0 (32-bit, non-SMP).  The installer is written in
[NSIS](http://nsis.sourceforge.net/Main_Page).

To build:

    make all

To install:

    make install

The install step expects that cygwin has `C:\` mounted as `/c`.  It is
also possible to build an installer via:

    make installer

This will produce an _exe_ file that can be used to install.

## Configuration

See the [documentation](http://nfsforwindows.com/support-documentation)
for more information on how to configure nfs.

Also see `doc/configuration.txt`.

Once your exports are configured it's possible to mount them.  Please
consult your client platform's documentation on how to mount the
exported NFS shares.

## Release Notes

### UNRELEASED CHANGES

Changes in this section will appear in the next release of Allegro
NFS.

TBD

### Version 6.3.2 [TBD July, 2017]

* Fix: It was possible for NFS clients to issue a readdir call with a
  requested max response size which is too large to fit into a UDP
  datagram.  Such requests are now handled without creating a
  too-large response.

### Version 6.3.1 [TBD July, 2017]

* Fix: correct a regression which resulted in empty directory listings
  for directories containing special files (such as pagefile.sys).

### Version 6.3 [12 July, 2017]

* Add configuration option for badly behaved clients that cannot
  handle 64-bit file-ids; when turned on, file-ids will be truncated
  to 32-bits.

* Fix: if the "nsm-state" file becomes corrupted, then handle this
  more gracefully by reseting its contents.

* Fix: readdir*() calls no longer takes a really long time on large
  directories.

* Fix: no longer show negative fractional seconds in debug output.

* Fix: install: detect when a reboot is pending and abort.

* Fix: uninstall: detect when a reboot is needed and offer to reboot.

### Version 6.2 [7 December, 2016]

* Fix: use an Allegro CL build that fixes a GC leak, which could
  result in the server process growing until it runs out of space.
  This bug was only present in version 6.1. 

* Fix: handle "tcp connection reset" errors, which previously made the
  server die and require manual restart.  They are now ignored.

* For silent installs (/S given to installer) do not start the
  configuration program at the end.

* Directory entries are now returned in the order in which Windows
  returns them.  Previously, directory listing were reversed.

* The server can now detect when files or directories are renamed
  externally (i.e., not by an NFS client) and update its cache
  accordingly.  Previously it could get into a confused state if files
  or directories were renamed externally.

### Version 6.1 [21 Jul, 2016]

* Log possible conflicting NFS server running if an existing (aka
  system) portmapper is detected.

* Fix: adjust the nfsd error handling so that certain errors do not
  cause the service to exit, and return reasonable errors to the
  client.

* Fix: adjustments in heap placement for Windows 10.  There should be
  fewer problems with service startup due to ASLR.

* Fix: increased general client compatibility.

* Fix: improved compatibility with the Windows Services For Unix (SFU)
  NFS client.

* Fix: don't display configuration window until config has been
  loaded.  This will help avoid confusion in cases where the NFS
  server is busy and it takes a while to get a response from it.

* Fix: handle full filesystem errors, returning the proper ENOSPC
  error code to the client.

* Fix: allow statfs and fsstat procedures on filenames (as opposed to
  a directories).

### Version 6.0 [27 Mar, 2015]

* Added support for persistent file handles for files on NTFS
  volumes. This means that you can restart Allegro NFS or the machine
  that it is running on and not suffer stale NFS file handle errors on
  the client. Non-NTFS files (or inaccessible files on NTFS volumes)
  still use non-persistent file handles are still subject to stable
  NFS file handle errors after restarts.

* Allow control over how long file attribute are cached through the
  configuration GUI. See the "File attribute caching time" setting in
  the Global tab of the Allegro NFS configuration program.

* Configuration for "Host lists" allows host names in the "New
  address" field. When loading the configuration and resolving the
  host names, users will be warned if the conversion of saved host
  names fails, and the host name will be ignored.

* When constructing file mode bits for reporting to an NFS client,
  Allegro NFS uses the filename extension to determine if the file
  should have the executable (x) bits set. Previously only files with
  extension .exe, .com, or .bat were marked executable. This list is
  now adjustable by modifying nfs.cfg and adding an entry for
  *executable-types*. This operation should only be performed by
  advanced users.

* File attributes are cached for a maximum of 5 seconds. Prior to this
  change, the expiration for cached file attributes would be extended
  each time they were accessed. This could be a problem in the
  following scenario:

  * an NFS client is repeatedly calling stat() on a file to see if its
    attributes had changed, and

  * the attributes of the file were altered outside of Allegro NFS's
    knowledge.

  In this case the new file attributes would never be returned, at
  least not until the client stopped probing for a sufficiently long
  time.

* Use higher precision timestamps in log messages, enabled via an
  option on the debug tab.

* Change default statfs blocksize from 8192 to 512. This improves
  compatibility with some broken NFS clients which do not work
  properly if the blocksize is not 512.

* Minor change to showmount output.

* General performance and stability improvements.

* Fix: input validation bugs in the Allegro NFS configuration program.

* Fix: mounting a subdirectory of an export. For example, if you have
  an export named "/export" which has a subdirectory named "files",
  then mounting servername:/export/files from an NFS client now works
  properly.

* Fix: file and directory changes on the server weren't seen by some
  clients. This would make files appear to be static even though they
  had been modified on the server.

* Fix: some previously invisible files are now seen by clients, such
  as in the roots of filesystems, System Volume Information and
  pagefile.sys.


### Version 5.1 [1 Aug, 2011]

* performance improvements
* minor license changes
* new: toggling of response to showmount requests.
* new: configurable log rotation.  Defaults to previous behavior.
* new: directory and file caching duration is now configurable.
* fix: interoperability fixes with VMware ESXi.
* fix: hanging of the server and 100% cpu usage problems are removed.
* fix: moving the error log into the install directory.
* fix: proper locking around exports and logging operations.
* small modifications to icons.

### Version 5.0 [22 Feb, 2010]

* performance improvements
* better support Windows Vista/7
* new: support for symbolic links
* new: support for international filenames (UTF-8)
* new: support for mount protocol version 2
* new console separate from service, that is shows log info
* added NLM and NSM debug options
* add date to timestamp logging
* New option `*nfs-set-mtime-on-write*` allows users to work around
  Windows' funny behavior with respect to file modification times.
  Normally Windows does not update the mtime on a file until the file
  is closed.  Since Allegro NFS keeps a file open while it is
  actively being written-to, a Windows program periodically checking
  the mtime of the file will not be able to tell that the file is
  actually being modified until the file is closed.  Updating the
  mtime during every write access suffers about a 10% penality so the
  option is disabled by default.
* improvements to hard link support
* better compatibility between UNIX and Windows-style locking
* improved interoperatibility with several UNIX clients
* many other small improvements and fixes

### Version 4.5 [20 June, 2007]

* Windows Vista Support
* Many interoperability fixes and bug fixes
* Improved locking support
* NLM and NSM port numbers can now be set manually

### Version 4.4 [29 Aug, 2006]

* NFS lock support
* Bug fixes
* Enhanced debugging

### Version 4.3 [27 Oct, 2005]

* Installs on systems with Windows Data Execution Prevention (DEP) turned on
* allow specification of mountd port number for dealing with firewall issues
* made mountd subprocess more robust in the face of malformed RPC messages
* adjusted the logging of some "normal" error conditions so they look
  less alarming
* better performance
* bug fixes 

### Version 4.0 [10 Aug, 2005]

* NFS protocol V3 support
* large file support
* better performance 
* Hard link creation support

### Version 3.0 [22 Apr, 2004]

* handle UNC pathnames
* added "use system portmapper" option 
* improved performance

### Version 2.0 [24 Feb, 2004]

* new configuration utility 

### Version 1.1.4 [3 Jul, 2003]

* improved interoperability with some clients
* configuration option for altering mode bits 

### Version 1.1.3 [21 Mar, 2003]

* bug fixes 

### Version 1.1.1 [23 Jan, 2003]

* Improvements of handling the NFS services 

### Version 1.0.36 [23 Sep, 2002]

* bug fixes 

### Version 1.0.33 [27 Feb, 2002]

* initial release 

# Implementation Notes

The following files in the `doc` subdirectory have various information
related to understanding how to debug and understand the product:

 * `access-control.txt` - info on controlling access to the nfs server
 * `configuration.txt` - info on configuration
 * `debugging.txt` - notes on debugging
 * `notes.txt` - implementation notes
 * `profiling.txt` - how to profile the server
 * `testing.txt` - information on testing
 * `todo.txt` - old and new todo information.
