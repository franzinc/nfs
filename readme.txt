Allegro NFS server documentation

Table of Contents:

1. Installation
   A. from source code
   B. using the supplied binaries
2. Configuration

$Id: readme.txt,v 1.11 2003/01/23 21:21:47 layer Exp $

*******************************************************************************
1. Installation

The Allegro NFS server (aNFSd) runs on Windows in Allegro Common
Lisp.

You can either build aNFSd from sources or use the binaries built by
Franz Inc.  If you want to build your own, then you must have Allegro
Common Lisp Enterprise Edition.

aNFSd works with a fully patched Allegro CL 6.2.  Evaluate
(sys:update-allegro) in a running 6.2 image to download and install
patches.

*******************************************************************************
1A. Installation: from source code

To build the NFS service, you need the ntservice package also from
opensource.franz.com.  This package is included with aNFSd.  If you
want to use your own version, you'll need to edit loadem.cl and change
the *ntservice.fasl* parameter to point to your own ntservice.fasl.

i. Start Lisp and evaluate:

	:ld loadem.cl
	(loadem)

   If you just want to test out the server, evaluate `(main)'.  The
   NFS server will start.  Give it a whirl.

   If you're satisfied, you'll probably want to turn the program into
   a full fledged service (the remaining steps).

ii. If you have GNU make (via cygwin) installed, then type "make" at a
   DOS or Bash prompt.  If not, startup ACL and evaluate (buildit).
   This will make an `nfs' sub-directory within your source directory.

   NOTE: if you use the makefile, nfs.cfg will be put into the `nfs'
   directory.  Otherwise, you will need to copy this file by hand.

iii. We recommend moving this newly created directory to somewhere
   like c:\nfs, the example used here.

*******************************************************************************
2. Configuration

Make a copy of nfs.cfg.sample and call it nfs.cfg.

Edit nfs.cfg and replace the values of the parameters to your liking.

*nfslocaluid* controls what user id the NFS server uses to specify the
 owner of all exported files.  Remote users with this user id will
 have write permission to all exported files.

*nfslocalgid* controls what group id the NFS server uses to specify
 the group of all exported files.  Currently, the NFS server does not
 use the group id when determining local file write access.

*nfslocalumask* controls which file mode bits are cleared before
 reporting them to the remote client.

*exports* controls what directories are exported and by what name they
 are known.  Each entry in the list is a list of two elements.  The
 first element in an entry is the filesystem name that the client will
 use.  The second element in an entry is the local directory that the
 name maps to.

*hosts-allow* and *hosts-deny* are explained in access-control.txt

*******************************************************************************
3. Execution

The NFS server can be started manually just by running the nfs.exe
that came with the prebuilt binaries or the nfs.exe that you created
using the sources.

The NFS server can also be installed as a Window service.  This will
make it start up automatically at boot time.  To install the NFS
server as a service, run nfs.exe with the /install switch.  For
example:

c:\nfs\nfs.exe /install

A window will be displayed to show you the results of the service
installation.  Under normal circumstances, it will say "NFS service
successfully installed."  If it doesn't work, it will try to tell you
why.

Run nfs.exe with the /remove switch to uninstall it as a service.  For
best results, stop the service (using the Services control panel
applet) before uninstalling it.
