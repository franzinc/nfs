Allegro NFS server documentation

Table of Contents:

1. Installation
   A. from source code
   B. using the supplied binaries
2. Configuration

$Id: readme.txt,v 1.8 2002/09/23 18:27:10 layer Exp $

*******************************************************************************
1. Installation

The Allegro NFS server (aNFSd) runs on Windows in Allegro Common
Lisp.

You can either build aNFSd from sources or use the binaries built by
Franz Inc.  If you want to build your own, then you must have Allegro
Common Lisp Enterprise Edition.

aNFSd works best with Allegro CL 6.1 and 6.2.

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

iv. Start ACL and evaluate:

	:ld loadem.cl
	(loadem)
	(create-service "c:\\nfs\\nfs.exe")
	(exit)

   If create-service returns 1, you've successfully registered the
   program as a service.  To test it, make sure that no other NFS
   servers are running (including one running in Lisp.  You may want
   to exit the Lisp to be sure).  Now open the Services control panel.
   Locate the "nfs" service and start it up.  Everything should work
   the same as it did as a standalone application.

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
