**Configuring the NFS server**

Make a copy of nfs.cfg.sample and call it nfs.cfg

Edit nfs.cfg and replace the values of the parameters to your liking.

*nfslocaluid* controls what user id the NFS server uses to specify the
 owner of all exported files.  Remote users with this user id will have write
 permission to all exported files.

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



**Building the NFS server**

To build the NFS service, you'll need the ntservice package which you
can find on http://opensource.franz.com.  Grab the ntservice.cl source
file and compile it.  

Edit loadem.cl.  You'll need to change the *ntservice.fasl* parameter
to point to the ntservice.fasl file you created in the prior step.

Start lisp and load loadem.cl.  

Evaluate '(loadem)'

If you just want to test out the server, evaluate (main).  The NFS
server will start.  Give it a whirl.

If you're satisfied, you'll probably want to turn the program into a
full fledged service.  That's pretty easy.  Folow these steps:

Evaluate (buildit).  This will make an 'nfs' subdir within your source
directory.  You can move the new directory if you'd like.  Just make
sure you remember the pathname to the nfs.exe that was created by
'buildit'.  You'll need it for the next step.

[note: buildit will copy the nfs.cfg file from the source directory
into the binary directory]

Evaluate (create-service path) but replace 'path' with a string that
contains the path to the nfs.exe that was created.  If your sources
are in c:\devel\nfs, you'd specify:

(create-service "c:\\devel\\nfs\\nfs\\nfs.exe") ;; Note the use of the double backslash.  

If create-service returns 1, you've successfully registered the
program as a service.  To test it, make sure that no other NFS servers
are running (including one running in lisp.  You may want to exit the
lisp to be sure).  Now open the Services control panel.  Locate the
"nfs" service start it up.  Everything should work the same as it did
as as standalone application.


