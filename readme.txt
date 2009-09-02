Interactive debugging of server:

:ld loadem
(debugmain) ;; main.cl

Or, if you want it without debugging turned on:

  (setf *configfile* "nfs.cfg")
  (read-nfs-cfg *configfile*)
  (startem)


