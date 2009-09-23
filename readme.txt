Interactive debugging of server:

:ld loadem

Then, for debugging:

  (debugmain) ;; main.cl

Or, without debugging:

  (setf *configfile* "nfs.cfg")
  (read-nfs-cfg *configfile*)
  (startem)

*******************************************************************************

:cd d:/src/nfs46/
(load "loadem")
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


