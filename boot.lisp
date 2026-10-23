;;;; boot.lisp: load the core and start. Run through run.sh, which supervises it.
(load (merge-pathnames "core.lisp" *load-truename*))
(lisping-core:main)
