(require 'asdf)
(push *default-pathname-defaults* asdf:*central-registry*) 
(asdf:oos 'asdf:load-op 'cl-zdiff :force t)
(save-lisp-and-die "cl-zdiff" :executable t :toplevel 'ztree.main:main)
