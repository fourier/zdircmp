(require 'asdf)
(push *default-pathname-defaults* asdf:*central-registry*) 
(asdf:oos 'asdf:load-op 'zdircmp :force t)
(mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf))
(ql:quickload :swank)
(save-lisp-and-die "zdircmp" :executable t :toplevel 'zdircmp.main:main)
