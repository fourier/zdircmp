(require 'asdf)
(push *default-pathname-defaults* asdf:*central-registry*) 
(asdf:oos 'asdf:load-op 'cl-zdiff :force t)
;; eval-when necessary otherwise the compiler will barf or ztree.tui
(save-lisp-and-die "cl-zdiff" :executable t :toplevel 'ztree.tui:start-tui)
