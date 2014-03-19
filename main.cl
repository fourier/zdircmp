(require 'cl-ncurses)

(cl-ncurses:initscr)
(cl-ncurses:printw "Hello, cl-ncurses")
(cl-ncurses:refresh)
(cl-ncurses:getch)
(cl-ncurses:endwin)
