CL-ztree-diff
=============
cl-ztree-diff (cl-zdiff) is a text-mode directory-diff tool inspired by commercial tools like Beyond Compare or Araxis Merge.
It is an attempt to remake [ztree-diff](https://github.com/fourier/ztree) as a stand-alone ncurses-based application, therefore it is implemented in Common Lisp.
Currenly it is tested only on OSX/SBCL platform/compiler combination.

Requirements
============
In order to build one need to have installed and available:
* SBCL - install through your package manager
* CL-FAD - install through asdf-install or quicklisp
* CL-NCURSES - install through asdf-install or quicklisp

Build instructions
==================
Run '''make'''

Usage
=====
Example:
{{{
./cl-zdiff dir1 dir2
}}}

