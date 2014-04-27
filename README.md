CL-ztree-diff
=============
cl-ztree-diff (cl-zdiff) is a text-mode directory-diff tool inspired by commercial tools like Beyond Compare or Araxis Merge.
It is an attempt to remake [ztree-diff](https://github.com/fourier/ztree) as a stand-alone ncurses-based application, therefore it is implemented in Common Lisp.
Currenly it is tested only on OSX/SBCL platform/compiler combination.

Requirements
============
In order to build one need to have installed and available:
* [SBCL](http://www.sbcl.org) - install through your package manager
* [CL-FAD](http://weitz.de/cl-fad/) - install through [quicklisp](http://www.quicklisp.org)
* [CL-NCURSES](http://common-lisp.net/project/cl-ncurses/) - install through [quicklisp](http://www.quicklisp.org)

In order to run on Debian Stable, one need to perform the following under root:
<pre>
ln -s /lib/i386-linux-gnu/libncurses.so.5 /usr/local/lib/libncurses.so
</pre>
for i386, since the ***libncurses*** is not in the one of search paths in cl-ncurses.
See [here](http://stackoverflow.com/questions/17416504/unable-to-load-libncurses-with-uffi) for the same problem.

Build instructions
==================
Run **make**

Usage
=====
Example:
<pre>
./cl-zdiff dir1 dir2
</pre>

