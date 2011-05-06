===============================================
 aHg: An Emacs front-end for the Mercurial SCM
===============================================

:Author: Alberto Griggio
:Contact: agriggio@users.sourceforge.net


Description
-----------

aHg is a simple Emacs_ front-end for the Mercurial_ (Hg) Distributed Source
Control Management (SCM) system. 

Its aims are simplicity and ease of use. It was inspired by DVC_, but it
focuses exclusively on Mercurial instead of supporting multiple Distributed
SCMs.

For a list of features, see the `Quick Guide`_ section below.

.. _Emacs: http://www.gnu.org/software/emacs/
.. _Mercurial: http://selenic.com/mercurial/
.. _DVC: http://www.xsteve.at/prg/emacs_dvc/dvc.html


Download
--------

An hg repository can be found at http://bitbucket.org/agriggio/ahg/


Requirements
------------

- Emacs version 22 or higher
- Mercurial version 1.0 or higher (it might work with older versions, but I
  haven't tested it)


Installation
------------

Put ``ahg.el`` in a directory where Emacs can find it (e.g. in
``/usr/share/emacs/site-lisp``). Alternatively, you can explicitly add the
directory of ``ahg.el`` to Emacs' ``load-path``, by adding this to your
``.emacs`` file::

  (setq load-path (cons "/dir/of/ahg/" load-path))

Then, simply add this to your ``.emacs``::

  (require 'ahg)


Quick Guide
-----------

After the installation, an ``aHg`` menu appears as a child of the standard
``Tools`` menu of Emacs. The available commands are:

Status: 
   Shows the status of the current working directory, much like the
   ``cvs-examine`` command of PCL-CVS.

Log Summary:
   Shows a table with a short change history of the current working
   directory.

Detailed Log:
   Shows a more detailed change history of the current working directory.

Commit Current File:
   Commits the file you are currently visiting.

View Changes of Current File:
   Displays changes of current file wrt. the tip of the repository.

Mercurial Queues:
   Support for basic commands of the mq extension.

Execute Hg Command:
   Lets you execute an arbitrary hg command. The focus goes to the minibuffer,
   where you can enter the command to execute. You don't have to type ``hg``,
   as this is implicit. For example, to execute ``hg outgoing``, simply enter
   ``outgoing``. Pressing ``TAB`` completes the current command or file name.

Help on Hg Command:
   Shows help on a given hg command (again, use ``TAB`` to complete partial
   command names).
   

aHg buffers
~~~~~~~~~~~

The ``Status``, ``Log Summary``, ``Detailed Log`` and the ``List of MQ
Patches`` commands display their results on special buffers. Each of these has
its own menu, with further available commands.


Customization
~~~~~~~~~~~~~

There are some options that you can customize (e.g. global keybindings or
fonts). To do so, use ``M-x customize-group RET ahg``.


License
-------

The program is released under the `GNU GPL`__ License.

__ http://www.gnu.org/copyleft/gpl.html
