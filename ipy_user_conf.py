""" User configuration file for IPython

This is a more flexible and safe way to configure ipython than *rc files
(ipythonrc, ipythonrc-pysh etc.)

This file is always imported on ipython startup. You can import the
ipython extensions you need here (see IPython/Extensions directory).

Feel free to edit this file to customize your ipython experience.

Note that as such this file does nothing, for backwards compatibility.
Consult e.g. file 'ipy_profile_sh.py' for an example of the things
you can do here.

See http://ipython.scipy.org/moin/IpythonExtensionApi for detailed
description on what you could do here.
"""

# Most of your config files and extensions will probably start with this import

import IPython.ipapi
ip = IPython.ipapi.get()

import os

def main():

    # uncomment if you want to get ipython -p sh behaviour
    # without having to use command line switches
    # import ipy_profile_sh

    import ipy_editors
    # emacsclient
    ipy_editors.install_editor("emacsclient -nw +$line $file")
    # or at least vim
    ipy_editors.install_editor("vim +$line $file")


    o = ip.options

    # automatically call callables without parens
    o.autocall = True
    # don't be verbose on system calls
    o.system_verbose = False
    # don't ask if you should quit
    o.confirm_exit = False
    # start editing on syntax errors
    o.autoedit_syntax = True

    ip.ex('import os, sys')
    ip.ex('import itertools as it')

    # future has landed
    o.autoexec.append('from __future__ import division, print_function, '
                      'unicode_literals, absolute_import')

    # For autoreloading of modules (%autoreload, %aimport)
    import ipy_autoreload

    # Tab completer that is not quite so picky (i.e.
    # "foo".<TAB> and str(2).<TAB> will work). Complete
    # at your own risk!
    import ipy_greedycompleter

    import readline
    # set treshold for `n possibility' query higher
    readline.parse_and_bind('set completion-query-items 1000')
    # don't ask me if you should display more
    readline.parse_and_bind('set page-completions no')


# some config helper functions you can use
def import_all(modules):
    """ Usage: import_all("os sys") """
    for m in modules.split():
        ip.ex("from %s import *" % m)

def execf(fname):
    """ Execute a file in user namespace """
    ip.ex('execfile("%s")' % os.path.expanduser(fname))

main()
