Ye Wenbin's Emacs Configuration
==========

How to install this .emacs?
------------------------

    git clone https://github.com/wenbinye/dot-emacs.git
    ln -sf dot-emacs .emacs.d

If the ~/.emacs already exists, delete it. Because we use ~/.emacs.d/init.el as init file.

Eim and PDE is a submodule in dot-emacs, you can use it by update the submodule:

    git submodule init
    git submodule update

Start emacs, and use M-x ywb-generate-loaddefs to create autoload config.
