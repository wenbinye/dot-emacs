REPO=https://github.com/wenbinye

cd $HOME
git clone $REPO/dot-emacs.git .emacs.d
cd /tmp
git clone $REPO/emacs-pde.git
cp -r emacs-pde/lisp $HOME/.emacs.d/site-lisp/pde
emacs -batch  -l $HOME/.emacs.d/init.el --eval '(ywb-generate-loaddefs)'
