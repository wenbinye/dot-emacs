REPO=https://github.com/wenbinye

cd /tmp
git clone $REPO/emacs-eim.git
cd emacs-eim
make install ELISPDIR=$HOME/.emacs.d/site-lisp/eim
