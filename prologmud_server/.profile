# ~/.profile: executed by Bourne-compatible login shells.

# export LD_LIBRARY_PATH=/usr/lib/guile/2.2/extensions

if [ "$BASH" ]; then
  if [ -f ~/.bashrc ]; then
    . ~/.bashrc
  fi
fi

# mesg n || true
