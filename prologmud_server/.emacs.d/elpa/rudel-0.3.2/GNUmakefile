EMACS = emacs -Q --batch

# FIXME: We keep rudel-loaddefs.el under VCS control.  This is contrary to
# normal and recommended practice, but it's due to the fact that the GNU
# ELPA scripts only generate rudel-autoloads.el and only consider files in
# the top-level directory for that (otherwise, maybe we could rely on
# buffer-locally setting generated-autoload-file in the non-main files).

rudel-loaddefs.el: $(shell find \( -name .git -prune \) -o \( -name '*.el' -print \) | \
	                       egrep -v 'rudel-(pkg|autoloads|loaddefs)')
	$(EMACS) \
	    --eval '(setq generate-autoload-cookie ";;;###rudel-autoload")' \
	    --eval '(setq generated-autoload-file (expand-file-name "$@"))' \
	    -f batch-update-autoloads \
	    $$(find \( -name .git -prune \) -o \( -name '*.el' -print \) | sed 's|/[^/]*$$||' | sort -u)
