# GNU Make rules for fetching and byte-compiling various elisp files.

EMACS=emacs -q --no-site-file

.PHONY: compile package elpa

package: *.el
	@ver=`grep -o "Version: .*" history.el | cut -c 10-`; \
	tar cvf history-$$ver.tar `git ls-files '*.el' | xargs`

elpa: *.el
	@ver=`grep -o "Version: .*" history.el | cut -c 10-`; \
	dir=history-$$ver; \
	mkdir -p "$$dir"; \
	cp `git ls-files '*.el' | xargs` history-$$ver; \
	echo "(define-package \"history\" \"$$version\" \
	\"Modular in-buffer completion framework\")" \
	> "$$dir"/history-pkg.el; \
	tar cvf history-$$ver.tar "$$dir"

clean:
	@rm -rf history-*/ history-*.tar history-*.tar.bz2 *.elc

compile:
	${EMACS} -Q --batch -L . -f batch-byte-compile history.el

