# Use the host SBCL by default. Override with e.g. `make SBCL=sbcl ctfg`.
# (A Homebrew SBCL records CC=gcc-12 in its sbcl.mk, which can't link against
# the system glibc; the Fedora-packaged /usr/bin/sbcl uses the system gcc.)
SBCL ?= /usr/bin/sbcl

ctfg: src/*.lisp *.asd runtime-files.tgz
	$(SBCL) --eval "(asdf:make :ctfg)" --quit

runtime-files.tgz: css/ctfg.css js/app.js index.html $(if $(wildcard images/banner.jpg),images/banner.jpg,images/banner.png)
	tar cvfz $@ css/ctfg.css js/app.js $(if $(wildcard images/banner.jpg),images/banner.jpg,images/banner.png) index.html

src/main.lisp: runtime-files.tgz
	touch $@

check: ctfg
	rm -f tests/ctfg-player*.spec.ts
	(cd tests; ./make_tests.sh 50;)
	npx playwright test

clean:
	rm -rf *~ ctfg events.db* scoreboard*.png tests/*.ts
