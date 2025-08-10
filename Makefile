ctfg: src/*.lisp *.asd runtime-files.tgz
	sbcl --eval "(asdf:make :ctfg)" --quit

runtime-files.tgz: css/ctfg.css js/app.js images/banner.png index.html
	tar cvfz $@ css/ctfg.css js/app.js images/banner.png index.html

src/main.lisp: runtime-files.tgz
	touch $@

check: ctfg
	rm -f tests/ctfg-player*.spec.ts
	(cd tests; ./make_tests.sh 50;)
	npx playwright test

clean:
	rm -rf *~ ctfg events.db* scoreboard*.png tests/*.ts
