ctfg: src/*.lisp *.asd
	sbcl --eval "(asdf:make :ctfg)" --quit

src/server.lisp: src/index.html
	touch src/server.lisp

check: ctfg
	rm -f tests/*.ts
	(cd tests; ./make_tests.sh 50;)
	npx playwright test

clean:
	rm -rf *~ ctfg events.db* scoreboard*.png tests/*.ts
