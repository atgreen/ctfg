ctfg: src/*.lisp *.asd
	sbcl --eval "(asdf:make :ctfg)" --quit

src/server.lisp: src/index.html src/challenges.json
	touch src/server.lisp

clean:
	rm -rf *~ ctfg
