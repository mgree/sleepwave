.PHONY : deploy clean

app/wave.js : src/wave.elm
	elm make $< --output=$@

deploy : app/wave.js
	git subtree push --prefix app origin gh-pages

clean :
	rm app/wave.js
