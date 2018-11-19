.PHONY : clean

app/wave.js : src/wave.elm
	elm make $< --output=$@

clean :
	rm app/wave.js
