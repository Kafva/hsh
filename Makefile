EXEC=sha256

$(EXEC): src/*.hs
	ghc $< -o $@

clean:
	rm -f $(EXEC) **/*.o **/*.hi

