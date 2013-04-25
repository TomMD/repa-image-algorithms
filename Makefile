all:
	ghc CV/LPR.hs -o LPR

clean:
	rm LPR *.jpg *.png

.PHONY : test

test:
	ghc CV/LPR.hs -o LPR
	./LPR test/Audi-A1.jpg

