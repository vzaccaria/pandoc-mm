
all: ./test/test.pdf

./test/test.tex: ./src/Main.hs Category.org
	stack install . && cat Category.org | pandoc-mm	> $@

show:
	stack install . && cat Category.org | pandoc-mm 

./test/test.pdf: ./test/test.tex
	pdflatex ./test/test.tex
