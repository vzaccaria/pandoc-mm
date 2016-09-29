
all: ./test/test.pdf

./test/tmp.tex: ./src/Main.hs Category.org
	stack install . && cat Category.org | pandoc-mm	> ./test/tmp.tex

./test/test.tex: ./test/header.tex ./test/tmp.tex ./test/final.tex
	cat $^ > $@


show:
	stack install . && cat Category.org | pandoc-mm 

./test/test.pdf: ./test/test.tex
	pdflatex ./test/test.tex
