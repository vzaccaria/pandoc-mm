
all: examples/Category.png

examples/%.pdf: examples/%.org
	stack exec pandoc-mm -- $<
	mv $*.pdf examples

examples/%.png: examples/%.pdf makefile
	convert -density 300 -quality 200 -delete 1--1 $< $@


