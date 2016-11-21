
all: examples/Category.png README.md examples/Category.pdf

SRC=\
	src/Main.hs \
	src/MindMap \
	src/MindMap/Data.hs \
	src/MindMap/Print.hs \
	src/Test.hs \
	src/UsageCLI.hs \
	src/Utils.hs

BIN=.stack-work/dist/x86_64-osx/Cabal-1.22.5.0/build/pandoc-mm/pandoc-mm

$(BIN): $(SRC)
	stack build .

examples/%.pdf: examples/%.org $(BIN)
	stack exec pandoc-mm -- $<
	mv $*.pdf examples

examples/%.png: examples/%.pdf makefile 
	convert -density 300 -quality 200 -delete 1--1 $< $@

README.md: templates/readme.markdown examples/Category.org
	example=`cat examples/Category.org` envsubst < templates/readme.markdown > ./README.md


clean:
	rm -f README.md examples/*.pdf examples/*.png
