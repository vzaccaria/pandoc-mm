
all: ./final.pdf

./final.pdf: ./src/Main.hs Category.org
	stack install . && cat Category.org | pandoc-mm

