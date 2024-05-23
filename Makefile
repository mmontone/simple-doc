all: README.md docs/index.html

README.md:
	sbcl --load './simple-doc.asd' \
             --eval '(require :simple-doc)' \
             --eval '(simple-doc:generate-markdown-doc #p"README.md" :simple-doc)' \
             --quit

docs/index.html:
	sbcl --load './simple-doc.asd' \
             --eval '(require :simple-doc)' \
             --eval '(simple-doc:generate-html-doc #p"docs/index.html" :simple-doc)' \
             --quit

clean:
	rm -f README.md
	rm -f docs/index.html
