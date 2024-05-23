README.md:
	sbcl --load './simple-doc.asd' \
             --eval '(require :simple-doc)' \
             --eval '(simple-doc:generate-markdown-doc #p"README.md" :simple-doc)' \
             --quit

clean:
	rm -f README.md
