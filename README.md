# SIMPLE-DOC

Simple documentation generator for Common Lisp

## Functions
### generate-html-doc (output-filename package &key (css \*default-css\*) (output-undocumented \*output-undocumented\*))
Generates HTML doc for a package

Arguments:

- **output-filename**: A pathname or string. The documentation is written to that file.
- **package**: (package) The package for which to generate the documentation
- **css**: The css stylesheet.


### generate-markdown-doc (output-filename package &key (output-undocumented \*output-undocumented\*))
Generates Markdown doc for a package

Arguments:

- **output-filename**: A pathname or string. The documentation is written to that file.
- **package**: (package) The package for which to generate the documentation
- **output-undocumented**: (boolean) If T, enums undocumented things in generated doc.


## Variables
### \*output-undocumented\*
If T, undocumented things appear on documentation.
