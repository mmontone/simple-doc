# SIMPLE-DOC

Simple documentation generator for Common Lisp

## Functions
### generate-html-doc

```lisp
(output-filename package &key (css *default-css*)
 (output-undocumented *output-undocumented*) (use-readme *use-readme*)
 (kind-of-symbols :external))
```

Generates HTML doc for a package

- **output-filename**: A pathname or string. The documentation is written to that file.
- **package**: (package) The package for which to generate the documentation
- **css**: The css stylesheet.
- **output-undocumented**: If T, undocumented things appear on documentation.
- **use-readme**: If T, reads the readme file and appends it to docs
- **kind-of-symbols**: The symbols to appear in the docs. One of :external, :present or :accessible




### generate-markdown-doc

```lisp
(output-filename package &key (output-undocumented *output-undocumented*)
 (use-readme *use-readme*) (kind-of-symbols :external))
```

Generates Markdown doc for a package

- **output-filename**: A pathname or string. The documentation is written to that file.
- **package**: (package) The package for which to generate the documentation
- **output-undocumented**: (boolean) If T, enums undocumented things in generated doc.
- **kind-of-symbols**: Kind of symbols to appear in the doc. One of :external, :present or :accessible
