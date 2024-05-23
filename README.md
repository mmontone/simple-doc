# SIMPLE-DOC

Simple documentation generator for Common Lisp

## Functions
### generate-html-doc

```lisp
(destination package &key (css *default-css*)
 (output-undocumented *output-undocumented*) (use-readme *use-readme*)
 (kind-of-symbols :external))
```

Generates HTML doc for a package

- **destination**: A destination specifier. See WITH-OUTPUT-TO-DESTINATION.
- **package**: (package) The package for which to generate the documentation
- **css**: The css stylesheet.
- **output-undocumented**: If T, undocumented things appear on documentation.
- **use-readme**: If T, reads the readme file and appends it to docs
- **kind-of-symbols**: The symbols to appear in the docs. One of :external, :present or :accessible



### generate-markdown-doc

```lisp
(destination package &key (output-undocumented *output-undocumented*)
 (use-readme *use-readme*) (kind-of-symbols :external)
 (include '(:package :package-documentation)) (categories *categories*))
```

Generates Markdown doc for a package



Args: - DESTINATION (OR PATHNAME STRING STREAM NIL T): The documentation is written a stream created from the type of DESTINATION. See WITH-DESTINATION-STREAM.
         - PACKAGE (PACKAGE-DESIGNATOR): The package for which to generate the documentation
         - OUTPUT-UNDOCUMENTED (BOOLEAN): If T, enums undocumented things in generated doc.
         - KIND-OF-SYMBOLS: Kind of symbols to appear in the doc. One of :EXTERNAL, :PRESENT or :ACCESSIBLE.
         - CATEGORIES: The list of definition categories that should appear in the docs. Default is *CATEGORIES*, that includes all definition categories: (:FUNCTION :MACRO :GENERIC-FUNCTION :SLOT-ACCESSOR :VARIABLE :CLASS :CONDITION
 :CONSTANT)
         - INCLUDE: Controls what gets included in the output. Default is '(:PACKAGE :PACKAGE-DOCUMENTATION). If :PACKAGE appears in the INCLUDE list, the document will start with the PACKAGE name. If :PACKAGE-DOCUMENTATION appears in the INCLUDE list, then the PACKAGE docstring is added to the document.
