Pandoc-mm
=========

> Generate a mindmap from an org-mode file. Complete with annotations.

Foreword
========

This is just a tiny wrapper built on [the shoulders of a
giant](https://github.com/jgm/pandoc).

Example
=======

Generate this:

![image](./examples/Category.png)

from this:

``` org
$example
```

Installation
============

I'd strongly suggest to install it through
[Stack](https://docs.haskellstack.org/en/stable/README/):

``` sh
git clone https://github.com/vzaccaria/pandoc-mm.git
cd pandoc-mm 
stack install .
```

This should compile the binary and put it into something reachable in
your path. In my case this is `~/.local/bin`.

Usage
=====

        pandoc-mm FILE [ -x ]
        pandoc-mm --help | -h
        pandoc-mm --version

    Options:
        -x, --latex            Output raw latex
        -h, --help             Show help
        --version              Show version.

    Arguments
        FILE                   Org file containing the mindmap

Without the `-x` option, it generates a pdf with xelatex and pdfcrop.
Otherwise, it generates standalone latex you can compile with your own
toolchain.

Supported syntax
================

`:PROPERTIES:` can be the following:

-   `:color:` is any color in a syntax understandable by
    [Tikz](http://www.texample.net/tikz/). 
    
- `:placement:` absolute coordinates of the annotation. If not present, *i*) it is assumed to be placed below the previous annotation or *ii)* if it is the first annotation then it is assumed to be `current page.south east`. Use property `HELPLINES` (see commented text in the example above) to show a help grid in the pdf.


*Warning*: it turns out that the official org-mode grammar does not accept spaces between properties, or between properties and headings. These are thus invalid headings:

```
* Category

:PROPERTIES:
:color: blue!30
:END:       
```

or:
```
* Category
:PROPERTIES:
:color: blue!30

:END:       
```



Faq
===

-   Why? Just for fun.
-   What about markdown? PR welcome!

