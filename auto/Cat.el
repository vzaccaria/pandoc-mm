(TeX-add-style-hook
 "Cat"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("standalone" "border=10pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("hyperref" "hidelinks" "pdfencoding=auto")))
   (TeX-run-style-hooks
    "latex2e"
    "standalone"
    "standalone10"
    "inputenc"
    "dtklogos"
    "tikz"
    "hyperref")
   (TeX-add-symbols
    '("info" ["argument"] 3)))
 :latex)

