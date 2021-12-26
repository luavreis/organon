(TeX-add-style-hook
 "diagramas-de-tensores"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("ulem" "normalem")))
   (TeX-run-style-hooks
    "latex2e"
    "imports/diagramas-de-tensores-preamble"
    "article"
    "art11"
    "graphicx"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "amssymb"
    "capt-of"
    "hyperref"
    "tikz"
    "tikz-cd")
   (LaTeX-add-labels
    "sec:orgdae6c24"
    "sec:org6cb00e4"
    "sec:orgaf1ea7a"
    "sec:org6f29ae5"
    "sec:orgc49b708"
    "sec:org70767f9"))
 :latex)

