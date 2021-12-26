(TeX-add-style-hook
 "diagramas-de-tensores-preamble"
 (lambda ()
   (TeX-run-style-hooks
    "adjustbox"
    "amsmath"
    "amsthm"
    "mathtools"
    "amssymb"
    "extarrows"
    "xcolor-material"
    "tikz"
    "tikz-cd"
    "pgfplots")
   (TeX-add-symbols
    '("tcoev" ["argument"] 1)
    '("tev" ["argument"] 1)
    '("fheads" ["argument"] 2)
    '("ftails" ["argument"] 2)
    '("heads" ["argument"] 2)
    '("tails" ["argument"] 2)
    '("defeq" ["argument"] 1)
    '("eq" ["argument"] 1)
    '("bk" 1)
    '("ass" 3)
    '("coev" 1)
    '("ev" 1)
    "mysize"
    "comp"
    "ot"
    "R"
    "F"
    "Cx"
    "cat"
    "bcat"
    "HomF"
    "Ob"
    "Mor"
    "dom"
    "cod"
    "Cc"
    "ObC"
    "MorC"
    "Vect"
    "VectF"
    "x"
    "a")
   (LaTeX-add-environments
    "proposition"))
 :latex)

