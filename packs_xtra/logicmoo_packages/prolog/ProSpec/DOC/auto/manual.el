(TeX-add-style-hook "manual"
 (function
  (lambda ()
    (LaTeX-add-bibliographies
     "/home/peter/TEX/peter"
     "/home/peter/TEX/logic")
    (LaTeX-add-labels
     "sec:Introduction"
     "sec:Prospec"
     "sec:ProspecSemantics"
     "sec:ProspecUsersGuide")
    (TeX-add-symbols
     "prospec"
     "eqtrafo"
     "sort")
    (TeX-run-style-hooks
     "theorems"
     "mathti"
     "named"
     "xspace"
     "goodies"
     "latex2e"
     "scrartcl10"
     "scrartcl"
     "fleqn"))))

