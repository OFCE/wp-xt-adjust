library(tidyverse)
og_bar = -.02
og_rn = 0.01
og_dep = 0.5
og_dep_plus = 1
og_po = 0.5
og_po_plus = 1

vog <- function(x) {
  lagog <- x
  og_rcn <- (lagog - og_bar) / og_rn
  vog_dep <- (og_dep * exp(og_rcn) + og_dep_plus * exp(-og_rcn))/(exp(og_rcn) +  exp(-og_rcn))
  vog_po <- (og_po * exp(og_rcn) + og_po_plus * exp(-og_rcn))/(exp(og_rcn) +  exp(-og_rcn))
  vog_po
  }

ggplot()+geom_function(fun = vog)+xlim(-0.05, 0.05)
