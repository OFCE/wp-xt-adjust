
vog <- function(og, 
                og_bar = -0.02,
                og_rn = .01,
                og_dep = 1.2,
                og_dep_plus = 1.5,
                og_po = 0.5,
                og_po_plus = 0.75, 
                multivar = 1) {
  og_rcn <- (og - og_bar) / og_rn
  vog_dep <- (og_dep * exp(og_rcn) + og_dep_plus * exp(-og_rcn))/(exp(og_rcn) + exp(-og_rcn))*multivar+
    (1-multivar)*og_dep
  vog_po <- (og_po * exp(og_rcn) + og_po_plus * exp(-og_rcn))/(exp(og_rcn) +  exp(-og_rcn))*multivar+
    (1-multivar)*og_po
  (vog_dep + vog_po)/2
}

ggplot()+geom_function(fun = vog) + xlim(-.04,.02)
