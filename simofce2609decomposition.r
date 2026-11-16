#1
source("simulations init.R")
future::plan("multisession", workers = workers)

noog = list(
  i_og = 0.006,
  add_og = c(0.0, 0.0, rep(0.00, 25)),
  add_tcho = c(0))

nok = list(
  og_po = 0,
  og_dep = 0
)

noec = list(
  add_rinst = 0.0
)

noib = list(
  add_ib = 0.0,
  no_rule = 1,
  no_phi = c(0,0,0)
)

cases <- expand_grid(
  .noog = c(TRUE, FALSE),
  .nok = c(TRUE, FALSE),
  .noec = c(TRUE, FALSE),
  .noib = c(TRUE, FALSE))

## principales variantes, cible de dette --------------------------
ccl <- pmap_dfr(
  cases,
  \(.noog, .nok, .noec, .noib) {
    source("simulations init.R")
    params <- list()
    if(.noog)
      params <- params |> list_modify(!!!noog)
    if(.nok)
      params <- params |> list_modify(!!!nok)
    if(.noec)
      params <- params |> list_modify(!!!noec)
    if(.noib)
      params <- params |> list_modify(!!!noib)

    pp <- list_modify(
      ibs[["optimal1"]],
      dstar = 1.2, loss_og = 5, loss_ib = 5, loss_ogpe = 0,
      !!!params)
    pp <- list_modify(rs25, !!!pp)
    brs <- setup_params(
      params = pp,
      modele = "dwrsm3",
      start_y = 2025,
      ameco_version = "5/2026"
    )
    phis <- calc_phis(
      brs,
      algorithm = "cobyla")
    run_model(brs, out = "none", params = phis, full_mc = FALSE) |>
      mutate(noog = .noog, nok = .nok, noec = .noec, noib = .noib, d = 120)
  },
  .progress = TRUE ) |> 
  futurize()

summary <- ccl |>
  filter(variable == "cib") |>
  select(year, deter, q0.5, q0.025, q0.975, noog, nok, noec, noib, d) |>
  filter(year == 2032 + 1)

## valeurs de Shapley des 4 interrupteurs ------------------------
# Valeur d'une coalition S = résultat quand les interrupteurs de S sont
# neutralisés (TRUE). phi_i = effet moyen de la neutralisation de i, moyenné
# sur tous les ordres possibles. La somme des phi_i vaut
# y(tout neutralisé) - y(rien de neutralisé).
shapley <- function(data, players, value, by = "year") {
  n <- length(players)

  map(players, \(i) {
    others <- setdiff(players, i)

    data |>
      select(all_of(c(by, players)), y = {{ value }}) |>
      pivot_wider(names_from = all_of(i), values_from = y, names_prefix = "i_") |>
      mutate(
        s = rowSums(pick(all_of(others))),
        w = factorial(s) * factorial(n - s - 1) / factorial(n),
        delta = i_TRUE - i_FALSE
      ) |>
      summarise(phi = sum(w * delta), .by = all_of(by)) |>
      mutate(switch = i)
  }) |>
    list_rbind()
}

players <- c("noog", "nok", "noec", "noib")

# `deter` : trajectoire déterministe (pas de quantiles : non additifs)
phi <- ccl |>
  filter(variable == "cib") |>
  shapley(players, value = deter) |>
  # contribution du choc = opposé de l'effet de sa neutralisation
  mutate(contribution = -phi)

labels_switch <- c(
  noog = "Écart de production initial",
  nok  = "Multiplicateurs",
  noec = "Prime de risque",
  noib = "Ajustement tardif")

gg_shapley <- ggplot(phi, aes(x = year, y = contribution, fill = switch)) +
  geom_col() +
  scale_fill_discrete(labels = labels_switch) +
  xlim(2032, 2037)+
  labs(x = NULL, y = "Contribution à l'impulsion cumulée", fill = NULL)

return(list(sims = ccl, summary = summary, shapley = phi))
