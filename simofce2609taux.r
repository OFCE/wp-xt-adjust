#1
source("simulations init.R")
future::plan("multisession", workers = workers)
add_rinst = c(0.015, 0.015, 0.0105, rep(0.005, 25))
## principales variantes, cible de dette --------------------------
ccl <- pmap_dfr(
  expand_grid(
    .h = 2032,
    .r  =   seq(-0.01, 0.02, .001),
    .k = c(1.2),
    .p = c(0.4)),
  \(.h, .r, .k, .p) {
    source("simulations init.R")

    pp <- list_modify(ibs[["optimal1"]],
                      dstar = 1.2, loss_og = 5, loss_ib = 5, loss_ogpe = 0,
                      cons_d = 27 - (.h - 2025), cons_dt = .h - 2025,
                      # c'est compliqué pour correspondre à ec=0.5 dans le cas central
                      add_rinst = c(0.015, 0.5*(0.025+.r), 0.5*(0.016+.r), rep(.r, 25)),
                      og_dep = .k, og_mce = .p    )
    phis <- calc_phis(
      brs25,
      algorithm = "cobyla",
      params = pp )
    run_model(brs25, out = "none", params = phis, full_mc = FALSE) |>
      mutate(h = .h, r = .r, p = .p, k = .k, d = 120)
  },
  .progress = TRUE ) |>
  futurize()

summary <- ccl |>
  filter(variable == "cib") |>
  select(year, deter, q0.5, q0.025, q0.975, h, p, k, r, d) |>
  filter(year == h + 1)


## si dette 60%


# table_scenarios(run_model(brs25d60, out = "none", full_mc = FALSE), dec = 2, bck = 0, fwd=8)

ccld60 <- pmap_dfr(
  expand_grid(
    .h = 2032,
    .r  =   seq(-0.01, 0.02, .001),
    .k = c(0.8, 1.2, 1.6),
    .p = c(0.4)),
  \(.h, .r, .k, .p) {
    source("simulations init.R")
    brs25d60 <- setup_params(
      params = list_modify(rs25, i_dette = -0.5214),
      modele = "dwrsm3",
      start_y = 2025,
      ameco_version = "5/2026"
    )

    pp <- list_modify(ibs[["optimal1"]],
                      dstar = 0.6, loss_og = 5, loss_ib = 5, loss_ogpe = 0,
                      cons_d = 27 - (.h - 2025), cons_dt = .h - 2025,
                      add_rinst = c(0.015, 0.5*(0.025+.r), 0.5*(0.016+.r), rep(.r, 25)),
                      og_dep = .k, og_mce = .p    )
    phis <- calc_phis(
      brs25d60,
      algorithm = "cobyla",
      params = pp )
    run_model(brs25d60, out = "none", params = phis, full_mc = FALSE) |>
      mutate(h = .h, r = .r, p = .p, k = .k, d = 60)
  },
  .progress = TRUE ) |> futurize()

summary_d60 <- ccld60 |>
  filter(variable == "cib") |>
  select(year, deter, q0.5, q0.025, q0.975, h, p, k, r, d) |>
  filter(year == h + 1)

sum <- bind_rows(summary, summary_d60)
ccl <- bind_rows(ccl, ccld60)

return(list(sims = ccl, summary = sum))


# table_scenarios(cc |> filter(ibs == "optimal1"), dec = 2)
# graph_scenarios(
#   cc |> filter(ibs == "optimal1"),
#   to = 2050,
#   vars = c("og", "dettep", "ib", "spp")
# )
#
# qs2::qs_save(cc, pathify("/ajust/cae26/dwcccae26.qs2"))

data <- ccl |>
  filter(
    between(year, 2025, 2050),
    variable %in% c("og", "ib", "dettep", "sep")) |>
  mutate(
    h = as.character(h),
    r = as.character(round(r*100,1)),
    tooltip = r)

data |>
  select(variable, year, mean, h, p, r, tooltip) |>
  pivot_wider(names_from = variable, values_from = mean) |>
  group_by(h, p, r) |>
  summarize(
    effort = sum(ib[between(year, 2026, as.integer(h))])) |>
  ggplot()+
  aes(x = r, color = p, y = -effort , group = p) +
  scale_y_continuous(labels = scales::label_percent(.1)) +
  geom_line()

