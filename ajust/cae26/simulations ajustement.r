#1
source(ofce::pathify("/ajust/cae26/simulations init.R"))

cclr <- map_dfr(
  c("fildle"),
  ~ {
    ps <- c(list(dstar = 1.2), ibs[[.x]])
    run_model(brs25, out = "non", params = ps, full_mc = FALSE) |>
      mutate(ibs = .x)
  },
  .progress = TRUE
)

table_scenarios(cclr |> filter(ibs == "fildle"), dec = 2)
graph_scenarios(
  cclr |> filter(ibs == "fildle"),
  to = 2050,
  vars = c("og", "dettep", "ib", "spp")
)

cc <- map_dfr(
  c("optimal1", "optimal2"),
  ~ {
    phis <- calc_phis(
      brs25,
      algorithm = "cobyla",
      params = c(
        c(dstar = 1.2, loss_og = 5, loss_ib = 10, loss_ogpe = 0),
        ibs[[.x]] ) )
    cli::cli_alert_info(phis$message)
    run_model(brs25, out = "none", params = phis, full_mc = FALSE) |>
      mutate(ibs = .x)
  },
  .progress = TRUE
) |>
  bind_rows(cclr)

table_scenarios(cc |> filter(ibs == "optimal1"), dec = 2)
graph_scenarios(
  cc |> filter(ibs == "optimal1"),
  to = 2050,
  vars = c("og", "dettep", "ib", "spp"))
table_scenarios(cc |> filter(ibs == "optimal2"), dec = 2)
graph_scenarios(
  cc |> filter(ibs == "optimal2"),
  to = 2050,
  vars = c("og", "dettep", "ib", "spp")
)

qs2::qs_save(cc, pathify("/dw cc.qs2"))

scn_per <- expand_grid(
  .perte = c("og", "ib", "mixte", "mo"),
  .df = c(0, 5, 10) / 100,
  .dstar = c(0.9, 1., 1.1, 1.2)
)

plan("multisession", workers = 16)
pertes <- future_pmap_dfr(
  scn_per,
  \(.perte, .df, .dstar, ...) {
    library(ofce)
    source(pathify("/R/dwr_sim.r"))
    source(pathify("/R/dwr_optim.r"))
    source(pathify("/R/dwr_utils.r"))
    source(pathify("/R/dwr_tools.r"))
    gg <- setup_params(
      params = rs25,
      model = "dwrsm3",
      start_y = 2025,
      ameco = "5/2026"
    )
    obj <- case_when(
      .perte == "og" ~ list(loss_og = 1, loss_ib = 0, loss_ogpe = 0),
      .perte == "ib" ~ list(loss_ib = 1, loss_og = 0, loss_ogpe = 0),
      .perte == "mixte" ~ list(loss_og = 5, loss_ib = 5, loss_ogpe = 1),
      .perte == "mo" ~ list(loss_og = 0, loss_ib = 0, loss_ogpe = 1)
    )
    phis <- calc_phis(
      gg,
      algorithm = "cobyla",
      params = c(
        list(
          dstar = .dstar,
          ecstar = 0,
          infstar = 1.75 / 100,
          multivar = 0
        ),
        loss_df = .df,
        ibs[["optimal"]],
        obj
      )
    )
    run_model(gg, params = phis, out = "non", full_mc = TRUE) |>
      pluck("full_mc") |>
      mutate(perte = .perte, df = .df, dstar = .dstar)
  },
  .progress = TRUE
)

psmtlr <- map_dfr(
  c(0.9, 1., 1.1, 1.2),
  ~ {
    linear_rule <- c(
      list(
        dstar = .x,
        loss_ogpe = 0,
        loss_exp = 7,
        loss_og = 1,
        loss_sp = 0,
        loss_t = 20,
        ib_t = 31,
        loss_ib = 0.5,
        loss_df = 0.0,
        loss_mo = 0,
        cons_d = 11,
        cons_sp = 0,
        cons_dt = 20,
        cons_spt = 20,
        tpo_sstar = 0.5,
        tpo_dstar = -0.1,
        tpo_og = -0.25
      ),
      ibs[["lr"]]
    )

    run_model(brs25, out = "graph", params = linear_rule, full_mc = TRUE) |>
      pluck("full_mc") |>
      mutate(perte = "lr", df = 0, dstar = .x)
  }
)

pertes <- pertes |>
  bind_rows(psmtlr) |>
  filter(variable %in% c("og", "ib")) |>
  pivot_wider(names_from = variable, values_from = sim) |>
  group_by(across(-c(year, og, ib))) |>
  arrange(year) |>
  mutate(ogc = cumsum(og), ibc = cumsum(ib)) |>
  group_by(year, perte, df, dstar) |>
  reframe(
    across(
      c(og, ib, ogc, ibc),
      ~ c(quantile(.x, probs = c(0.025, 0.5, 0.975)), mean(.x))
    ),
    quantile = c("q0.025", "q0.5", "q0.975", "mean")
  ) |>
  ungroup()

return(list(cc = cc, pertes = pertes))
