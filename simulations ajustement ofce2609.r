#1
source("simulations init.R")
## fil de l'eau----------------------
cclr <- map_dfr(
  c("fildle", "fildle_plus_r"),
  ~ {
    ps <- c(list(dstar = 1.2), ibs[[.x]])
    run_model(brs25, out = "non", params = ps, full_mc = FALSE) |>
      mutate(ibs = .x)
  }, .progress = TRUE ) |> 
  mutate(loss_ib = NA, sp = NA)

# table_scenarios(cclr |> filter(ibs == "fildle"), dec = 2, bck = 0, fwd=8)
# table_scenarios(cclr |> filter(ibs == "fildle"), dec = 2, bck = 0, fwd=8)
# graph_scenarios(
#   cclr |> filter(ibs == "fildle"),
#   to = 2050,
#   vars = c("og", "dettep", "ib", "spp")
# )

# ccnl <- map_dfr(
#   c("optimal1", "optimal2", "optimal1_plus_r"),
#   ~ {
#     phis <- calc_phis(
#       brs25,
#       algorithm = "cobyla",
#       params = c(
#         c(dstar = 1.2, loss_og = 5, loss_ib = 0, loss_ogpe = 0),
#         ibs[[.x]] ) )
#     cli::cli_alert_info(phis$message)
#     run_model(brs25, out = "none", params = phis, full_mc = FALSE) |>
#       mutate(ibs = .x)
#   },
#   .progress = TRUE
# ) |> 
#   mutate(loss_ib = 0)

## principales variantes, cible de dette --------------------------
ccl <- map_dfr(
  # c("optimal1", "ameco"),
  c("optimal1", "ameco", "optimal2", "optimal1_plus_r", "optimal1_moins_r",
    "no2027", "d2037", "d2030", "nomult", "highmult", "ogpers", "multivar"),
  ~ {
    pp <- list_modify(rs25, dstar = 1.2, loss_og = 5, loss_ib = 5, loss_ogpe = 0, !!!ibs[[.x]])
    brs <- setup_params(
      params = pp,
      modele = "dwrsm3",
      start_y = 2025,
      ameco_version = "5/2026"
    )
    
    phis <- calc_phis(
      brs,
      algorithm = "cobyla" )
    cli::cli_alert_info(phis$message)
    run_model(brs, out = "none", params = phis, full_mc = FALSE) |>
      mutate(ibs = .x)
  },
  .progress = TRUE
) |> 
  mutate(loss_ib = 5, sp = FALSE)

# table_scenarios(ccl |> filter(ibs == "ameco"), dec = 2, bck = 0, fwd=8)
# table_scenarios(ccl |> filter(ibs == "optimal1"), dec = 2, bck = 0, fwd=8)

## cible de solde -----------------

phis <- calc_phis(
  brs25,
  algorithm = "cobyla",
  params = list_modify(
    ibs[["optimal1"]], 
    dstar = 1.2, cons_d=0, cons_dt=0, cons_sp = 19, 
    cons_spt = 7, loss_og = 5, loss_ib = 10, loss_ogpe = 0,
    no_rule = 1, no_phi = c(1,1,0), 
    add_spstar = c(0,0,0,0,0,0, 0, 0.005)))
cli::cli_alert_info(phis$message)
ccsd <- run_model(brs25, out = "none", params = phis, full_mc = FALSE) |>
  mutate(ibs = "ddecroit", loss_ib = 5, sp = TRUE)

phis <- calc_phis(
  brs25,
  algorithm = "cobyla",
  params = list_modify(
    ibs[["optimal1"]], 
    dstar = 1.2, cons_d=0, cons_dt=0, cons_sp = 19, 
    cons_spt = 7, loss_og = 5, loss_ib = 10, loss_ogpe = 0,
    no_rule = 1, no_phi = c(1,1,0), add_spstar = 0.0))
cli::cli_alert_info(phis$message)
ccss <- run_model(brs25, out = "none", params = phis, full_mc = FALSE) |>
  mutate(ibs = "dstable", loss_ib = 5, sp = TRUE)

phis <- calc_phis(
  brs25,
  algorithm = "cobyla",
  params = list_modify(
    ibs[["optimal1"]], 
    dstar = 1.2, cons_d=0, cons_dt=0, cons_sp = 21, 
    cons_spt = 5, loss_og = 5, loss_ib = 10, loss_ogpe = 0,
    no_rule = 1, no_phi = c(1,1,0), 
    add_spstar = c(0,0,0,0,0,0, 0, 0.0)))
cli::cli_alert_info(phis$message)
ccsd30 <- run_model(brs25, out = "none", params = phis, full_mc = FALSE) |>
  mutate(ibs = "dstable30", loss_ib = 5, sp = TRUE)

# table_scenarios(ccss |> filter(ibs == "optimal1"), dec = 2, bck = 0, fwd=8)
# graph_scenarios(
#   ccss |> filter(ibs == "optimal1"),
#   to = 2050,
#   vars = c("ib", "dettep", "sppsd", "spp")
# )

## et si la dette était à 60% ? -----------------

brs25d60 <- setup_params(
  params = list_modify(rs25, i_dette = -0.5214),
  modele = "dwrsm3",
  start_y = 2025,
  ameco_version = "5/2026"
)

# table_scenarios(run_model(brs25d60, out = "none", full_mc = FALSE), dec = 2, bck = 0, fwd=8)

ccdd <- map_dfr(
  c("optimal1", "optimal2", "optimal1_plus_r", "optimal1_plus_r8",
    "no2027", "d2037", "d2030", "nomult", "ogpers", "highmult", "multivar"),
  ~ {
    pp <- list_modify(
      ibs[[.x]], 
      dstar = 0.6, loss_og = 5, loss_ib = 5, loss_ogpe = 0,
      add_rinst = c(0.015, 0.01, 0.0055, rep(0.0, 25)))
    if(str_detect(.x, "plus_r$"))
      pp <- list_modify(
        pp, 
        add_rinst = c(0.015, 0.017, rep(0.017, 25)))
    if(str_detect(.x, "plus_r8$"))
      pp <- list_modify(
        pp, 
        add_rinst = c(0.015, 0.008, rep(0.008, 25)))
    phis <- calc_phis(
      brs25d60,
      algorithm = "cobyla",
      params = pp)
    cli::cli_alert_info(phis$message)
    run_model(brs25d60, out = "none", params = phis, full_mc = FALSE) |>
      mutate(ibs = .x)
  },  .progress = TRUE) |> 
  mutate(loss_ib = 5, sp = FALSE, ifd = TRUE)

# table_scenarios(ccdd |> filter(ibs == "optimal1"), dec = 2, bck = 0, fwd=8)
## cible de solde & dette à 60% -----------------

phis <- calc_phis(
  brs25d60,
  algorithm = "cobyla",
  params = list_modify(
    ibs[["optimal1"]], 
    dstar = 0.6, cons_d=0, cons_dt=0, cons_sp = 19, 
    cons_spt = 7, loss_og = 5, loss_ib = 10, loss_ogpe = 0,
    no_rule = 1, no_phi = c(1,1,0), add_spstar = 0.0,
    add_rinst = c(0.015, 0.01, 0.0055, rep(0.0, 25))))
cli::cli_alert_info(phis$message)
ccssdd <- run_model(brs25d60, out = "none", params = phis, full_mc = FALSE) |>
  mutate(ibs = "dstable", loss_ib = 5, sp = TRUE, ifd = TRUE)

data <- bind_rows( ccl, cclr, ccss, ccsd, ccsd30) |> 
  mutate(ifd = FALSE) |> 
  bind_rows(ccdd) |> 
  bind_rows(ccssdd) |> 
  filter(year<=2050)

return(data)

# table_scenarios(cc |> filter(ibs == "optimal1"), dec = 2)
# graph_scenarios(
#   cc |> filter(ibs == "optimal1"),
#   to = 2050,
#   vars = c("og", "dettep", "ib", "spp")
# )
# 
# qs2::qs_save(cc, pathify("/ajust/cae26/dwcccae26.qs2"))

