#1
source("simulations init.R")

cclr <- map_dfr(
  c("fildle", "fildle_plus_r"),
  ~ {
    ps <- c(list(dstar = 1.2), ibs[[.x]])
    run_model(brs25, out = "non", params = ps, full_mc = FALSE) |>
      mutate(ibs = .x)
  },
  .progress = TRUE
) |> 
  mutate(loss_ib = NA)

# table_scenarios(cclr |> filter(ibs == "fildle"), dec = 2, bck = 0, fwd=8)
table_scenarios(cclr |> filter(ibs == "fildle_plus_r"), dec = 2, bck = 0, fwd=8)
# graph_scenarios(
#   cclr |> filter(ibs == "fildle"),
#   to = 2050,
#   vars = c("og", "dettep", "ib", "spp")
# )

ccnl <- map_dfr(
  c("optimal1", "optimal2", "optimal1_plus_r"),
  ~ {
    phis <- calc_phis(
      brs25,
      algorithm = "cobyla",
      params = c(
        c(dstar = 1.2, loss_og = 5, loss_ib = 0, loss_ogpe = 0),
        ibs[[.x]] ) )
    cli::cli_alert_info(phis$message)
    run_model(brs25, out = "none", params = phis, full_mc = FALSE) |>
      mutate(ibs = .x)
  },
  .progress = TRUE
) |> 
  mutate(loss_ib = 0)

ccl <- map_dfr(
  c("optimal1", "optimal2", "optimal1_plus_r"),
  ~ {
    phis <- calc_phis(
      brs25,
      algorithm = "cobyla",
      params = c(
        c(dstar = 1.2, loss_og = 5, loss_ib = 5, loss_ogpe = 0),
        ibs[[.x]] ) )
    cli::cli_alert_info(phis$message)
    run_model(brs25, out = "none", params = phis, full_mc = FALSE) |>
      mutate(ibs = .x)
  },
  .progress = TRUE
) |> 
  mutate(loss_ib = 5)


ccss <- map_dfr(
  c("optimal1", "optimal2", "optimal1_plus_r"),
  ~ {
    phis <- calc_phis(
      brs25,
      algorithm = "cobyla",
      params = c(
        c(dstar = 1.2, cons_d=12, cons_dt=0, cons_sp = 7, cons_spt = 19, loss_og = 5, loss_ib = 0, loss_ogpe = 0),
        ibs[[.x]] ) )
    cli::cli_alert_info(phis$message)
    run_model(brs25, out = "none", params = phis, full_mc = FALSE) |>
      mutate(ibs = .x)
  },
  .progress = TRUE
) |> 
  mutate(loss_ib = 0, sp = TRUE)

# table_scenarios(ccss |> filter(ibs == "optimal1"), dec = 2, bck = 0, fwd=8)
# graph_scenarios(
#   ccss |> filter(ibs == "optimal1"),
#   to = 2050,
#   vars = c("ib", "dettep", "sppsd", "spp")
# )

cc <- bind_rows(ccnl, ccl, cclr, ccss)

# table_scenarios(cc |> filter(ibs == "optimal1"), dec = 2)
# graph_scenarios(
#   cc |> filter(ibs == "optimal1"),
#   to = 2050,
#   vars = c("og", "dettep", "ib", "spp")
# )
# 
# qs2::qs_save(cc, pathify("/ajust/cae26/dwcccae26.qs2"))

return(cc)
