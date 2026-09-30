source(ofce::pathify("/ajust/simulations init.R"))

scn_per <- expand_grid(
  .perte = c("og", "ib", "mo", "mixte"), 
  .df = c(0)/100,
  .cons_spt = c(5, 10, 15, 20, 25),
  .sppstar = c(0, 0.005, 0.0075, 0.01))

plan("multisession", workers = 16)
pertes <- future_pmap_dfr(scn_per, \(.perte, .df, .cons_spt, .sppstar, ...) {
  library(ofce)
  source(pathify("/R/dwr_sim.r"))
  source(pathify("/R/dwr_optim.r"))
  source(pathify("/R/dwr_utils.r"))
  source(pathify("/R/dwr_tools.r"))
  gg <- setup_params(params=rs25, model="dwrsm3", start_y = 2024, ameco = "5/2025")
  obj <- case_when(
    .perte =="og" ~ list(loss_og=1, loss_ib = 0, loss_ogpe = 0),
    .perte =="ib" ~ list(loss_ib = 1, loss_og = 0, loss_ogpe = 0),
    .perte =="mixte" ~ list(loss_og = 5, loss_ibp = 50, loss_ogpe = 1), 
    .perte =="mo" ~ list(loss_og = 0, loss_ib = 0, loss_ogpe = 1),
    TRUE ~ list(loss_og=1, loss_ib = 1, loss_ogpe = 1))
  phis <- calc_phis(
    gg,
    algorithm = "cobyla",
    params = c(list(
      ecstar = 0, infstar = 1.75/100,
      multivar = 0,
      tpo_dstar = 0.,
      loss_df = .df,
      cons_d = 0, cons_spt = .cons_spt, cons_sp = 40-.cons_spt, sppstar = .sppstar),
      ibs[["optimal"]], obj) )
  
  run_model(gg, params = phis, out="none", full_mc = TRUE) |> 
    pluck("full_mc") |> 
    mutate(perte = .perte, df = .df, cons_spt = .cons_spt, sppstar = .sppstar)
}, .progress = TRUE) 


pertes <- pertes |> 
  filter(variable %in% c("og", "ib", "dettep")) |> 
  pivot_wider(names_from = variable, values_from = sim) |> 
  group_by(across(-c(year, og, ib))) |> 
  arrange(year) |> 
  mutate(ogc = cumsum(og),
         ibc = cumsum(ib),
         minog = min(og),
         dettep_max = max(dettep),
         year_max = year[which.max(dettep)]) |> 
  group_by(year, perte, df, cons_spt, sppstar) |> 
  reframe(
    across(c(og, ib, ogc, ibc, dettep), ~c(quantile(.x, probs=c(0.025, 0.5, 0.975)), mean(.x))),
    quantile = c("q0.025", "q0.5", "q0.975", "mean") ) |> 
  ungroup()

return(pertes)