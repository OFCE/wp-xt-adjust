source(pathify("/ajust/simulations init.R"))


scn <- expand_grid(dstar = seq(90, 130, by = 10)/100,
                   ecstar = 0,
                   infstar = 1.75/100,
                   og_dep = c(0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2.),
                   og_mce = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
                   mult = "fixe",
                   objective = c("mixte"),
                   ib = c("psmt", "optimal"))

plan("multisession", workers = 16)
rs <- future_pmap_dfr(scn, \(dstar, ecstar, mult, ib, objective, infstar, og_dep, og_mce, ...) {
  library(ofce)
  source(pathify("/R/dwr_sim.r"))
  source(pathify("/R/dwr_optim.r"))
  source(pathify("/R/dwr_utils.r"))
  source(pathify("/R/dwr_tools.r"))
  gg <- setup_params(params=rs25, model="dwrsm3", start_y = 2024, ameco = "5/2025")
  obj <- case_when(
    objective =="og" ~ list(loss_og=1, loss_ib = 0, loss_ogpe = 0),
    objective =="ib" ~ list(loss_ib = 1, loss_og = 0, loss_ogpe = 0),
    objective =="mixte" ~ list(loss_og = 5, loss_ibp = 50, loss_ogpe = 1), 
    objective =="mo" ~ list(loss_og = 0, loss_ib = 0, loss_ogpe = 1),
    TRUE ~ list(loss_og=1, loss_ib = 1, loss_ogpe = 1))
  phis <- calc_phis(
    gg,
    algorithm = "cobyla",
    params = c(list(
      dstar = dstar, ecstar = ecstar, infstar = infstar,
      og_dep = og_dep, og_mce = og_mce,
      multivar = ifelse(mult=="fixe", 0, 1)),
      ibs[[ib]], obj) )
  run <- run_model(gg, params = phis, out="non", full_mc = TRUE)
  run$full_mc |> 
    filter(year>=2025) |> 
    arrange(year) |> 
    pivot_wider(names_from = variable, values_from = sim) |> 
    group_by(draw) |>
    mutate(
      yibm = max(year[year<2045&ib<0]),
      ibc = cumsum(ib),
      ogc = cumsum(og)) |>
    filter(year %in% seq(2025, 2070, by=1)) |> 
    transmute(year, draw, ibc, ogc, og, ib, dettep, 
              dstar = !!dstar, ecstar = !!ecstar, mult = !!mult, 
              ibs = !!ib, objective = !!objective, infstar = !!infstar, 
              og_dep = !!og_dep, og_mce = !!og_mce)
}, .progress = TRUE)

rs1 <- rs |>
  filter(year>=2025, year<=2070) |>
  mutate(
    ibs = factor(ibs, c("optimal", "psmt", "frontload", "backload")) ) |> 
  group_by(year, dstar, ecstar, mult, ibs, objective, infstar, og_dep, og_mce) |>
  reframe(across(-draw, ~c(quantile(.x, c(0.025, 0.25,0.5,0.75, 0.975)), mean(.x))),
          quantile = c("q0.025", "q0.25", "q0.5", "q0.75", "q0.975", "mean")) |>
  ungroup() |>
  mutate(dstar = round(dstar,3)) |>
  mutate(
    ibs = factor(ibs, c("optimal", "psmt", "frontload", "backload"))) |>
  select(year, quantile, dstar, ecstar, mult, ibs, ib, ibc, og, ogc, objective, infstar, og_dep, og_mce) |>
  pivot_longer(cols = c(ib, ibc, og, ogc), names_to = "variable", values_to = "value") |> 
  pivot_wider(names_from = quantile, values_from = value) |>
  arrange(mult, objective, dstar, ibs, year, variable, og_dep, og_mce) |>
  group_by(mult, objective, ecstar, dstar, ibs, infstar, variable, og_dep, og_mce) |>
  mutate(
    libc1 = str_c("min: ",round(min(q0.5[year>=2029])*100, 2), "%"),
    libc2 = str_c("total: ", round(q0.5[year==max(year)]*100, 2), "%"),
    libc  = ifelse(year==2045, str_c(libc1, "\n", libc2), NA_character_)) |>
  ungroup() |>
  mutate(dstar = factor(
    dstar, label = str_c("d*=", round(100*unique(dstar)),"%"))) 

rs2 <- rs |> 
  filter(year>=2025, year<=2070) |>
  mutate(
    ibs = factor(ibs, c("optimal", "psmt", "frontload", "backload")) ) |>
  group_by(across(-c(year, ib, ibc, ogc, og, dettep))) |>
  mutate(
    p_psmt  = between(year, 2025, 2029),
    p_2psmt  = between(year, 2025, 2034),
    p_target  = between(year, 2030, 2045),
    p_ftarget = between(year, 2025, 2045),
    p_full = between(year, 2025, 2050) ) |>
  summarize(
    minog_psmt = min(og[p_psmt]),
    meanib_psmt = mean(ib[p_psmt]),
    cumib_psmt = sum(ib[p_psmt]),
    minog_2psmt = min(og[p_2psmt]),
    meanib_2psmt = mean(ib[p_2psmt]),
    cumib_2psmt = sum(ib[p_2psmt]),
    minog_ftarget = min(og[p_ftarget]),
    meanib_ftarget = mean(ib[p_ftarget]),
    cumib_ftarget = sum(ib[p_ftarget]),
    minog_full = min(og[p_full]),
    meanib_full = mean(ib[p_full]),
    cumib_full = sum(ib[p_full]),
    maxd_ftarget = max(dettep[p_ftarget]),
    maxd_full = max(dettep[p_full]),
    maxd_psmt = max(dettep[p_psmt]),
    maxd_2psmt = max(dettep[p_2psmt]),
    .groups = "drop") |>
  pivot_longer(cols = starts_with(c("min", "mean", "cum", "max")) ) |>
  separate(name, into = c("variable", "period"), sep = "_") |>
  group_by(across(-c(draw, value))) |> 
  summarize(
    q0.5 = quantile(value, 0.5),
    q0.975 = quantile(value, 0.975),
    q0.025 = quantile(value, 0.025),
    q0.75 = quantile(value, 0.75),
    q0.25 = quantile(value, 0.25),
    mean = mean(value),
    .groups = "drop" ) 

return(list(mins = rs2, trajectoires= rs1))