library(future)
library(furrr)
library(progressr)
library(scales)
library(paletteer)
library(lubridate)
library(glue)
library(gt)
library(tidyverse)
library(patchwork)
library(countrycode)
library(ofce)
# devtools::install_github("mrc-ide/odin.dust")
# devtools::install_github("mrc-ide/odin")
# devtools::install_github("mrc-ide/dust")
future::plan("multisession", workers=16)
progressr::handlers(progressr::handler_progress(format = ":bar :percent :eta", width = 80))
source(pathify("/R/dwr_sim.r"))
source(pathify("/R/dwr_optim.r"))
source(pathify("/R/dwr_utils.r"))
source(pathify("/R/dwr_tools.r"))
source(pathify("/R/dwr_mongo.r"))

pays_1 <- c("FRA", "ITA", "DEU", "ESP", "NLD", "BEL", "GRC", "PRT", "AUT", "IRL")
pays_2 <- c( "GRC", "PRT")
# source(pathify("/divers/variantes/ze.r"))
br <- paletteer::paletteer_d("miscpalettes::brightPastel")

line25 <- list(
  draws = 256,  
  start_hist = 2007, end_year = 2035,
  og_lag = 0.25, og_mce = 0.3, 
  og_cin = 0.2, og_po = 0.5, og_dep = 0.8,
  og_ddpot = 0, pot_r = 0,
  ogn_sigma = 0.015, ogn_ar = 0.9, inf_sigma = 0.01,  go_mc = TRUE,
  tcho_okun = 0.5, inf_tcho = 0.15, 
  r_mat = 9,
  dstar = 1.1, dstarr = 0.98, infstar = 0.0175,
  loss_t = 20, loss_d = 10, ib_t = 30,
  cons_d = 10, cons_dt = 20, 
  loss_df = 0.0, loss_ib = 0, loss_og = 0, loss_mo = 0,
  ibcap = 0.05,
  multivar = 0, og_bar = -.02, og_rn = 0.01, og_po_plus = 1.5, og_dep_plus = 1.5,
  r_dette_star = 0, r_dette = 0,
  tpo_sstar = 0.5, tpo_dstar = -0.1, tpo_og = -0.25,
  gpot = c(0.8, 0.95, 1.1)/100,
  use_gpot = TRUE,
  i_og = -0.0067,
  i_tpo = +0.0014,
  i_dette = 0.0,
  pcpo = 0.5, 
  taut = .002545898,
  add_og = c(-0.000, 0.0037, 0.0, 0.0, 0.0 ), #, 0.005, 0.005, 0.005),
  add_inf = c(-0.006, -0.002, 0.0, 0.0, 0.0, 0.0), 
  no_rule = c(1),
  add_ib = c(-0.0075, -0.009, -0.007, -0.008, -0.007,0),
  nairu = 0.07,
  add_tcho = c(0.001, 0.003, 0.0),
  ecstar = 0.00,
  add_rinst = c(0.015, 0.0085, 0.003, 0.0015, 0.0005, 0),
  add_rapp = c(-0.075, 0)/100,
  dettep_cc =0, og_cc = 0, ec_cc =0, sp_cc = 0, inf_cc = 0) 

bline25 <- setup_params(
  params=line25,  modele ="dwrsm3", start_y = 2024, 
  ameco_version = "5/2025")

ibs <- list(psmt = list(add_ib =  c(-0.0075, -0.009, -0.007, -0.008, -0.007,0),
                        no_rule = c(1,1,1,1,1,0)),
            backload = list(add_ib = c(0, -0.0075, -0.009, -0.007, -0.008, -0.007, 0),
                            no_rule = c(1,1,1,1,1,1, 0)),
            frontload = list(add_ib = c(-0.0145, -0.009, -0.007, -0.008, 0),
                             no_rule = c(1,1,1,1,0)),
            optimal = list(add_ib = c(0),
                           no_rule = c(0)))

cc0 <- run_model(bline25, out="table", params = ibs[["psmt"]], full_mc = FALSE)

cc1 <- run_model(bline25, out="table", params = ibs[["optimal"]], full_mc = FALSE)
cc1 <- run_model(bline25, out="graph", params = ibs[["optimal"]], full_mc = FALSE)

targets <- list(
  og = c(loss_d = 10 , loss_og = 1, loss_df = 0.0 ),
  ib = c(loss_d = 10 , loss_ib = 1, loss_df = 0.0 ),
  mo = c(loss_d = 10 , loss_mo = 1, loss_df = 0.0 ),
  ogsp = c(loss_sp = 10 , loss_og = 1, loss_df = 0.0),
  ibsp = c(loss_d = 10 , loss_ib = 1, loss_df = 0.0 ),
  mosp = c(loss_d = 10 , loss_mo = 1, loss_df = 0.0 ))

objpsmt <- imap_dfr(targets, ~{
  source(pathify("/R/dwr_sim.r"))
  source(pathify("/R/dwr_optim.r"))
  source(pathify("/R/dwr_utils.r"))
  source(pathify("/R/dwr_tools.r"))
  base <- setup_params(
    params=line25,  modele ="dwrsm3", start_y = 2024, 
    ameco_version = "5/2025")
  pp <- c(list(dstar=1.1, draws = 1, ogn_sigma = 0, inf_sigma = 0), ibs[["psmt"]], .x)
  phis <- calc_phis(base, params = pp, dt=15, algorithm = "cobyla")
  run_model(bline25, params = phis, out="none", full_mc = FALSE) |> 
    mutate(obj = .y)
}, .progress = TRUE) |> 
  bind_rows( 
    cc0 |> mutate( obj = "lr") )
saveRDS(objpsmt, pathify("/ajust/objpsmt.rds"))

objoptim <- imap_dfr(targets, ~{
  source(pathify("/R/dwr_sim.r"))
  source(pathify("/R/dwr_optim.r"))
  source(pathify("/R/dwr_utils.r"))
  source(pathify("/R/dwr_tools.r"))
  base <- setup_params(
    params=line25,  modele ="dwrsm3", start_y = 2024, 
    ameco_version = "5/2025")
  pp <- c(list(dstar=1.1, draws = 1, ogn_sigma = 0, inf_sigma = 0), ibs[["optimal"]], .x)
  phis <- calc_phis(base, params = pp, dt=15, algorithm = "cobyla")
  run_model(bline25, params = phis, out="none", full_mc = FALSE) |> 
    mutate(obj = .y)
}, .progress = TRUE) |> 
  bind_rows( 
    cc1 |> mutate( obj = "lr") )
saveRDS(objoptim, pathify("/ajust/objoptim.rds"))

algos <- expand_grid(.d = 1.1, .algo = c("l2", "cobyla", "auglag"),
                     .loss = c("ib", "og", "mo", "ogp8"))

algo <- pmap_dfr(algos, \(.d, .algo, .loss, ...) {
  source(pathify("/R/dwr_sim.r"))
  source(pathify("/R/dwr_optim.r"))
  source(pathify("/R/dwr_utils.r"))
  source(pathify("/R/dwr_tools.r"))
  base <- setup_params(
    params=line25,  modele ="dwrsm3", start_y = 2024, 
    ameco_version = "5/2025")
  ploss <- case_when(
    .loss == "ib" ~ list(loss_ib = 1, loss_ogp = 0, loss_og = 0, loss_mo = 0, loss_exp = 2),
    .loss == "og" ~ list(loss_ib = 0, loss_ogp = 0, loss_og = 1, loss_mo = 0, loss_exp = 2),
    .loss == "mo" ~ list(loss_ib = 0, loss_ogp = 0, loss_og = 0, loss_mo = 1, loss_exp = 2),
    .loss == "og4" ~ list(loss_ib = 0, loss_ogp = 0, loss_og = 1, loss_mo = 0, loss_exp = 4),
    .loss == "og8" ~ list(loss_ib = 0, loss_ogp = 0, loss_og = 1, loss_mo = 0, loss_exp = 8),
    .loss == "ogp8" ~ list(loss_ib = 0, loss_ogp = 1, loss_og = 0, loss_mo = 0, loss_exp = 8))
  pp <- c(list(dstar=.d, draws = 1, ogn_sigma = 0, inf_sigma = 0, loss_d = 5, loss_sp = 0), ibs[["optimal"]], ploss)
  phis <- calc_phis(base, params = pp, dt=15, algorithm = .algo)
  run_model(bline25, params = phis, out="none", full_mc = FALSE) |> 
    mutate(dstar = .d, algorithm = .algo, loss = .loss)
}, .progress = TRUE)

algo |> 
  filter(variable=="og", year>=2025, year<=2050) |> 
  ggplot() +
  geom_line(aes(x=year, y = (q0.5))) +
  facet_grid(rows=vars(algorithm), cols = vars(loss))

run_scenarios(
  baseline = b1.1$sim,
  model = "dwrsm3",
  phis = TRUE,
  vars = c("og","dettep","ib","sep"),
  scenarios = list(
    "ib" = list(loss_og = 0, loss_ib =1),
    "sog" = list(loss_og = 0, loss_sog =1)))

phis_alt <- calc_phis(
  bline25, algorithm = "cobyla",
  params = c(list(dstar = 1.1, 
                  loss_og = 1, loss_ib = 0, loss_df = 0.0,
                  loss_d = 10, loss_sp = 0, loss_t = 20, ib_t = 40,
                  draws = 512, ogn_sigma = 0.015, inf_sigma = 0.01, 
                  tpo_dstar = -0.1, tpo_sstar = 0.5, tpo_og = -0.3, add_spstar = 0.0,
                  cons_d = 0, cons_sp = 20, cons_dt = 10, cons_spt = 10),
             ibs[["optimal"]]), dc=1, dt=20)

ccalt <- run_model(bline25, params = list_modify(phis_alt),out="graph", full_mc = FALSE, to=2055,
                   vars = c("og", "spp", "ib", "dettep"))

ccalt |> filter(variable=="ib", year>=2025, year <=2050) |> summarize(sum(q0.5))

phis1.25 <- calc_phis(bline25, params = list(dstar=1.25))
phis1.2 <- calc_phis(bline25, params = c(list(dstar=1.2)))
b1.2 <- run_model(bline25, params = phis1.2, out="table", full_mc = TRUE, dec=2)

b1.2 <- run_model(bline25, params = phis1.2, out="graph", full_mc = TRUE)
saveRDS(b1.2, "divers/ajust/cc.rds")

bb <- bind_rows(b1.2$full_mc |> mutate(dstar = 1.2),
                b1.1$full_mc |> mutate(dstar = 1.1 ),
                b1.25$full_mc |> mutate(dstar = 1.25 ))

bb |> 
  pivot_wider(names_from = variable, values_from = sim) |> 
  group_by(draw, dstar) |>
  summarize(
    e_inf = sum(ib), 
    e30 = sum(ib[year<=2030]),
    e35 = sum(ib[year<=2035]),
    e45 = sum(ib[year<=2045]), 
    .groups = "drop") |> 
  group_by(dstar) |> 
  reframe(across(-c(draw), ~quantile(.x, c(0.025,0.5,0.975))),
          quantile = c(0.025,0.5,0.975))

scn <- expand_grid(dstar = seq(0.9, 1.3, by = 0.025),
                   ecstar = c(-0.005, 0, 0.005),
                   infstar = c(1.75, 2)/100,
                   mult = "fixe",
                   objective = c("mixte"),
                   ib = c("psmt", "optimal"))

rs25 <- list(
  draws = 256,  
  start_hist = 2007, end_year = 2035,
  og_lag = 0.05, og_mce = 0.3, 
  og_cin = 0.2, og_po = 0.5, og_dep = 0.8,
  ogn_sigma = 0.015, ogn_ar = 0.9, inf_sigma = 0.01,  go_mc = TRUE,
  tcho_okun = 0.5, inf_tcho = 0.005, 
  r_mat = 9,
  dstar = 1.1, dstarr = 0.98, infstar = 0.0175,
  loss_t = 20, loss_d = 10, ib_t = 30,
  cons_d = 10, cons_dt = 20, 
  loss_df = 0.0, loss_ib = 0, loss_og = 0, loss_mo = 0,
  ibcap = 0.05,
  multivar = 0, og_bar = -.02, og_rn = 0.01, og_po_plus = 1.5, og_dep_plus = 1.5,
  r_dette_star = 0, r_dette = 0,
  ttpo_sstar = 0.25, tpo_dstar = -0.1, tpo_og = -0.25,
  gpot = c(0.8, 1.1)/100,
  use_gpot = TRUE,
  i_og = -0.0067,
  i_tpo = +0.0014,
  i_dette = 0.0,
  pcpo = 0.5, 
  taut = .002545898,
  add_og = c(-0.000, 0.0037, 0.0, 0.0, 0.0 ), #, 0.005, 0.005, 0.005),
  add_inf = c(-0.007, 0.0, 0.0, 0.0, 0.0, 0.0), 
  no_rule = c(1),
  add_ib = c(-0.0075, -0.009, -0.007, -0.008, -0.007,0),
  nairu = 0.07,
  add_tcho = c(0.001, 0.003, 0.0),
  ecstar = 0.00,
  add_rinst = c(0.015, 0.0085, 0.003, 0.0015, 0.0005, 0),
  add_rapp = c(-0.075, 0)/100)

plan("multisession", workers = 16)
rs <- future_pmap_dfr(scn, \(dstar, ecstar, mult, ib, objective, infstar, ...) {
  library(ofce)
  source(pathify("/R/dwr_sim.r"))
  source(pathify("/R/dwr_optim.r"))
  source(pathify("/R/dwr_utils.r"))
  source(pathify("/R/dwr_tools.r"))
  gg <- setup_params(params=rs25, model="dwrsm3", start_y = 2024, ameco = "5/2025")
  obj <- case_when(
    objective=="og" ~ list(loss_og=1, loss_ib = 0),
    objective=="ib" ~ list(loss_ib = 1, loss_og = 0),
    objective=="mixte" ~ list(loss_og = 1, loss_ib = 0.5),
    TRUE ~ list(loss_og=1, loss_ib = 0.5))
  phis <- calc_phis(
    gg, dt = 20,
    algorithm = "cobyla",
    params = c(list(
      dstar = dstar, ecstar = ecstar, infstar = infstar,
      multivar = ifelse(mult=="fixe", 0, 1)),
      ibs[[ib]], obj) )
  run <- run_model(gg, params = phis, out="non", full_mc = TRUE)
  run$full_mc |> 
    filter(year>=2025) |> 
    arrange(year) |> 
    pivot_wider(names_from = variable, values_from = sim) |> 
    group_by(draw) |>
    mutate(
      yibm = max(year[year<2040&ib<0]),
      ibc = cumsum(ib),
      ogc = cumsum(og)) |>
    filter(year %in% seq(2025, 2070, by=1)) |> 
    # group_by(year) |> 
    # reframe(across(-draw, ~quantile(.x, c(0.25,0.5,0.75))),
    #         quantile = c(0.25,0.5,0.75)) |> 
    # ungroup() |> 
    transmute(year, draw, ibc, ogc, og, ib, dettep, 
              dstar = !!dstar, ecstar = !!ecstar, mult = !!mult, 
              ibs = !!ib, objective = !!objective, infstar = !!infstar )
}, .progress = TRUE)

saveRDS(rs, pathify("/ajust/sensibilite.rds"))

library(ggh4x)
rs |> 
  group_by(year) |> 
  reframe(across(-draw, ~quantile(.x, c(0.25,0.5,0.75))),
          quantile = c(0.25,0.5,0.75)) |> 
  ungroup() |> 
  mutate(dstar = round(dstar,3)) |> 
  filter(year>=2029, year<=2050, mult=="fixe", objective == "mixte",
         dstar %in% seq(0.9, 1.3, .2)) |> 
  mutate(
    ibs = factor(ibs, c("optimal", "psmt", "frontload", "backload")),
    quantile = str_c("q", quantile)) |>
  filter(infstar == "0.0175") |> 
  select(year, quantile, dstar, ecstar, mult, ibs, value = ibc, objective, infstar) |> 
  pivot_wider(names_from = quantile, values_from = value) |>
  arrange(mult, objective, dstar, ibs, year) |> 
  group_by(mult, objective, ecstar, dstar, ibs, infstar) |> 
  mutate(
    libc1 = str_c("min: ",round(min(q0.5[year>=2029])*100, 2), "%"),
    libc2 = str_c("final: ", round(q0.5[year==max(year)]*100, 2), "%"),
    libc  = ifelse(year==2029, str_c(libc1, "\n", libc2), NA_character_)) |> 
  ungroup() |> 
  mutate(dstar = factor(
    dstar, label = str_c("d*=", round(100*unique(dstar)),"%"))) |> 
  ggplot() +
  aes(y =q0.5, ymin=q0.25, ymax =q0.75, 
      x = year) +
  geom_ribbon(aes(ymin=q0.25, ymax=q0.75),
              alpha = 0.2, fill = "orange") +
  geom_line(
    alpha = 1, size = 0.1, linewidth = 0.2, color = "orange") +
  geom_text(
    aes(label=libc, y=-0.057),
    color = "black",
    size = 2, hjust = 0 )+
  # scale_color_distiller(palette="Oranges", direction = -1, name = NULL)+
  scale_y_continuous(
    limits = c(-0.06, -0.02),
    breaks=scales::breaks_width(0.01),
    labels=scales::label_percent(1),
    oob = scales::squish)+
  scale_x_continuous(breaks = c(2030, 2040, 2050), 
                     labels = c("2030", "40", "50"),
                     minor_breaks = scales::breaks_width(1)) +
  guides(color = guide_legend(nrow = 1),
         x = guide_axis(minor.ticks = TRUE)) +
  theme_ofce(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width = unit(24, "pt"),
    legend.text.position = "bottom",
    axis.text.x = element_text(hjust=0, size = rel(0.9)),
    legend.title = marquee::element_marquee(vjust = 0.9),
    panel.spacing = unit(5, "pt")
  )+
  facet_nested(
    cols=vars(ibs, dstar), rows= vars(ecstar),
    labeller = labeller(
      ecstar = c("-0.005" = "ec*=-0.5%", 
                 "0" = "ec*=0%", 
                 "0.005" = "ec*=0.5%"),
      infstar = c("0.0175"="i*=1,75%", "0.02"="i*=2.0%"),
      ibs = c("psmt"= "PSMT", "optimal"="Optimal", "Frontload", "Backload"))) +
  xlab(NULL)+
  ylab("Cumul des impulsions")

rs |> 
  filter(year>=2025, year<=2050, 
         mult=="fixe", objective == "mixte", quantile == "0.5") |> 
  mutate(
    ibs = factor(ibs, c("optimal", "psmt", "frontload", "backload")) ) |>
  select(year, quantile, dstar, ecstar, mult,
         ibs, ibc, dettep, ogc, og, ib, objective, infstar) |> 
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
    maxd_2psmt = max(dettep[p_2psmt])) |> 
  pivot_longer(cols = starts_with(c("min", "mean", "cum", "max")) ) |> 
  separate(name, into = c("variable", "period"), sep = "_") |>
  filter(period %in% c("psmt", "2psmt", "full"), infstar == "0.0175" ) |> 
  ggplot() + 
  aes(x = dstar, y = value, group = ibs, color = ibs) +
  facet_nested(rows = vars(variable), cols = vars(period, ecstar), scale = "free") +
  geom_line() +
  PrettyCols::scale_color_pretty_d("Joyful")+
  scale_y_continuous(labels=scales::label_percent(.1))+
  scale_x_continuous(labels=scales::label_percent(1))+
  theme_ofce(
    panel.spacing = unit(6, "pt"),
    axis.text.x = element_text(size = rel(0.8))
  )


