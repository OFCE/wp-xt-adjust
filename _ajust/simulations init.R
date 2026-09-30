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

rs25 <- list(
  draws = 256,  
  start_hist = 2007, end_year = 2035,
  og_lag = 0.25, og_mce = 0.4, 
  og_cin = 0.2, og_po = 0.6, og_dep = 1.4,
  og_ddpot = 0, pot_r = 0,
  ogn_sigma = 0.015, ogn_ar = 0.9, inf_sigma = 0.01,  go_mc = TRUE,
  tcho_okun = 0.6, inf_tcho = 0.15, tcho_mce = 0.75,
  r_mat = 9,
  dstar = 1.1, dstarr = 0.98, infstar = 0.0175,
  loss_t = 20, loss_d = 10, ib_t = 39,
  cons_d = 20, cons_dt = 20, cons_sp = 0, cons_spt = 30,
  loss_df = 0.0, loss_ib = 0, loss_og = 0, loss_ogpe = 0, loss_exp = 8,
  ibcap = 0.05,
  multivar = 0, og_bar = -.02, og_rn = 0.01, og_po_plus = 1.5, og_dep_plus = 1.5,
  r_dette_star = 0, r_dette = 0,
  tpo_sstar = 0.5, tpo_dstar = -0.05, tpo_og = -0.25,
  gpot = c(1.1)/100,
  use_gpot = TRUE,
  i_og = -0.0067,
  i_tpo = +0.0014,
  i_dette = 0.0,
  pcpo = 0.5, 
  taut = .002545898,
  add_og = c(-0.001, 0.003, 0.0, 0.0, 0.0 ), #, 0.005, 0.005, 0.005),
  add_inf = c(-0.006, -0.002, 0.0, 0.0, 0.0, 0.0), 
  no_rule = c(1),
  add_ib = c(-0.0077, -0.0086, -0.0067, -0.008, -0.007,0),
  nairu = 0.07,
  add_tcho = c(-0.004, -0.001, 0.0),
  ecstar = 0.00,
  add_rinst = c(0.015, 0.0085, 0.003, 0.0015, 0.0005, 0),
  add_rapp = c(-0.075, 0)/100)

brs25 <- setup_params(
  params=rs25,  modele ="dwrsm3", start_y = 2024, 
  ameco_version = "5/2025")

ibs <- list(psmt = list(add_ib =  c(-0.0075, -0.0086, -0.0067, -0.008, -0.007,0),
                        no_rule = c(1,1,1,1,1,0)),
            psmtlr = list(add_ib =  c(-0.0075, -0.0086, -0.0067, -0.008, -0.007,0),
                          no_rule = c(1,1,1,1,1,0)),
            backload = list(add_ib = c(0, -0.0075, -0.0086, -0.0067, -0.008, -0.007, 0),
                            no_rule = c(1,1,1,1,1,1, 0)),
            frontload = list(add_ib = c(-0.0145, -0.0086, -0.0067, -0.008, 0),
                             no_rule = c(1,1,1,1,0)),
            optimal = list(add_ib = c(0),
                           no_rule = c(0)),
            lr = list(add_ib = c(0),
                      no_rule = c(0)))
