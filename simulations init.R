library(scales)
library(future)
library(futurize)
library(furrr)
library(progressr)
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
progressr::handlers(progressr::handler_progress(
  format = ":bar :percent :eta",
  width = 70
))
source(pathify("R/dwr_sim.r"))
source(pathify("R/dwr_optim.r"))
source(pathify("R/dwr_utils.r"))
source(pathify("R/dwr_tools.r"))
source(pathify("R/dwr_mongo.r"))

workers <- 4

ofce::init_qmd()
options(OutDec=".")
rs25 <- list(
  draws = 256,
  start_hist = 2007,
  end_year = 2035,
  og_lag = 0.1,
  og_mce = 0.4,
  og_cin = 0.0,
  og_po = 0.5,
  og_dep = 1.2,
  og_ddpot = 0,
  pot_r = 0,
  ogn_sigma = 0.015,
  ogn_ar = 0.9,
  inf_sigma = 0.01,
  go_mc = TRUE,
  tcho_okun = 0.6,
  inf_tcho = 0.15,
  tcho_mce = 0.75,
  r_mat = 9,
  dstar = 1.2,
  dstarr = 0.98,
  infstar = 0.018,
  loss_t = 20,
  loss_d = 10,
  ib_t = 26,
  cons_d = 20,
  cons_dt = 7,
  cons_sp = 0,
  cons_spt = 12,
  loss_df = 0.0,
  loss_ib = 0,
  loss_og = 0,
  loss_ogpe = 0,
  loss_exp = 8,
  ibcap = 0.025,
  multivar = 0,
  og_bar = -.02,
  og_rn = 0.001,
  og_po_plus = 0.75,
  og_dep_plus = 1.5,
  r_dette_star = 0,
  r_dette = 0,
  tpo_sstar = 0.5,
  tpo_dstar = -0.075,
  tpo_og = -0.25,
  gpot = c(1.25, 1.25, 1.2, 1.15, 1.1, 1.0) / 100,
  use_gpot = TRUE,
  i_og = 0.002,
  i_tpo = 0.0013,
  i_dette = -0.0048,
  pcpo = 0.5,
  taut = c(-.0009),
  add_og = c(-0.0063, -0.0054, rep(0.00, 25)),
  add_inf = c(+0.0023, 0.0031, 0.001, 0),
  no_rule = 1,
  no_phi = 0,
  add_ib = c(-0.00, 0.0, 0.0, 0.0, 0.0, 0),
  nairu = 0.07,
  add_tcho = c(0.00375, 0.00, 0.0),
  ecstar = 0.00,
  add_rinst = c(0.009, 0.01, 0.007, rep(0.0055, 25)),
  add_rapp = c(0.045, -0.05, 0) / 100,
  dette_oneoff = c(0.5, 0.5, 0)/100
)

brs25 <- setup_params(
  params = rs25,
  modele = "dwrsm3",
  start_y = 2025,
  ameco_version = "5/2026"
)
r_inst_f <- function(.r) c(0.015, 0.75*0.015+0.25*.r, 0.25*0.015 + 0.75*.r, rep(.r, 25))
r_inst_5 <- r_inst_f(0.005)
r_inst_0 <- r_inst_f(0.0)
r_inst_8 <- r_inst_f(0.008)
r_inst_17 <- r_inst_f(0.017)

ibs <- list(
  fildle = list(
    add_ib = c(-0.005, -0.008, 0),
    no_rule = 1,
    no_phi = 1,
    add_rinst = r_inst_5
  ),
  fildle_plus_r = list(
    add_ib = c(-0.004, -0.007, 0),
    no_rule = c(1),
    no_phi = 1,
    add_rinst = r_inst_17
  ),
  optimal1 = list(
    add_ib = c(-0.005, -0.008, 0),
    no_rule = c(1, 1, 0),
    no_phi = c(1, 1, 0),
    add_rinst = r_inst_5
  ),
  ameco = list(
    i_og  = 0.0,
    add_og = c(-0.0069, -0.0061, rep(0.00, 25)),
    taut = c(+.0001),
    add_tcho = c(0.0025, 0.00, 0.0),
    add_ib = c(-0.005, -0.008, 0),
    no_rule = c(1, 1, 0),
    no_phi = c(1, 1, 0),
    add_rinst = r_inst_5
  ),
  optimal1_moins_r = list(
    add_ib = c(-0.005, -0.008, 0),
    no_rule = c(1, 1, 0),
    no_phi = c(1, 1, 0),
    add_rinst = r_inst_0
  ),
  optimal1_plus_r = list(
    add_ib = c(-0.005, -0.008, 0),
    no_rule = c(1, 1, 0),
    no_phi = c(1, 1, 0),
    add_rinst = r_inst_17
  ),
  optimal1_plus_r5 = list(
    add_ib = c(-0.005, -0.008, 0),
    no_rule = c(1, 1, 0),
    no_phi = c(1, 1, 0),
    add_rinst = r_inst_5
  ),
  optimal2 = list(
    add_ib = c(-0.005, -0.008, 0),
    no_rule = c(0),
    no_phi = c( 0),
    add_rinst = r_inst_5 
    ),
  no2027 = list(
    add_ib = c(-0.005, 0, 0),
    no_rule = c(1, 1, 0, 0),
    no_phi = c(1, 1, 0),
    add_rinst = r_inst_5
  ),
  d2037 = list(
    cons_d = 15,
    cons_dt = 12,
    add_ib = c(-0.005, -0.008, 0),
    no_rule = c(1, 1, 0),
    no_phi = c(1, 1, 0),
    add_rinst = r_inst_5
  ),
  d2030 = list(
    cons_d = 22,
    cons_dt = 5,
    add_ib = c(-0.005, -0.008, 0),
    no_rule = c(1, 1, 0),
    no_phi = c(1, 1, 0),
    add_rinst = r_inst_5
  ),
  nomult = list(
    og_po = 0,
    og_dep = 0,
    add_ib = c(-0.005, -0.008, 0),
    no_rule = c(1, 1, 0),
    no_phi = c(1, 1, 0),
    add_rinst = r_inst_5
  ),
  ogpers = list(
    og_mce = 0.3,
    add_ib = c(-0.005, -0.008, 0),
    no_rule = c(1, 1, 0),
    no_phi = c(1, 1, 0),
    add_rinst = r_inst_5
  ),
  highmult = list(
    og_po = 0.75,
    og_dep = 1.45,
    add_ib = c(-0.005, -0.008, 0),
    no_rule = c(1, 1, 0),
    no_phi = c(1, 1, 0),
    add_rinst = r_inst_5
  ),
  multivar = list(
    multivar = 1,
    og_bar = -.02,
    og_po = 0.5,
    og_dep = 1.2,
    og_rn = 0.01,
    og_po_plus = 0.75,
    og_dep_plus = 1.5,
    add_ib = c(-0.005, -0.008, 0),
    no_rule = c(1, 1, 0),
    no_phi = c(1, 1, 0),
    add_rinst = r_inst_5
  ),
  lr = list(
    add_ib = c(0),
    no_rule = c(0),
    no_phi = 0
  ))
