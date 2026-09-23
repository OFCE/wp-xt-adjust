# avec output true

# endogènes -----------------
## qpib pib en volume
## vpib pib en valeur (mds euro)
## og écart de production
## ppib prix du pib
## inf  inflation
## imp_dep, imp_po, imp_int décompositon de l'impulsion budgétaire en trois composantes
##                           dépenses, recettes, et intérêt
## dette dette publique, en valeur (mds euro)
## tppdette dette publique en point de pib potentiel
## dettep dette publique en point de pib
## cha_int  charge d'intérêt en valeur (mds euro)
## tchaint  charge d'intérêt en point de pib potentiel
## r_app    taux d'intérêt souverain national apparent (1% = 0.01)
## r_inst   taux d'intérêt souverain instantané (moyen année) (1% = 0.01)
## gpot croissance potentielle en volume
## aide de odin https://mrc-ide.github.io/odin/
## tcho est le taux de chômage au sens du BIT 
## tvnairu est le NAIRU de AMECO tandis que Nairu est le chômage d'équilibre 

# exogènes --------------
## dep_prim dépenses primaires en valeur
## rec_po   recettes fiscales (et non fiscales) en valeur (mds euro)
## pibpot pib potentiel en volume

# écart de production -------------------
og <- lagog + dog + add_og[step]
dog_auto <- og_lag * (lagog - lag2og) -
  og_mce * (lagog - 0) - 
  og_ddpot * ((pibpot/lagpibpot-1)-(lagpibpot/lag2pibpot-1)) -
  pot_r * (r_inst-lagr_inst)
dog <- dog_auto +
  vog_dep * ib_dep + 
  vog_po * ib_po + 
  og_cin * ib_cin +
  og_noise 
  
# on retarde de cette façon, cela permet de passer une condition initiale
update(og_noise) <- ogn_ar * og_noise + (1-ogn_ar) * rnorm(0, ogn_sigma*og_mce)
update(lagog) <- og
update(lag2og) <- lagog

# multiplicateur variable ----------

og_rcn <- (lagog - og_bar) / og_rn
vog_dep <- (og_dep * exp(og_rcn) + og_dep_plus * exp(-og_rcn))/(exp(og_rcn) + exp(-og_rcn))*multivar[step]+
  (1-multivar[step])*og_dep
vog_po <- (og_po * exp(og_rcn) + og_po_plus * exp(-og_rcn))/(exp(og_rcn) +  exp(-og_rcn))*multivar[step]+
  (1-multivar[step])*og_po


# chômage&NAIRU -------------------
tcho <- lagtcho - tcho_okun * dog - tcho_mce * (lagtcho - lagtvnairu + lagog)  + add_tcho[step]
update(lagtcho) <- tcho
update(lag2tcho) <- lagtcho

tvnairu <- lagtvnairu - tvnairu_mce * (lagtvnairu - nairu)
update(lagtvnairu) <- tvnairu

# prix du pib -------------------
txppib_i <- lagtxppib_i +
  inf_lag * (lagtxppib_i - lag2txppib_i) +
  - inf_mce * (lagtxppib_i - infstar + inf_tcho * (lagtcho - lagtvnairu)) +
  add_inf[step] + inf_noise

inf_noise <- rnorm(0, inf_sigma*inf_mce)

ppib_i <- (1 + txppib_i)*lagppib_i
update(lagppib_i) <- ppib_i
update(lag2ppib_i) <- lagppib_i
update(lag3ppib_i) <- lag2ppib_i
lagtxppib_i <- lagppib_i/lag2ppib_i-1 
lag2txppib_i <- lag2ppib_i/lag3ppib_i-1 

txppib <- txppib_i + add_inf_e[step]
lagtxppib <- lagppib/lag2ppib-1 
ppib <- (1 + txppib)*lagppib
update(lagppib) <- ppib
update(lag2ppib) <- lagppib

# pib volume&valeur&potentiel -----------
pibpot <- lagpibpot*(1+(gpot[step]-1*((tvnairu-lagtvnairu)/(1-(tvnairu-i_lagtvnairu)))))
update(lagpibpot) <- pibpot
update(lag2pibpot) <- lagpibpot
qpib <- (1 + og) * pibpot
vpib <- qpib * ppib
lagvpib <- lagqpib * lagppib
lag2vpib <- lag2qpib * lag2ppib
update(lagqpib) <- qpib
update(lag2qpib) <- lagqpib
gpib <- qpib/lagqpib-1
update(laggpib) <- gpib
gvpib <- gpib + txppib
laggvpib <- laggpib + lagtxppib
gpot_v <- pibpot/lagpibpot-1

# dette ------------
dette <- lagdette + ddette + dette_oneoff[step]
update(lagdette) <- dette
update(lag2dette) <- lagdette
dettep <- dette/vpib
dettepp <- dette/pibpot/ppib
lagdettep <- lagdette/lagvpib
update(lagdettepp) <- dettepp
lag2dettep <- lag2dette/lag2vpib
ddette <- - rec_po + dep_prim + cin - taut[step] * vpib

# taux d'intérêt ---------------
r_inst <- gpot_v + (0.2 * txppib + 0.8 * infstar) + 
  ecart_c + 
  r_dette * lagdettep - r_dette_star * dstarr + add_rinst[step]
r_app <- (1-1/r_mat) * lagr_app +  1/r_mat * r_inst + add_rapp[step]
update(lagr_app) <- r_app
ecart_c <- lagecart_c + decart_c
decart_c <- -ec_mce * (lagecart_c - ecstar)
update(lagecart_c) <- ecart_c 
update(lagr_inst) <- r_inst 

# impulsion --------------
# l'impulsion sur les dépenses est définie comme la variation du taux des dépenses par rapport au potentiel
dep_prim <- tdeppp * ppib * pibpot + dep_og * lagog

# ppib_dep <- lagppib_dep*(1+ infdep_star*txppib + (1-infdep_star)*lagtxppib_dep - infdep_mce*(lagppib_dep/lagppib-1))
# pibpot_dep <- lagpibpot_dep*(1+ potdep_star*gpot_v + (1-potdep_star)*lagtxpibpot_dep - potdep_mce*(lagpibpot_dep/lagpibpot-1))
# 
# #pibpot_dep <- lagpibpot_dep*(1+(-((lag2pibpot_dep-lagpibpot_dep)/lag2pibpot_dep)-potdep_mce*(lagpibpot_dep/lagpibpot-1)))
# lagtxppib_dep <- (lagppib_dep-lag2ppib_dep)/lag2ppib_dep
# lagtxpibpot_dep <- (lagpibpot_dep-lag2pibpot_dep)/lag2pibpot_dep
# 
# update(lagppib_dep) <- ppib_dep
# update(lag2ppib_dep) <- lagppib_dep
# update(lagpibpot_dep) <- pibpot_dep
# update(lag2pibpot_dep) <- lagpibpot_dep

update(lagtdeppp) <- tdeppp
update(lag2tdeppp) <- lagtdeppp

tdepp_e <- dep_prim/pibpot/ppib
update(lagtdepp_e) <- tdepp_e 

# l'impulsion sur les recettes est définie comme la variation du tpo
rec_po <- tpo * vpib
update(lagtpo) <- tpo

# l'impulsion sur les intérêts (le multiplicateur est suceptible d'être nul (og_int))
# charge d'intérêts nette de la part BC
cin  <- r_app * lagdette - dettebc * lagdette * r_bc
cinp <- cin / vpib
update(lagcinp) <- cinp
update(lag2cinp) <- lagcinp

ib_cin <- lagcinp - lag2cinp

# ib après loss_t, définie par la stabilité de la dette --------------
ktau_og <- 1/(1 - tpo * (pcpo * vog_po + (1-pcpo) * vog_dep))
update(lagktau_og) <- ktau_og
ib_sup_inf <-  lagktau_og * (- lagdettep + (1-beta) * lag2dettep + lagspp + lagtpo * dog_auto)
update(lagib_sup_inf) <- ib_sup_inf
betastar <- (lagr_app - gpot_v - infstar) / (1 + gpot_v + infstar)
beta <- (lagr_app - laggvpib) / (1 + laggvpib)
sppsd <-  lagdettep * beta + add_spstar[step]
update(lagsppsd) <- sppsd

# règle budgétaire --------------
spp <- (rec_po - dep_prim)/vpib + taut[step]
update(lagspp) <- spp
update(lag2spp) <- lagspp
spstar <- (ecart_c+r_dette*dstar-r_dette_star*dstarr)/(1+gpot_v+infstar) * dstar

update(lagog_cc) <- og_cc[step]
initial(lagog_cc) <- i_lagog

update(lagspp_cc) <- spp_cc[step]
initial(lagspp_cc) <- i_lagspp

update(lagdettep_cc) <- dettep_cc[step]
initial(lagdettep_cc) <- i_lagdette/i_lagqpib/i_lagppib

update(laginf_cc) <- inf_cc[step]
initial(laginf_cc) <- i_lagppib/i_lag2ppib-1

ib_fr <- tpo_sstar * (lagspp - (spp_cc[step]==0) * sppsd - (spp_cc[step]!=0) * lagspp_cc) +
  tpo_dstar * (lagdettep - (dettep_cc[step]==0) * dstar - (dettep_cc[step]!=0) *lagdettep_cc) +
  tpo_og*(lagog - (og_cc[step]!=0) *lagog_cc) +
  tpo_ec*(ecart_c - (ec_cc[step]==0)*ecstar - ec_cc[step]) +
  tpo_inf*(lagtxppib - (inf_cc[step]==0)*infstar - laginf_cc ) +
  phi[step]

ib_fre <- ib_fr * (1-no_rule[step])

ib_sup <- max(min(ib_fre, ibcap), -ibcap)  +
  add_ib[step]  
# +
# (ib_sup_inf * (step>=loss_t) ) * 0.

ib_spont_dep <- tx_deriv_dep[step] * lagtdeppp 
  # lagtdeppp * (lagppib_dep*lagpibpot_dep / lagpibpot / lagppib - lag2ppib_dep * lag2pibpot_dep/ lag2pibpot / lag2ppib)
ib_dep <- ib_spont_dep + (1-pcpo) * ib_sup + add_tdeppp[step]
tdeppp <- lagtdeppp + ib_dep

ib_spont_po <-  - tx_deriv_po[step] * lagtpo
ib_po <- ib_spont_po + pcpo * ib_sup - add_tpo[step]
tpo <- lagtpo - ib_po

# fonction de perte ----------------------
# utilisée pour la partie optimisation
update(loss_d_only) <- loss_d_only +
  loss_d * (step >= loss_t) * (lagdettep - dstar)^2 + 
  loss_sp * (step >= loss_t) * (lagspp - lagdettep * beta)^2
# print("dstar {dstar}")
initial(loss_d_only) <- 0
update(df) <- df/(1+loss_df)
initial(df) <- 1
update(cum_ogp) <- cum_ogp + (og<0) * og * loss_sogp * df
initial(cum_ogp) <- 0
minof <- (og<minog)*(1-no_rule[step])
update(minog) <- (minof)*og +(1-minof)*minog
initial(minog) <- 0

update(cum_no_d) <- cum_no_d + 
    loss_og * og^2 * df + 
    loss_ibp * (ib_sup * (ib_sup>0))^2 * df +
    loss_ib * (ib_sup)^2 * df +
    loss_tcho * (tcho-tvnairu)^2 * df
initial(cum_no_d) <- 0
loss_no_d <- cum_ogp^2 + loss_mo * abs(minog) + cum_no_d

# sorties ------
# dans dust c'est un peu capilotracté
# sans output, on est obligé de faire un update()
# et de définir une CI nulle
# on pense ensuite à décaler les variables pour les utilisations
# 
update(dettep_o) <- dettep
update(dettepp_o) <- dettepp
update(og_o) <- og
update(txppib_o) <- ppib/lagppib-1
update(gpib_o) <- qpib/lagqpib-1
update(gpot_o) <- gpot_v
update(spp_o) <- spp
update(ci_o) <- cinp
update(sep_o) <- - cinp + spp
update(ssp_o) <- -cinp + spp - tpo * og / (1+og)
update(tpo_o) <- tpo
update(tdep_o) <- tdepp_e * pibpot/qpib
update(taut_o) <- taut[step]
update(tdeppp_o) <- tdepp_e 
update(ib_dep_o) <- ib_dep
update(ib_po_o) <- ib_po
update(ib_o) <- ib_po + ib_dep
update(qpib_o) <- qpib
update(ppib_o) <- ppib
update(tcho_o) <- tcho
update(tvnairu_o) <- tvnairu
update(spstar_o) <- spstar
update(r_app_o) <- r_app
update(r_inst_o) <- r_inst
update(loss_o) <- loss_no_d + loss_d_only
update(loss_d_only_o) <- loss_d_only
update(loss_nd_o) <- loss_no_d
update(sppsd_o) <- sppsd
update(ec_o) <- ecart_c

initial(txppib_o) <- 0
initial(gpib_o) <- 0
initial(gpot_o) <- 0
initial(spp_o) <- 0
initial(sep_o) <- 0
initial(ssp_o) <- 0
initial(og_o) <- 0
initial(dettep_o) <- 0
initial(dettepp_o) <- 0
initial(ci_o) <- 0
initial(tpo_o) <- 0
initial(taut_o) <- 0
initial(tdep_o) <- 0
initial(ib_dep_o) <- 0
initial(ib_o) <- 0
initial(ib_po_o) <- 0
initial(qpib_o) <- 0
initial(ppib_o) <- 0
initial(tcho_o) <- 0
initial(tvnairu_o) <- 0
initial(tdeppp_o) <- 0
initial(spstar_o) <- 0
initial(r_app_o) <- 0
initial(r_inst_o) <- 0
initial(ec_o) <- 0

initial(loss_o) <- 0
initial(loss_nd_o) <- 0
initial(loss_d_only_o) <- 0
initial(lagib_sup_inf) <- 0
initial(lagktau_og) <- 0
initial(sppsd_o) <- 0

initial(og_noise) <- (1-ogn_ar)*rnorm(0, ogn_sigma*og_mce)

# init ----------------
i_lagog <- user(0)
i_lagtcho <- user(0)
i_lag2tcho <- user(0)
i_lagtvnairu <- user(0)
i_lag2og <- user(0)
i_lagqpib <- user(1)
i_lag2qpib <- user(1)
i_lag3qpib <- user(1)
i_lagppib <- user(1)
i_lag2ppib <- user(1)
i_lag3ppib <- user(1)
i_lagpibpot <- user(1)
i_lag2pibpot <- user(1)
i_lagtpo <- user(0.5)
i_lagtdeppp <- user(0.5)
i_lagr_app <- user(0.04)
i_lagr_inst <- user(0.04)
i_lagdette <- user(1)
i_lagspp <- user(0.0)
i_lag2spp <- user(0.0)
i_lagecart_c <- user(0.0)
i_ogn <-  user(0)
i_lagcip <- user(0.02)
i_lag2cip <- user(0.02)
i_lag2dette <- user(1)
i_lag2tdeppp <- user(0.5)

## Initial conditions
# initial(og) <- init_og
initial(lagog) <- i_lagog
initial(lag2og) <- i_lag2og
initial(lagtcho) <- i_lagtcho
initial(lag2tcho) <- i_lag2tcho
initial(lagtvnairu) <- i_lagtvnairu
initial(lagppib_i) <- i_lagppib
initial(lag2ppib_i) <- i_lag2ppib
initial(lag3ppib_i) <- i_lag3ppib
initial(lagppib) <- i_lagppib
initial(lag2ppib) <- i_lag2ppib
# initial(lagppib_dep) <- i_lagppib
# initial(lag2ppib_dep) <- i_lag2ppib
initial(lagqpib) <- i_lagqpib
initial(lag2qpib) <- i_lag2qpib
initial(laggpib) <- i_lag2qpib/i_lag3qpib - 1
initial(lagpibpot) <- i_lagpibpot
# initial(lagpibpot_dep) <- i_lagpibpot
# initial(lag2pibpot_dep) <- i_lag2pibpot
initial(lag2pibpot) <- i_lag2pibpot
initial(lagtpo) <- i_lagtpo
initial(lagtdepp_e) <- i_lagtdeppp
initial(lagtdeppp) <- i_lagtdeppp
initial(lag2tdeppp) <- i_lag2tdeppp
initial(lagr_app) <- i_lagr_app
initial(lagr_inst) <- i_lagr_inst
initial(lagdette) <- i_lagdette
initial(lagdettepp) <- i_lagdette/i_lagpibpot/i_lagppib
initial(lag2dette) <- i_lag2dette
initial(lagspp) <- i_lagspp
initial(lag2spp) <- i_lag2spp
initial(lagcinp) <- i_lagcip
initial(lag2cinp) <- i_lag2cip
initial(lagecart_c) <- i_lagecart_c
initial(lagsppsd) <-  i_lagdette/i_lagppib/i_lagqpib * (i_lagr_app - (i_lagppib*i_lagqpib/(i_lag2ppib*i_lag2qpib)-1))/(1+(i_lagppib*i_lagqpib/(i_lag2ppib*i_lag2qpib)-1))
# params -------------
periods <- user(50)

## parameters
og_lag   <- user(0)
og_mce   <- user(0.29)
og_ddpot   <- user(0.29)
dep_og <- user(0.0)
tvnairu_mce   <- user(0.13)
tcho_okun   <- user(0.33)
tcho_mce   <- user(0.27)
inf_tcho <- user(0.1)
inf_lag <- user(0.0)
inf_mce <- user(0.68)
inf_sigma <- user(0.005)
infdep_mce <- user(0.5)
infdep_star <- user(0.5)
potdep_star <- user(0.5)
potdep_mce <- user(0.5)
pot_r   <- user(0.15)
og_dep  <- user(0.7)
og_po  <-  user(0.7)
og_cin  <- user(0)
infstar <- user(0.0175)
r_mat   <- user(8)
dstar <- user(0.6)
dstarr <- user(0.6)
nairu <- user(0.07)
tpo_sstar <- user(0)
tpo_dstar <- user(0)
tpo_sstar2 <- user(0)
tpo_1sstar <- user(0)
tpo_dstar2 <- user(0)
tpo_1dstar <- user(0)
tpo_1og <- user(0)
tpo_og <- user(0)
tpo_og2 <- user(0)
tpo_ec <- user(0)
tpo_inf <- user(0)
pcpo <- user(1)
ogn_ar <- user(0)
ogn_sigma <- user(0.005)
r_dette <- user(0.01)
r_dette_star <- user(0.01)
ibcap <- user(0.01)
ecstar <- user(-0.015)
ec_mce <- user(0.1)
dettebc <- user(0)
r_bc <- user(0)

ibsup_inf <- user(0.3)
ibsup_d <- user(0.1)

og_bar <- user(-0.01)
og_rn <- user(0.01)
og_dep_plus <- user(2)
og_po_plus <- user(2)

# loss

loss_dog <- user(0)
loss_d <- user(10)
loss_sp <- user(0)
loss_t <- user(20)
loss_df <- user(0.0)
loss_ib <- user(0)
loss_ibp <- user(0)
loss_og <- user(1)
loss_ogp <- user(0)
loss_sog <- user(0)
loss_sogp <- user(0)
loss_mo <- user(0)
loss_exp <- user(2)
loss_tcho <- user(0)

# controls&exogenes -----------
dette_oneoff[] <- user(0)
phi[] <- user(0)
taut[] <- user(0)
add_og[] <- user(0)
add_og_e[] <- user(0)
gpot[] <- user(0)
tx_deriv_dep[] <- user(0)
tx_deriv_po[] <- user(0)
no_rule[] <- user(0)
add_ib[] <- user(0)
add_inf[] <- user(0)
add_inf_e[] <- user(0)
add_rapp[] <- user(0)
add_rinst[] <- user(0)
add_tcho[] <- user(0)
add_tpo[] <- user(0)
add_tdeppp[] <- user(0)
multivar[] <- user(0)
add_spstar[] <- user(0)

dettep_cc[] <- user(0)
spp_cc[] <- user(0)
og_cc[] <- user(0)
ec_cc[] <- user(0)
inf_cc[] <- user(0)

dim(dette_oneoff) <- periods
dim(phi) <- periods
dim(gpot) <- periods
dim(taut) <- periods
dim(add_og) <- periods
dim(add_tpo) <- periods
dim(add_tdeppp) <- periods
dim(add_og_e) <- periods
dim(tx_deriv_dep) <- periods
dim(tx_deriv_po) <- periods
dim(no_rule) <- periods
dim(add_ib) <- periods
dim(add_inf) <- periods
dim(add_inf_e) <- periods
dim(add_rapp) <- periods
dim(add_rinst) <- periods
dim(add_tcho) <- periods
dim(multivar) <- periods
dim(add_spstar) <- periods

dim(dettep_cc) <- periods
dim(spp_cc) <- periods
dim(og_cc) <- periods
dim(ec_cc) <- periods
dim(inf_cc) <- periods
