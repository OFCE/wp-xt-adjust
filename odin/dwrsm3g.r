# dwrsm3g — déclinaison infra-annuelle de dwrsm3 (g : « granulaire »)
#
# Le pas du modèle est 1/n_sub année (n_sub = 12 : mensuel ; n_sub = 1 : annuel).
# Avec n_sub = 1, dwrsm3g est équivalent à dwrsm3 (test V0, tolérance ~1e-12 en
# déterministe : toutes les transformations ci-dessous se réduisent exactement
# à celles de dwrsm3 quand n_sub = 1). dwrsm3 reste le modèle de production ;
# dwrsm3g sert à explorer les phénomènes infra-annuels. Les commentaires
# d'équations sont dans odin/dwrsm3.r, ici on ne commente que les écarts.
#
# Conventions :
#  - an <- 1 + floor((step-1)/n_sub) : indice d'année du step calculé (les
#    exogènes restent annuelles, indicées par an) ; mth <- step - (an-1)*n_sub :
#    mois dans l'année (1 = janvier) ; is_jan / is_dec : step de janvier /
#    décembre (à n_sub = 1, ils coïncident avec le step annuel de dwrsm3) ;
#  - décisions annuelles : la règle budgétaire et l'impulsion sont évaluées à
#    chaque step mais appliquées (tdeppp, tpo) en janvier seulement ; la
#    décision de janvier est tenue constante le reste de l'année (états
#    lagib_*_dec, variables ib_*_eff) ; les multiplicateurs agissent via le
#    noyau kern (neutre : pleine réponse en janvier, tenue ensuite) ;
#  - add-ons de niveau (add_og, add_tcho, dette_oneoff) : appliqués en janvier ;
#    add-ons de taux annualisés (add_rinst, add_rapp, add_spstar) : appliqués
#    chaque mois ; add-ons d'inflation (add_inf, add_inf_e) : convertis en
#    choc mensuel (1+x)^(1/n_sub)-1 et appliqués chaque mois ;
#  - ratios et taux restent annualisés à chaque mois (tpo, tdeppp, r_app,
#    r_inst, ...) ; les ratios de flux budgétaires (spp, ci, sep, ssp, tdep,
#    tdeppp) sont sortis en cumul YTD (ratio de sommes depuis janvier ; valeur
#    de décembre = ratio annuel exact de dwrsm3) ; les flux de valeur sont
#    annualisés et l'accumulation de dette est mensuelle (ddette/n_sub) ;
#  - niveaux (pibpot, ppib, dette) : croissance mensuelle géométrique
#    (1+g)^(1/n_sub)-1 ; aux frontières d'année ils coïncident avec dwrsm3 ;
#  - ECM mensuels x_m = 1-(1-x)^(1/n_sub) ; momentum apparié par racines
#    annuelles x^(1/n_sub) (og_lag, inf_lag) ; og_ddpot rescalé par n_sub
#    (l'accélération est mesurée au pas mensuel) ;
#  - bruit : AR(1) racine appariée ogn_ar_m = ogn_ar^(1/n_sub) ; les
#    innovations sont rescalées par k_ogn et k_inf (user(1), calculés côté R
#    dans sub_noise_factors, R/dwr_sim.r) pour que l'injection annuelle de
#    bruit sur og et l'inflation de décembre sur décembre gardent la variance
#    de dwrsm3 (l'injection annuelle sur og est la somme pondérée des bruits
#    mensuels, l'inflation annuelle la somme des déviations mensuelles) ;
#  - perte : sémantique annuelle conservée — les termes en retards
#    (loss_d_only) s'engagent en janvier (les retards sont alors les valeurs
#    de décembre, exactes aux frontières d'année), les termes instantanés
#    (cum_ogp, minog, cum_no_d) s'engagent en décembre (fin d'année) ;
#    actualisation mensuelle (1+loss_df)^(1/n_sub) ; seuil sur l'indice
#    d'année (an >= loss_t) ;
#  - noyaux infra-annuels optionnels : mkern[mth] (profil de la réponse des
#    multiplicateurs, doit sommer à 1) et pkern[mth] (vitesse de réaction des
#    prix par mois) ; par défaut (mkern_use = 0, pkern_use = 0) : réponse
#    pleine en janvier tenue constante (neutre au sens de dwrsm3) et ECM
#    mensuel des prix.
#
# Contrat côté R : simuler exactement 1:(periods*n_sub+1) puis agréger à
# l'année (décembre pour stocks, cumulants et ratios de flux — le solde et
# Consort sont calculés dans le modèle comme ratios de sommes YTD, leur valeur
# de décembre est le ratio annuel ; moyenne pour les autres taux/écarts ;
# composition géométrique pour les croissances) — voir aggregate_annual() dans
# R/dwr_sim.r et onesim() dans R/dwr_optim.r. n_sub est un paramètre entier.
#
# endogènes -----------------
## mêmes variables que dwrsm3 (cf odin/dwrsm3.r)

# exogènes --------------
## exogènes annuelles, indicées par an

# infra-annualité -------------------
n_sub <- user(1)
an <- 1 + floor((step - 1) / n_sub)
mth <- step - (an - 1) * n_sub
is_jan <- (mth == 1)
is_dec <- (mth == n_sub)

# vitesses mensuelles (ECM géométriques, momentum apparié par racines)
og_mce_m <- 1 - (1 - og_mce)^(1/n_sub)
og_lag_m <- og_lag^(1/n_sub)
tcho_mce_m <- 1 - (1 - tcho_mce)^(1/n_sub)
tvnairu_mce_m <- 1 - (1 - tvnairu_mce)^(1/n_sub)
inf_mce_m <- 1 - (1 - inf_mce)^(1/n_sub)
inf_lag_m <- inf_lag^(1/n_sub)
ec_mce_m <- 1 - (1 - ec_mce)^(1/n_sub)

# rescalage du bruit : racine appariée ; les facteurs k_ogn/k_inf (user(1),
# cf sub_noise_factors dans R/dwr_sim.r) rescalent les innovations pour que
# les agrégats annuels aux frontières d'année gardent la variance de dwrsm3
ogn_ar_m <- ogn_ar^(1/n_sub)

# chocs annuels convertis en chocs mensuels
gpot_m <- (1 + gpot[an])^(1/n_sub) - 1
add_inf_m <- (1 + add_inf[an])^(1/n_sub) - 1
add_inf_e_m <- (1 + add_inf_e[an])^(1/n_sub) - 1
infstar_m <- (1 + infstar)^(1/n_sub) - 1

# taux annualisés équivalents au mois courant
gpot_v_a <- (1 + gpot_v)^n_sub - 1
txppib_a <- (1 + txppib)^n_sub - 1

# noyaux infra-annuels -------------------
mkern_use <- user(0)
pkern_use <- user(0)
dim(mkern) <- n_sub
mkern[] <- user(1)
dim(pkern) <- n_sub
pkern[] <- user(1)
kern <- (1 - mkern_use) * is_jan + mkern_use * mkern[mth]
inf_mce_eff <- inf_mce_m * ((1 - pkern_use) + pkern_use * pkern[mth])

# écart de production -------------------
# add_og : choc de niveau, appliqué en janvier
og <- lagog + dog + is_jan * add_og[an]
# og_ddpot rescalé par n_sub : l'accélération est mesurée au pas mensuel
# (nul au sein de l'année, le saut de janvier reproduit le terme annuel)
dog_auto <- og_lag_m * (lagog - lag2og) -
  og_mce_m * (lagog - 0) -
  og_ddpot * n_sub * ((pibpot/lagpibpot-1)-(lagpibpot/lag2pibpot-1)) -
  pot_r * (r_inst-lagr_inst)
dog <- dog_auto +
  kern * (vog_dep * ib_dep_eff + vog_po * ib_po_eff) +
  og_cin * ib_cin +
  og_noise

# on retarde de cette façon, cela permet de passer une condition initiale
# l'échelle d'innovation reste annuelle (ogn_sigma*og_mce), rescalée par k_ogn
update(og_noise) <- ogn_ar_m * og_noise + (1 - ogn_ar_m) * rnorm(0, ogn_sigma * og_mce * k_ogn * stochastic)
update(lagog) <- og
update(lag2og) <- lagog

# multiplicateur variable ----------

og_rcn <- (lagog - og_bar) / og_rn
vog_dep <- (og_dep * exp(og_rcn) + og_dep_plus * exp(-og_rcn))/(exp(og_rcn) + exp(-og_rcn))*multivar[an]+
  (1-multivar[an])*og_dep
vog_po <- (og_po * exp(og_rcn) + og_po_plus * exp(-og_rcn))/(exp(og_rcn) +  exp(-og_rcn))*multivar[an]+
  (1-multivar[an])*og_po

vog <-  (vog_dep  + vog_po)/2

# chômage&NAIRU -------------------
# add_tcho : choc de niveau, appliqué en janvier
tcho <- lagtcho - tcho_okun * dog - tcho_mce_m * (lagtcho - lagtvnairu + lagog)  + is_jan * add_tcho[an]
update(lagtcho) <- tcho
update(lag2tcho) <- lagtcho

tvnairu <- lagtvnairu - tvnairu_mce_m * (lagtvnairu - nairu)
update(lagtvnairu) <- tvnairu

# prix du pib -------------------
# add_inf : choc de taux annuel converti en choc mensuel, appliqué chaque mois
txppib_i <- lagtxppib_i +
  inf_lag_m * (lagtxppib_i - lag2txppib_i) +
  - inf_mce_eff * (lagtxppib_i - infstar_m + inf_tcho * (lagtcho - lagtvnairu)) +
  add_inf_m + inf_noise

# l'écart-type mensuel est l'écart-type annuel multiplié par k_inf
inf_noise <- rnorm(0, inf_sigma * inf_mce * k_inf * stochastic)

ppib_i <- (1 + txppib_i)*lagppib_i
update(lagppib_i) <- ppib_i
update(lag2ppib_i) <- lagppib_i
update(lag3ppib_i) <- lag2ppib_i
lagtxppib_i <- lagppib_i/lag2ppib_i-1 
lag2txppib_i <- lag2ppib_i/lag3ppib_i-1 

txppib <- txppib_i + add_inf_e_m
lagtxppib <- lagppib/lag2ppib-1 
ppib <- (1 + txppib)*lagppib
update(lagppib) <- ppib
update(lag2ppib) <- lagppib

# pib volume&valeur&potentiel -----------
# croissance potentielle mensuelle géométrique
pibpot <- lagpibpot*(1+(gpot_m - 1*((tvnairu-lagtvnairu)/(1-(tvnairu-i_lagtvnairu)))))
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

# niveaux de fin d'année (frontières annuelles exactes pour la règle et la perte)
update(lagppib_dec) <- is_dec * ppib + (1 - is_dec) * lagppib_dec
update(lag2ppib_dec) <- is_dec * lagppib_dec + (1 - is_dec) * lag2ppib_dec
update(lagqpib_dec) <- is_dec * qpib + (1 - is_dec) * lagqpib_dec
update(lag2qpib_dec) <- is_dec * lagqpib_dec + (1 - is_dec) * lag2qpib_dec
# inflation et croissances glissantes sur un an (aux frontières d'année,
# identiques aux grandeurs annuelles de dwrsm3)
lagtxppib_y <- lagppib_dec/lag2ppib_dec-1
laggpib_y <- lagqpib_dec/lag2qpib_dec-1
laggvpib_y <- laggpib_y + lagtxppib_y

# dette ------------
# dette_oneoff : choc de niveau, appliqué en janvier
# flux annualisés, accumulation mensuelle
dette <- lagdette + ddette + is_jan * dette_oneoff[an]
update(lagdette) <- dette
update(lag2dette) <- lagdette
dettep <- dette/vpib
dettepp <- dette/pibpot/ppib
lagdettep <- lagdette/lagvpib
update(lagdettepp) <- dettepp
lag2dettep <- lag2dette/lag2vpib
ddette <- (- rec_po + dep_prim + cin - taut[an] * vpib) / n_sub

# taux d'intérêt ---------------
# r_inst : taux annualisé (gpot_v_a, txppib_a annualisés) ; add_rinst, add_rapp
# appliqués chaque mois ; r_app : moyenne mobile de fenêtre r_mat*n_sub mois
r_inst <- gpot_v_a + (0.2 * txppib_a + 0.8 * infstar) + 
  ecart_c + 
  r_dette * lagdettep - r_dette_star * dstarr + add_rinst[an]
r_app <- (1-1/(r_mat*n_sub)) * lagr_app +  1/(r_mat*n_sub) * r_inst + add_rapp[an]
update(lagr_app) <- r_app
ecart_c <- lagecart_c + decart_c
decart_c <- -ec_mce_m * (lagecart_c - ecstar)
update(lagecart_c) <- ecart_c 
update(lagr_inst) <- r_inst 

# impulsion --------------
# l'impulsion sur les dépenses est définie comme la variation du taux des dépenses par rapport au potentiel
dep_prim <- tdeppp * ppib * pibpot + dep_og * lagog

update(lagtdeppp) <- tdeppp
update(lag2tdeppp) <- lagtdeppp

tdepp_e <- dep_prim/pibpot/ppib
update(lagtdepp_e) <- tdepp_e 

# l'impulsion sur les recettes est définie comme la variation du tpo
rec_po <- tpo * vpib
update(lagtpo) <- tpo

# l'impulsion sur les intérêts (le multiplicateur est suceptible d'être nul (og_int))
# charge d'intérêts nette de la part BC, annualisée
cin  <- r_app * lagdette - dettebc * lagdette * r_bc
cinp <- cin / vpib
update(lagcinp) <- cinp
update(lag2cinp) <- lagcinp

# variation au pas mensuel : la somme télescopée sur l'année redonne la
# variation annuelle de dwrsm3
ib_cin <- lagcinp - lag2cinp

# cumulants intra-année --------------
# flux annualisés cumulés depuis janvier. Sémantique d'odin : dans update(),
# les états référencés au membre droit valent leur valeur en DÉBUT de step ;
# on sépare donc l'état cumulé au step précédent (cum_*) de la somme YTD
# incluant le mois courant (sum_*), seule à alimenter les ratios et les
# sorties. Au step de décembre, sum_* somme l'année entière : les ratios
# ci-dessous valent alors les ratios annuels de dwrsm3 comme ratio de deux
# sommes (Σ flux / Σ vpib), cohérents avec l'accumulation de dette
# (Σ ddette = - sep_ytd * Σ vpib). À n_sub = 1 : cum_* reste nul, sum_* vaut
# le flux courant et les ratios coïncident exactement avec dwrsm3.
sum_vpib <- is_jan * vpib + (1 - is_jan) * (cum_vpib + vpib)
sum_rec_po <- is_jan * rec_po + (1 - is_jan) * (cum_rec_po + rec_po)
sum_dep_prim <- is_jan * dep_prim + (1 - is_jan) * (cum_dep_prim + dep_prim)
sum_cin <- is_jan * cin + (1 - is_jan) * (cum_cin + cin)
sum_pibpot_ppib <- is_jan * ppib * pibpot + (1 - is_jan) * (cum_pibpot_ppib + ppib * pibpot)
sum_og <- is_jan * og + (1 - is_jan) * (cum_og + og)
update(cum_vpib) <- sum_vpib
update(cum_rec_po) <- sum_rec_po
update(cum_dep_prim) <- sum_dep_prim
update(cum_cin) <- sum_cin
update(cum_pibpot_ppib) <- sum_pibpot_ppib
update(cum_og) <- sum_og
initial(cum_vpib) <- 0
initial(cum_rec_po) <- 0
initial(cum_dep_prim) <- 0
initial(cum_cin) <- 0
initial(cum_pibpot_ppib) <- 0
initial(cum_og) <- 0

# ratios YTD (valeur de décembre = ratio annuel « ratio de sommes »)
# og_ytd : moyenne YTD de l'og (cohérente avec l'agrégation « mean » de og),
# utilisée pour l'ajustement cyclique de ssp
og_ytd <- sum_og / mth
spp_ytd <- (sum_rec_po - sum_dep_prim) / sum_vpib + taut[an]
cinp_ytd <- sum_cin / sum_vpib
sep_ytd <- spp_ytd - cinp_ytd
ssp_ytd <- sep_ytd - tpo * og_ytd / (1 + og_ytd)
tdep_ytd <- sum_dep_prim / sum_vpib
tdepp_ytd <- sum_dep_prim / sum_pibpot_ppib

# ib après loss_t, définie par la stabilité de la dette --------------
ktau_og <- 1/(1 - tpo * (pcpo * vog_po + (1-pcpo) * vog_dep))
update(lagktau_og) <- ktau_og
# calculé pour diagnostic (non utilisé), dog_auto est mensuel ici
ib_sup_inf <-  lagktau_og * (- lagdettep + (1-beta) * lag2dettep + lagspp + lagtpo * dog_auto)
update(lagib_sup_inf) <- ib_sup_inf
betastar <- (lagr_app - gpot_v_a - infstar) / (1 + gpot_v_a + infstar)
beta <- (lagr_app - laggvpib_y) / (1 + laggvpib_y)
sppsd <-  beta * lagdettep + add_spstar[an]
update(lagsppsd) <- sppsd

# règle budgétaire --------------
# lagspp s'engage en décembre sur le solde annuel (spp_ytd en décembre = ratio
# de sommes de l'année close) ; la règle de janvier le lit ainsi comme dwrsm3
# lit le solde de l'année précédente. À n_sub = 1 : identique à dwrsm3.
update(lagspp) <- is_dec * spp_ytd + (1 - is_dec) * lagspp
update(lag2spp) <- lagspp
spstar <- (ecart_c+r_dette*dstar-r_dette_star*dstarr)/(1+gpot_v_a+infstar) * dstar

update(lagog_cc) <- og_cc[an]
initial(lagog_cc) <- i_lagog

update(lagspp_cc) <- spp_cc[an]
initial(lagspp_cc) <- i_lagspp

update(lagdettep_cc) <- dettep_cc[an]
initial(lagdettep_cc) <- i_lagdette/i_lagqpib/i_lagppib

update(laginf_cc) <- inf_cc[an]
initial(laginf_cc) <- i_lagppib/i_lag2ppib-1

# évaluée à chaque step mais appliquée en janvier seulement ; au step de
# janvier, les retards sont les valeurs de décembre (frontières exactes) et
# lagtxppib_y est l'inflation glissante sur un an
ib_fr <- tpo_sstar * (lagspp - (spp_cc[an]==0) * sppsd - (spp_cc[an]!=0) * lagspp_cc) +
  tpo_dstar * (lagdettep - (dettep_cc[an]==0) * dstar - (dettep_cc[an]!=0) *lagdettep_cc) +
  tpo_og*(lagog - (og_cc[an]!=0) *lagog_cc) +
  tpo_ec*(ecart_c - (ec_cc[an]==0)*ecstar - ec_cc[an]) +
  tpo_inf*(lagtxppib_y - (inf_cc[an]==0)*infstar - laginf_cc ) 

ib_fre <- ib_fr * (1-no_rule[an]) +
  phi[an]*(1-no_phi[an])

ib_sup <- max(min(ib_fre, ibcap), -ibcap)  +
  add_ib[an]  

ib_spont_dep <- tx_deriv_dep[an] * lagtdeppp 
ib_dep <- ib_spont_dep + (1-pcpo) * ib_sup + add_tdeppp[an]
# application en janvier seulement : tdeppp/tpo constants intra-année
tdeppp <- lagtdeppp + is_jan * ib_dep

ib_spont_po <-  - tx_deriv_po[an] * lagtpo
ib_po <- ib_spont_po + pcpo * ib_sup - add_tpo[an]
tpo <- lagtpo - is_jan * ib_po

# décisions de janvier, tenues constantes dans l'année
update(lagib_dep_dec) <- is_jan * ib_dep + (1 - is_jan) * lagib_dep_dec
update(lagib_po_dec) <- is_jan * ib_po + (1 - is_jan) * lagib_po_dec
update(lagib_sup_dec) <- is_jan * ib_sup + (1 - is_jan) * lagib_sup_dec
ib_dep_eff <- is_jan * ib_dep + (1 - is_jan) * lagib_dep_dec
ib_po_eff <- is_jan * ib_po + (1 - is_jan) * lagib_po_dec
ib_sup_eff <- is_jan * ib_sup + (1 - is_jan) * lagib_sup_dec

# fonction de perte ----------------------
# utilisée pour la partie optimisation ; sémantique annuelle :
#  - termes en retards engagés en janvier (retards = valeurs de décembre) ;
#  - termes instantanés engagés en décembre (fin d'année) ;
#  - actualisation mensuelle (1+loss_df)^(1/n_sub) ; seuil sur l'indice d'année.
update(loss_d_only) <- loss_d_only +
  is_jan * loss_d * (an >= loss_t) * (lagdettep - dstar)^2 + 
  is_jan * loss_sp * (an >= loss_t) * (lagspp - lagdettep * beta)^2
initial(loss_d_only) <- 0
update(df) <- df/(1+loss_df)^(1/n_sub)
initial(df) <- 1
update(cum_ogp) <- cum_ogp + is_dec * (og<0) * og * loss_sogp * df
initial(cum_ogp) <- 0
minof <- (og<minog)*(1-no_rule[an])
update(minog) <- is_dec * ((minof)*og +(1-minof)*minog) + (1-is_dec)*minog
initial(minog) <- 0

update(cum_no_d) <- cum_no_d + 
    is_dec * (loss_og * og^2 * df + 
    loss_ibp * (ib_sup_eff * (ib_sup_eff>0))^2 * df +
    loss_ib * (ib_sup_eff)^2 * df +
    loss_tcho * (tcho-tvnairu)^2 * df)
initial(cum_no_d) <- 0
loss_no_d <- cum_ogp^2 + loss_mo * abs(minog) + cum_no_d

# sorties ------
# dans dust c'est un peu capilotracté
# sans output, on est obligé de faire un update()
# et de définir une CI nulle
# on pense ensuite à décaler les variables pour les utilisations
# 
# ib_* : décision de janvier tenue constante (ib_*_eff)
update(dettep_o) <- dettep
update(dettepp_o) <- dettepp
update(og_o) <- og
update(txppib_o) <- ppib/lagppib-1
update(gpib_o) <- qpib/lagqpib-1
update(gpot_o) <- gpot_v
update(spp_o) <- spp_ytd
update(ci_o) <- cinp_ytd
update(sep_o) <- sep_ytd
update(ssp_o) <- ssp_ytd
update(tpo_o) <- tpo
update(tdep_o) <- tdep_ytd
update(taut_o) <- taut[an]
update(tdeppp_o) <- tdepp_ytd
update(ib_dep_o) <- ib_dep_eff
update(ib_po_o) <- ib_po_eff
update(ib_o) <- ib_po_eff + ib_dep_eff
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
update(vog_o) <- vog

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
initial(vog_o) <- 0
initial(og_noise) <- (1-ogn_ar_m)*rnorm(0, ogn_sigma * og_mce * k_ogn * stochastic)
initial(lagib_dep_dec) <- 0
initial(lagib_po_dec) <- 0
initial(lagib_sup_dec) <- 0
initial(lagppib_dec) <- i_lagppib
initial(lag2ppib_dec) <- i_lag2ppib
initial(lagqpib_dec) <- i_lagqpib
# dwrsm3 initialise son état laggpib à i_lag2qpib/i_lag3qpib-1 (croissance de
# start-1, un an de retard sur le schéma de régime) : on réplique cette CI pour
# que laggpib_y coïncide avec laggpib dès la première année simulée
initial(lag2qpib_dec) <- i_lagqpib * i_lag3qpib / i_lag2qpib

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
initial(lagog) <- i_lagog
initial(lag2og) <- i_lag2og
initial(lagtcho) <- i_lagtcho
initial(lag2tcho) <- i_lag2tcho
initial(lagtvnairu) <- i_lagtvnairu
# les taux mensuels sont dérivés de ces niveaux : on part de l'équivalent
# mensuel des taux annuels historiques (exacts à n_sub = 1 : i_lag2ppib, i_lag3ppib)
initial(lagppib_i) <- i_lagppib
initial(lag2ppib_i) <- i_lagppib / (i_lagppib/i_lag2ppib)^(1/n_sub)
initial(lag3ppib_i) <- i_lagppib / ((i_lagppib/i_lag2ppib)*(i_lag2ppib/i_lag3ppib))^(1/n_sub)
initial(lagppib) <- i_lagppib
initial(lag2ppib) <- i_lag2ppib
initial(lagqpib) <- i_lagqpib
initial(lag2qpib) <- i_lag2qpib
initial(laggpib) <- i_lag2qpib/i_lag3qpib - 1
initial(lagpibpot) <- i_lagpibpot
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
# stochastic : multiplicateur des écarts-types du bruit (0 = déterministe)
stochastic <- user(1)
# facteurs de rescalage du bruit infra-annuel (cf sub_noise_factors, R/dwr_sim.r)
k_ogn <- user(1.0)
k_inf <- user(1.0)
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
no_phi[] <- user(0)
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
dim(no_phi) <- periods
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
