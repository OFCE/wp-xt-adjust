# exploration du modèle infra-annuel dwrsm3g
#
# dwrsm3g (odin/dwrsm3g.r) décline dwrsm3 au pas 1/n_sub année (mensuel si
# n_sub = 12) ; à n_sub = 1 il est identique à dwrsm3 (test V0). Les ratios de
# flux budgétaires (spp, ci, sep, ssp, tdep, tdeppp) y sont calculés comme
# ratios de sommes intra-année : leur valeur de décembre est le ratio annuel
# Σ flux / Σ vpib, cohérent avec l'accumulation de dette (identité de la
# section 4) ; l'agrégation annuelle les prend en décembre (groupe « dec » de
# .dwr.agg, R/dwr_sim.r). La règle de janvier lit le solde de l'année close
# via lagspp (engagé en décembre).
#
# sections :
#  1. variantes annuelle et mensuelle (n_sub = 1, 12)
#  2. V0 : identité dwrsm3g (n_sub = 1) = dwrsm3
#  3. V1 : écarts déterministes à noyaux neutres (n_sub = 12)
#  4. identité dette : Δdette = -sep(décembre) * Σvpib / n_sub
#  5. détail mensuel : noyaux mkern (multiplicateurs) et pkern (prix)
#  6. boucle fermée : règle active (optimal1), puis phis optimisés sur dwrsm3
#  7. stochastique : distribution annuelle n_sub = 12 vs modèle annuel

source("simulations init.R")

## 1. variantes ----
brg1 <- setup_params(params = list_modify(rs25, n_sub = 1L),
                     modele = "dwrsm3g", start_y = 2025, ameco_version = "5/2026")
brg12 <- setup_params(params = list_modify(rs25, n_sub = 12L),
                      modele = "dwrsm3g", start_y = 2025, ameco_version = "5/2026")

sim3 <- calc_sim_dust(brs25$params$p, brs25$globals$model, history = brs25$params$h)
simg1 <- calc_sim_dust(brg1$params$p, brg1$globals$model, history = brg1$params$h)
simg12 <- calc_sim_dust(brg12$params$p, brg12$globals$model, history = brg12$params$h)

# écart maximal par variable entre deux simulations (séries q0.5, années > 2025)
cmp_sim <- function(a, b, tag) {
  inner_join(select(a, year, variable, a = q0.5),
             select(b, year, variable, b = q0.5),
             by = c("year", "variable")) |>
    filter(year > 2025) |>
    group_by(variable) |>
    summarize(!!sym(tag) := max(abs(a - b)), .groups = "drop")
}

## 2. V0 : identité à n_sub = 1 ----
(v0 <- cmp_sim(sim3, simg1, "v0_max") |> arrange(desc(v0_max)))
cat("V0 (passerelle 1e-12) :", all(v0$v0_max < 1e-12), "\n")

## 3. V1 : écarts déterministes à noyaux neutres ----
# noyaux neutres : réponse pleine en janvier tenue constante, ECM mensuel des
# prix ; il ne reste que les écarts de discrétisation et l'effet boule de
# neige intra-annuel des intérêts
(v1 <- cmp_sim(sim3, simg12, "v1_max") |> arrange(desc(v1_max)))

## 4. identité dette ----
# les flux mensuels sont annualisés (ddette/n_sub) : la variation de dette sur
# l'année est exactement -solde(décembre) * Σvpib / n_sub. Le solde rapporté
# en ratio de sommes ferme l'identité ; une moyenne de ratios mensuels non.
pm12 <- calc_sim_dust(brg12$params$p, brg12$globals$model,
                      history = brg12$params$h, detail = "monthly")
pmm <- pm12 |>
  filter(year > 2025) |>
  select(year, month, variable, q0.5) |>
  pivot_wider(names_from = variable, values_from = q0.5) |>
  mutate(vpib = qpib * ppib, dette = dettep * vpib)
dec <- pmm |>
  filter(month == 12) |>
  transmute(year, dette_dec = dette, lag_dette = lag(dette))
ident <- pmm |>
  group_by(year) |>
  summarize(sum_vpib = sum(vpib), sep_dec = sep[month == 12],
            sep_mean = mean(sep), .groups = "drop") |>
  left_join(dec, by = "year") |>
  mutate(resid_dec = (dette_dec - lag_dette) + sep_dec * sum_vpib / 12,
         resid_mean = (dette_dec - lag_dette) + sep_mean * sum_vpib / 12)
cat("identité dette (ratio de sommes, décembre) : max |résidu| =",
    max(abs(ident$resid_dec), na.rm = TRUE),
    " ; moyenne mensuelle :", max(abs(ident$resid_mean), na.rm = TRUE), "\n")

## 5. détail mensuel : noyaux ----
# impulsion de -1 pp en 2027, règle coupée : seule la réponse des
# multiplicateurs varie (mkern doit sommer à 1)
imp <- rep(0, 75); imp[2] <- -0.01
mk_spread <- sqrt(1:12) / sum(sqrt(1:12))
pd <- purrr::imap_dfr(
  list("neutre (janvier)" = list(mkern_use = 0),
       "étalé (racine)" = list(mkern_use = 1, mkern = mk_spread)),
  ~ calc_sim_dust(
      list_modify(brg12$params$p, add_ib = imp, no_rule = 1, no_phi = 1, !!!.x),
      brg12$globals$model, history = brg12$params$h, detail = "monthly") |>
    mutate(kern = .y))
pa <- calc_sim_dust(list_modify(brs25$params$p, add_ib = imp, no_rule = 1, no_phi = 1),
                    brs25$globals$model, history = brs25$params$h)

ggplot() +
  geom_hline(yintercept = 0, linewidth = 0.2) +
  geom_line(data = filter(pd, variable == "og", between(year, 2026, 2031)),
            aes(x = year + (month - 0.5) / 12, y = q0.5, col = kern)) +
  geom_point(data = filter(pa, variable == "og", between(year, 2026, 2031)),
             aes(x = year, y = q0.5), shape = 21, fill = "white", size = 2, stroke = 0.5) +
  labs(title = "Impulsion de -1 pp en 2027 : og mensuel (dwrsm3g) vs points annuels (dwrsm3)",
       x = NULL, y = "og", col = "noyau")

# choc d'inflation de 2 pp en 2027 : vitesse de réaction des prix (pkern doit
# moyenner 1 : 1 = ECM mensuel non retouché)
imp_inf <- rep(0, 75); imp_inf[2] <- 0.02
pk_slow <- c(rep(0.2, 6), rep(1.8, 6))
pd_p <- purrr::imap_dfr(
  list("neutre (ECM mensuel)" = list(pkern_use = 0),
       "retardé (2e semestre)" = list(pkern_use = 1, pkern = pk_slow)),
  ~ calc_sim_dust(
      list_modify(brg12$params$p, add_inf = imp_inf, no_rule = 1, no_phi = 1, !!!.x),
      brg12$globals$model, history = brg12$params$h, detail = "monthly") |>
    mutate(kern = .y))
pa_p <- calc_sim_dust(list_modify(brs25$params$p, add_inf = imp_inf, no_rule = 1, no_phi = 1),
                      brs25$globals$model, history = brs25$params$h)

ggplot() +
  geom_hline(yintercept = 0, linewidth = 0.2) +
  geom_line(data = filter(pd_p, variable == "txppib", between(year, 2026, 2031)),
            aes(x = year + (month - 0.5) / 12, y = q0.5, col = kern)) +
  geom_point(data = filter(pa_p, variable == "txppib", between(year, 2026, 2031)),
             aes(x = year, y = q0.5), shape = 21, fill = "white", size = 2, stroke = 0.5) +
  labs(title = "Choc d'inflation de 2 pp en 2027 : txppib mensuel vs points annuels",
       x = NULL, y = "txppib", col = "noyau")

## 6. boucle fermée ----
# la règle des défauts (tpo_sstar = 0.5, tpo_dstar = -0.05, tpo_og = -0.25)
# est activée à partir de 2027 (optimal1). En boucle fermée les écarts se
# composent : la règle lit des états mensuels (lagog de décembre, sppsd courant)
# alors que dwrsm3 lit leurs homologues annuels ; lagspp, lui, est le solde
# annuel exact (engagé en décembre).
vo <- c(list(dstar = 1.2), ibs$optimal1)
s3_r <- run_model(brs25, out = "none", params = vo)
sg1_r <- run_model(brg1, out = "none", params = vo)
sg12_r <- run_model(brg12, out = "none", params = vo)
(rv0 <- cmp_sim(sg1_r, s3_r, "v0_regle") |> arrange(desc(v0_regle)))
(rv1 <- cmp_sim(sg12_r, s3_r, "v1_regle") |> arrange(desc(v1_regle)))

# flux production : phis optimisés sur dwrsm3 (cobyla), rejoués sur dwrsm3g
phis <- calc_phis(brs25, algorithm = "cobyla",
                  params = c(c(dstar = 1.2, loss_og = 5, loss_ib = 5, loss_ogpe = 0),
                             ibs$optimal1))
cli::cli_alert_info(phis$message)
p3 <- run_model(brs25, out = "none", params = phis)
p12 <- run_model(brg12, out = "none", params = phis)
cmp_sim(p3, p12, "phi_max") |> arrange(desc(phi_max)) |> slice_head(n = 8)

## 7. stochastique : distribution annuelle ----
# les facteurs k_ogn / k_inf (sub_noise_factors) calibrent le bruit mensuel
# pour que les agrégats aux frontières d'année gardent la variance de dwrsm3
rs25_mc <- list_modify(rs25, go_mc = TRUE, draws = 100,
                       ogn_sigma = 0.005, inf_sigma = 0.005)
brs_mc <- setup_params(params = rs25_mc, modele = "dwrsm3",
                       start_y = 2025, ameco_version = "5/2026")
brg12_mc <- setup_params(params = list_modify(rs25_mc, n_sub = 12L), modele = "dwrsm3g",
                         start_y = 2025, ameco_version = "5/2026")
s3mc <- calc_sim_dust(brs_mc$params$p, brs_mc$globals$model, history = brs_mc$params$h,
                      full_mc = TRUE)$full_mc
s12mc <- calc_sim_dust(brg12_mc$params$p, brg12_mc$globals$model,
                       history = brg12_mc$params$h, full_mc = TRUE)$full_mc
inner_join(
  s3mc |> group_by(year, variable) |> summarize(sd1 = sd(sim), .groups = "drop"),
  s12mc |> group_by(year, variable) |> summarize(sd12 = sd(sim), .groups = "drop"),
  by = c("year", "variable")) |>
  filter(variable %in% c("og", "txppib", "spp", "dettep"),
         year %in% c(2030, 2050, 2080)) |>
  mutate(ratio = sd12 / sd1)
