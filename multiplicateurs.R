source("simulations init.R")

phis_norule <- calc_phis(
  brs25,
  algorithm = "cobyla",
  params = list_modify(
    ibs[["optimal1"]],
    dstar = 1.2, loss_og = 5, loss_ib = 5, loss_ogpe = 0, no_rule = c(1) , no_phi = c(1,1,0)) )

phis_rule <- calc_phis(
  brs25,
  algorithm = "cobyla",
  params = list_modify(
    ibs[["optimal1"]],
    dstar = 1.2, loss_og = 5, loss_ib = 5, loss_ogpe = 0, no_rule = c(1,1,0) , no_phi = c(1,1,0)) )

cc_norule <- run_model(brs25, out = "none", params = phis_norule, full_mc = FALSE) |> mutate(rule = "Pas de règle")
cc_rule <- run_model(brs25, out = "none", params = phis_rule, full_mc = FALSE) |> mutate(rule = "Règle linéaire")
cc_rule_opt <- run_model(brs25, out = "none", params = phis_rule, full_mc = FALSE) |> mutate(rule = "Règle optimale")

cc <- bind_rows(cc_norule, cc_rule, cc_rule_opt)

# ggplot(cc |> filter(variable=="dettep", between(year, 2025,2050)))+geom_line(aes(x=year, y=q0.5, color = rule))

ib <- list(
  add_ib = c(-0.004+0.01, -0.007-0.01, -0.0, 0))

ps_norule <- list_modify(phis_norule, !!!ib)
var_norule <- run_model(brs25, out = "non", params = ps_norule, full_mc = FALSE) |>
  mutate(rule = "Pas de règle")

ps_rule <- list_modify(phis_rule, !!!ib)
var_rule <- run_model(brs25, out = "non", params = ps_rule, full_mc = FALSE) |>
  mutate(rule = "Règle linéaire")

ps_rule_opt <- calc_phis(
  brs25,
  algorithm = "cobyla",
  params = list_modify(
    ibs[["optimal1"]],
    !!!ib,
    dstar = 1.2, loss_og = 5, loss_ib = 5, loss_ogpe = 0, no_rule = c(1,1,0) , no_phi = c(1,1,0)) )
var_rule_opt <- run_model(brs25, out = "non", params = ps_rule_opt, full_mc = FALSE) |>
  mutate(rule = "Règle optimale")

var <- bind_rows(var_norule, var_rule, var_rule_opt)

res <- cc |>
  left_join(var, join_by(variable, year, rule), suffix = c("", ".ib")) |>
  mutate(
    dvar = mean.ib - mean,
    dlvar = mean.ib/mean - 1) |>
  select(rule, variable, year, dvar) |>
  pivot_wider(names_from = variable, values_from = dvar) |>
  group_by(rule) |>
  mutate(ogc = cumsum(og)) |>
  ungroup() |>
  pivot_longer(c(-year, -rule), names_to = "variable", values_to = "dvar")

return(res)

ggplot(res |> filter(variable%in%c("dettep", "og", "ogc", "ssp"), year<=2050))+
  geom_line(aes(x=year, y=dvar)) +
  facet_grid(cols = vars(variable), rows = vars(rule), scales = "free_y")+
  xlim(2020, 2050)

