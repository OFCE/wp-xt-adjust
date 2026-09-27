source("simulations init.R")

phis <- calc_phis(
  brs25,
  algorithm = "cobyla",
  params = list_modify(
    ibs[["optimal1"]], 
    dstar = 1.2, loss_og = 5, loss_ib = 5, loss_ogpe = 0, no_rule = c(1,1,0) , no_phi = c(1,1,0)) )

cc <- run_model(brs25, out = "none", params = phis, full_mc = FALSE) 
ggplot(cc |> filter(variable=="dettep", between(year, 2025,2050)))+geom_line(aes(x=year, y=q0.5))

ib2 = list(
  add_ib = c(-0.004+0.01, -0.007, -0.0, 0),
  psi = phis$phi)

ps <- list_modify(phis, !!!ib2)
cb <- run_model(brs25, out = "non", params = ps, full_mc = FALSE) 

cb2 <- cc |> 
  left_join(cb, join_by(variable, year), suffix = c("", ".ib")) |> 
  mutate(
    dvar = mean.ib - mean,
    dlvar = mean.ib/mean - 1) |> 
  select(variable, year, dvar) |> 
  pivot_wider(names_from = variable, values_from = dvar) |> 
  mutate(ogc = cumsum(og)) |> 
  pivot_longer(c(-year), names_to = "variable", values_to = "dvar")

ggplot(cb2 |> filter(variable%in%c("dettep", "og", "ogc", "ssp"), year<=2050))+
  geom_line(aes(x=year, y=dvar)) +
  facet_wrap(vars(variable), scales = "free_y")+
  xlim(2020, 2035)
  