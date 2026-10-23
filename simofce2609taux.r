#1
source("simulations init.R")
## principales variantes, cible de dette --------------------------
ccl <- pmap_dfr(
  expand_grid(
    .h = 2032,
    .r  =   seq(-0.01, 0.02, .001),
    .k = c(0.8, 1.2, 1.6),
    .p = c(0.4)),
  \(.h, .r, .k, .p) {
    
    pp <- list_modify(ibs[["optimal1"]],
                      dstar = 1.2, loss_og = 5, loss_ib = 5, loss_ogpe = 0,
                      cons_d = 27 - (.h - 2025), cons_dt = .h - 2025,
                      add_rinst = c(0.015, .r, .r, rep(.r, 25)),
                      og_dep = .k, og_mce = .p    )
    phis <- calc_phis(
      brs25,
      algorithm = "cobyla",
      params = pp )
    run_model(brs25, out = "none", params = phis, full_mc = FALSE) |>
      mutate(h = .h, r = .r, p = .p, k = .k)
  },
  .progress = TRUE ) 

summary <- ccl |> 
  filter(variable == "cib") |> 
  select(year, deter, q0.5, q0.025, q0.975, h, p, k, r) |> 
  filter(year == h) 

return(list(sims = ccl, summary = summary))


# table_scenarios(cc |> filter(ibs == "optimal1"), dec = 2)
# graph_scenarios(
#   cc |> filter(ibs == "optimal1"),
#   to = 2050,
#   vars = c("og", "dettep", "ib", "spp")
# )
# 
# qs2::qs_save(cc, pathify("/ajust/cae26/dwcccae26.qs2"))

data <- ccl |> 
  filter(
    between(year, 2025, 2050), 
    variable %in% c("og", "ib", "dettep", "sep")) |> 
  mutate(
    h = as.character(h),
    r = as.character(round(r*100,1)),
    tooltip = r)

data |> 
  select(variable, year, mean, h, p, r, tooltip) |> 
  pivot_wider(names_from = variable, values_from = mean) |> 
  group_by(h, p, r) |> 
  summarize(
    effort = sum(ib[between(year, 2026, as.integer(h))])) |> 
  ggplot()+
  aes(x = r, color = p, y = -effort , group = p) +
  scale_y_continuous(labels = scales::label_percent(.1)) +
  geom_line()
  
