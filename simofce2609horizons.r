#1
source("simulations init.R")
future::plan("multisession", workers = workers)

## principales variantes, cible de dette --------------------------
ccl <- pmap_dfr(
  expand_grid(
    .h  =   c(2030, 2031, 2032, 2033, 2034, 2035, 2036, 2037),
    .mult = c(0.8, 1, 1.2) ,
    .pers = c(0.4, 0.3, 0.2) ),
  \(.h, .mult, .pers) {
    source("simulations init.R")

    pp <- list_modify(ibs[["optimal1"]],
                      dstar = 1.2, loss_og = 5, loss_ib = 5, loss_ogpe = 0,
                      cons_d = 27 - (.h - 2025), cons_dt = .h - 2025,
                      og_dep = .mult,
                      og_mce = .pers
    )
    phis <- calc_phis(
      brs25,
      algorithm = "cobyla",
      params = pp )
    run_model(brs25, out = "none", params = phis, full_mc = FALSE) |>
      mutate(h = .h, k = .mult, p = .pers)
  },
  .progress = TRUE ) |>
  futurize()

summary <- ccl |>
  filter(variable == "cib") |>
  select(year, deter, q0.5, q0.025, q0.975, h, p, k) |>
  filter(year == h+1)

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
    tooltip = h)
gg <- ggplot(data |> filter(p == 0.4))+
  theme_ofce(base_size = 10)+
  theme(
    legend.position = c(1,0),
    legend.justification = c(1,0)  )+
  PrettyCols::scale_color_pretty_d("Summer") +
  PrettyCols::scale_fill_pretty_d("Summer") +
  geom_ribbon(
    aes(x=year, ymin=q0.025, ymax=q0.975, fill = h), alpha = 0.05) +
  geom_line(
    aes(x = year, y = q0.5, color = h), linewidth = 0.25) +
  geom_point_interactive(
    aes(x = year, y = mean, fill = h, shape = nat, size = nat,
        tooltip = tooltip, data_id = year),
    color="white", size = 1., shape = 21, stroke=0.25, hover_nearest = TRUE)+
  scale_size_manual(values= c("obs" = 2, "pmst" = 1.5, "rule" = 1)) +
  scale_shape_manual(values= c("obs" = 21, "pmst" = 23, "rule" = 23)) +
  facet_wrap(ggplot2::vars(variable), scales = "free_y", labeller = labeller(variable= vlabels)) +
  scale_y_continuous(labels = scales::label_percent(1))+
  scale_x_continuous(
    breaks = scales::pretty_breaks(),
    labels = date1,
    minor_breaks = scales::breaks_width(1)) +
  xlab(NULL)+ylab(NULL)+
  guides(shape = "none", size = "none", fill="none",
         x = guide_axis(minor.ticks = TRUE))+
  labs(color = NULL, fill=NULL) +
  ofce_caption(
    source = "Debtwatch v2.0, AMECO, prévision d'automne 2026 de l'OFCE",
    note = "Les impulsions sont celle de l'OFCE, prévision d'automne 2026, pour 2026 et 2027")

girafy(gg, r=1.5)

summary |>
  ggplot()+
  aes(x = h, color = p, y = -effort , group = p) +
  geom_line() +
  facet_wrap(vars(k)) +
  scale_y_continuous(
    labels = scales::label_percent(),
    sec.axis = ~.x*3050
  )

