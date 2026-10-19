# on regroupe ici des fonctions pour utiliser le modèle en variante
#

# mets en route le modèle, le compile et
# calcule une règle pseudo optimale et génère un baseline
# params contient une liste de paramètres qui sont appliqués au baseline

setup_params <- function(
  country = "FRA",
  ameco_version = "5/2022",
  start_y = NULL,
  start_hist = 2007,
  end_year = 2100,
  seed = 42,
  adjust_dstar = TRUE,
  adjust_ec = TRUE,
  params = NULL,
  modele = "dwrstochasticmodel"
) {
  globals <- set_globals(
    country,
    version = "offapp",
    ameco_version = ameco_version,
    mongo = FALSE
  )

  if (is.null(start_y)) {
    start_y <- globals$years[[2]]
  }

  params <- list_modify(
    params,
    start_year = start_y,
    start_hist = start_hist,
    end_year = end_year,
    periods = end_year - start_y,
    seed = seed
  )
  modpath <- glue::glue("/odin/{modele}.r") |> pathify()
  suppressMessages(
    globals$model <- odin.dust::odin_dust(
      modpath,
      options = odin::odin_options(target = "c", verbose = FALSE)
    ),
    classes = c("message", "warning")
  )

  dandp <- dataandparams(
    country = country,
    start_year = start_y,
    globals = globals
  )
  p_def <- dandp$p_def

  if (adjust_dstar & is.null(params$dstar)) {
    e_dstar <- dandp$historical_data |>
      dplyr::filter(year == start_y) |>
      dplyr::pull(dettep)
    params <- purrr::list_modify(params, dstar = e_dstar, dsstar = e_dstar)
  }

  if (adjust_ec & is.null(params$ecstar)) {
    ec <- dandp$historical_data |>
      dplyr::filter(year == start_y) |>
      dplyr::pull(ec)
    params <- purrr::list_modify(params, ecstar = ec)
  }

  p_def <- purrr::list_modify(p_def, !!!params)

  pp <- set_params(p_def, globals, dandp)

  return(list(params = pp, globals = globals, dandp = dandp))
}

# on regroupe ici des fonctions pour utiliser le modèle en variante
#

# mets en route le modèle, le compile et
# calcule une règle pseudo optimale et génère un baseline
# params contient une liste de paramètres qui sont appliqués au baseline

setup_model <- function(
  country = "FRA",
  ameco_version = "5/2022",
  start_y = NULL,
  start_hist = 2007,
  end_year = 2100,
  seed = 42,
  adjust_dstar = TRUE,
  adjust_ec = TRUE,
  params = NULL,
  full_mc = FALSE,
  out = "graph",
  from = NULL,
  to = NULL,
  modele = "dwrstochasticmodel",
  vars = c("og", "dettep", "ib", "ci")
) {
  globals <- set_globals(
    country,
    version = "offapp",
    ameco_version = ameco_version,
    mongo = FALSE
  )
  if (is.null(start_y)) {
    start_y <- globals$years[[2]]
  }

  params <- list_modify(
    params,
    start_year = start_y,
    start_hist = start_hist,
    end_year = end_year,
    periods = end_year - start_y,
    seed = seed
  )
  modpath <- glue::glue("odin/{modele}.r")
  suppressMessages(
    globals$model <- odin.dust::odin_dust(
      modpath,
      options = odin::odin_options(target = "c", verbose = FALSE)
    ),
    classes = c("message", "warning")
  )

  dandp <- dataandparams(
    country = country,
    start_year = start_y,
    globals = globals
  )
  p_def <- dandp$p_def

  if (adjust_dstar & is.null(params$dstar)) {
    e_dstar <- dandp$historical_data |>
      dplyr::filter(year == start_y) |>
      dplyr::pull(dettep)
    params <- purrr::list_modify(params, dstar = e_dstar, dsstar = e_dstar)
  }

  if (adjust_ec & is.null(params$ecstar)) {
    ec <- dandp$historical_data |>
      dplyr::filter(year == start_y) |>
      dplyr::pull(ec)
    params <- purrr::list_modify(params, ecstar = ec)
  }

  p_def <- purrr::list_modify(p_def, !!!params)

  pp <- set_params(p_def, globals, dandp)
  bp <- calc_rule_params(globals, params = pp$p, draws = pp$p$draws)
  sims <- calc_sim_dust(bp, globals$model, history = pp$h, full_mc = full_mc)

  if (full_mc) {
    baseline <- sims$sim
  } else {
    baseline <- sims
  }

  baseline <- baseline |>
    mutate(scenario = "baseline")
  attr(baseline, "baseline") <- list(
    globals = globals,
    dandp = dandp,
    params = pp,
    rule = bp,
    scenario = params
  )
  .last.baseline <<- baseline

  if (out == 'graph') {
    if (is.null(from)) {
      from <- dandp$years[[2]] - 4
    }
    if (is.null(to)) {
      to <- dandp$years[[2]] + 10
    }

    print(graph_scenarios(
      baseline,
      from = from,
      to = to,
      start_y = dandp$start_year,
      vars = vars
    ))
  }
  if (out == 'table') {
    print(table_scenarios(baseline, bck = 2, fwd = 1))
  }
  if (full_mc) {
    return(list(sim = baseline, full_mc = sims$full_mc))
  }
  if (!full_mc) {
    return(baseline)
  }
}

run_model <- function(
  context,
  seed = 42,
  params = NULL,
  out = "graph",
  from = NULL,
  to = NULL,
  full_mc = FALSE,
  detail = c("annual", "monthly"),
  dec = 2,
  vars = c("og", "dettep", "ib", "spp")
) {
  detail <- match.arg(detail)
  # détail mensuel : données et graphique mensuels ; seule la table reste annuelle
  if (detail == "monthly" && out == "table")
    stop("detail = 'monthly' n'est pas compatible avec out = 'table' ",
         "(graph_scenarios est infra-annuel, table_scenarios reste annuel)")
  globals <- context$globals
  pp <- context$params
  p <- pp$p |> list_modify(!!!params)
  # bp <- calc_rule_params(globals, params=p, draws=pp$p$draws)
  sims <- calc_sim_dust(p, globals$model, history = pp$h, full_mc = full_mc, detail = detail)

  if (full_mc) {
    baseline <- sims$sim
  } else {
    baseline <- sims
  }

  baseline <- baseline |>
    mutate(scenario = "baseline")
  dandp <- context$dandp
  attr(baseline, "baseline") <- list(
    globals = globals,
    dandp = dandp,
    params = pp,
    # rule = bp,
    scenario = params
  )
  .last.baseline <<- baseline

  if (out == 'graph') {
    if (is.null(from)) {
      from <- dandp$years[[2]] - 4
    }
    if (is.null(to)) {
      to <- dandp$years[[2]] + 25
    }

    print(graph_scenarios(
      baseline,
      from = from,
      to = to,
      start_y = dandp$start_year,
      vars = vars
    ))
  }
  if (out == 'table') {
    print(table_scenarios(baseline, dec = dec))
  }
  if (full_mc) {
    return(list(sim = baseline, full_mc = sims$full_mc))
  }
  if (!full_mc) {
    return(baseline)
  }
}


# prend une liste (de listes) de paramètres et génère le dataframe correspondant
# suivant l'option calcule l'écart au baseline

run_parameters <- function(
  baseline = .last.baseline,
  params = NULL,
  rerule = TRUE,
  graph = TRUE
) {
  cases <- purrr::cross(params)
  if (length(cases) == 0) {
    return(NULL)
  }
  if (!rerule) {
    bp <- attr(baseline, 'baseline')$rule
  }
  sim <- furrr::future_map_dfr(
    cases,
    ~ {
      pp <- baseline$params$p
      pp <- purrr::list_modify(pp, !!!.x)
      globals <- baseline$globals
      suppressMessages(
        globals$model <- odin.dust::odin_dust(
          "odin/dwrstochasticmodel.r",
          options = odin::odin_options(target = "c", verbose = FALSE)
        ),
        classes = c("message", "warning")
      )
      p <- set_params(pp, globals, baseline$dandp)
      if (rerule) {
        ps <- calc_rule_params(globals, params = p$p, draws = p$p$draws)
      } else {
        ps <- purrr::list_modify(
          p$p,
          top_sstar = bp$tpo_sstar,
          tpo_dstar = bp$tpo_dstar,
          tpo_og = bp$tpo_og
        )
      }
      ss <- calc_sim_dust(ps, globals$model, history = p$h)
      bind_cols(as_tibble(.x), ss)
    },
    .options = furrr::furrr_options(seed = TRUE)
  )
  attr(sim, "baseline") <- baseline
  if (graph) {
    print(graph_simulations(sim))
  }
  invisible(sim)
}

run_scenarios <- function(
  baseline = .last.baseline,
  scenarios = NULL,
  modele = "dwrstochasticmodel",
  rerule = TRUE,
  phis = FALSE,
  graph = TRUE,
  diff = FALSE,
  vars = c("og", "dettep", "ib", "ci"),
  from = start_year(baseline) - 2,
  to = start_year(baseline) + 15,
  plotly = TRUE,
  workers = 4
) {
  if (!rerule) {
    bp <- attr(baseline, 'baseline')$rule
  }
  if (length(scenarios) <= 2) {
    future::plan("sequential")
  } else {
    future::plan(multisession, workers = workers)
  }
  mpath <- glue("odin/{modele}.r") |> pathify()
  sims <- furrr::future_imap(
    scenarios,
    ~ {
      pp <- attr(baseline, "baseline")$scenario
      pp <- purrr::list_modify(pp, !!!.x)
      globals <- attr(baseline, "baseline")$globals
      suppressMessages(
        globals$model <- odin.dust::odin_dust(
          mpath,
          options = odin::odin_options(target = "c", verbose = FALSE)
        ),
        classes = c("message", "warning")
      )
      p <- set_params(pp, globals, attr(baseline, "baseline")$dandp)
      if (rerule) {
        if (phis) {
          ps <- calc_phis(list(globals = globals, params = list(p = pp)), .x)
        } else {
          ps <- calc_rule_params(globals, params = p$p, draws = p$p$draws)
        }
      } else {
        ps <- purrr::list_modify(
          p$p,
          tpo_sstar = bp$tpo_sstar,
          tpo_dstar = bp$tpo_dstar,
          tpo_og = bp$tpo_og
        )
      }
      ss <- calc_sim_dust(ps, globals$model, history = p$h)
      list(p = ps, d = bind_cols(tibble(scenario = .y), ss))
    },
    .options = furrr::furrr_options(seed = TRUE)
  )
  sims <- purrr::transpose(sims)
  sim <- bind_rows(sims[["d"]])
  ps <- sims[["p"]]
  names(ps) <- names(scenarios)
  sim <- sim |>
    bind_rows(baseline)
  attr(sim, "baseline") <- attr(baseline, "baseline")
  attr(sim, "scenarios") <- scenarios
  attr(sim, "ps") <- ps
  if (graph) {
    plt <- graph_scenarios(
      sim,
      vars = vars,
      from = from,
      to = to,
      diff = diff,
      plotly = plotly
    )
    print(plt)
  }
  invisible(sim)
}

start_year <- function(sim) attr(sim, "baseline")[["dandp"]][["start_year"]]
draws <- function(sim) attr(sim, "baseline")[["dandp"]][["params"]][["draws"]]

bind_scenarios <- function(...) {
  scn <- list(...)
  bsl <- scn[[1]] |> filter(scenario == "baseline")
  ss <- purrr::map(scn, ~ filter(.x, scenario != "baseline"))
  ss <- dplyr::bind_rows(ss)
  ss <- dplyr::bind_rows(ss, bsl)
  attr(ss, 'baseline') <- attr(scn[[1]], 'baseline')
  attr(ss, 'scenario') <- purrr::reduce(
    scn,
    ~ list_modify(.x, attr(.y, 'scenario')),
    .init = list()
  )
  return(ss)
}

graph_scenarios <- function(
  sim,
  scenario = unique(sim$scenario),
  vars = c("og", "dettep", "ib", "spp"),
  start_y = NULL,
  from = start_year(sim) - 4,
  to = start_year(sim) + 10,
  diff = FALSE,
  plotly = TRUE
) {
  # pas infra-annuel : axe continu year + (month - 0.5)/n_sub, l'historique
  # annuel reste aux années entières (month manquant)
  mly <- "month" %in% names(sim)
  lsim <- sim |>
    dplyr::ungroup() |>
    dplyr::filter(variable %in% vars) |>
    dplyr::filter(between(year, from, to)) |>
    dplyr::mutate(
      variable = factor(variable, vars, ordered = TRUE),
      time = if (mly) {
        dplyr::if_else(is.na(month), year,
                       year + (month - 0.5) / max(month, na.rm = TRUE))
      } else {
        year
      }
    )

  if (diff) {
    lsim <- lsim |>
      group_by(time, variable) |>
      mutate(
        q0.025 = q0.025 - q0.5[scenario == "baseline"],
        q0.975 = q0.975 - q0.5[scenario == "baseline"],
        q0.5 = q0.5 - q0.5[scenario == "baseline"]
      ) |>
      ungroup() |>
      filter(scenario != "baseline")
  }

  if (is.null(start_y)) {
    start_y <- start_year(sim)
  }
  # couches simulation / historique : au pas mensuel l'historique se repère
  # par month manquant ; en annuel, frontière start_y (comportement inchangé)
  if (mly) {
    lay_sim <- ~ dplyr::filter(.x, !is.na(month))
    lay_hist <- ~ dplyr::filter(.x, is.na(month))
  } else {
    lay_sim <- ~ dplyr::filter(.x, year >= start_y)
    lay_hist <- ~ dplyr::filter(.x, year <= start_y)
  }
  if (draws(sim) > 1) {
    ribs <- list(ggplot2::geom_ribbon(
      data = lay_sim,
      aes(x = time, ymin = q0.025, ymax = q0.975, fill = scenario),
      alpha = 0.1
    ))
  } else {
    ribs <- NULL
  }

  labels <- attr(sim, 'baseline')$globals$vars |> as_tibble()
  strips_label <- (labels |> pull(label, name = ids))[vars]

  gg <- ggplot2::ggplot(lsim |> filter(scenario %in% !!scenario)) +
    ggplot2::geom_line(
      data = lay_sim,
      ggplot2::aes(x = time, y = mean, col = scenario)
    ) +
    ggplot2::geom_point(
      data = lay_sim,
      ggplot2::aes(x = time, y = mean, col = scenario),
      size = 1.25,
      fill = "white",
      shape = 21,
      stroke = 0.25
    ) +
    ggplot2::geom_line(
      data = lay_hist,
      ggplot2::aes(x = time, y = mean, col = scenario)
    ) +
    ggplot2::geom_point(
      data = lay_hist,
      ggplot2::aes(x = time, y = mean, col = scenario),
      size = 1
    ) +
    ribs +
    ggplot2::facet_wrap(
      ggplot2::vars(variable),
      scales = "free_y",
      labeller = labeller(variable = strips_label)
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(0.1)) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
    ofce::theme_ofce(base_family = "Open Sans") +
    labs(color = NULL, fill = NULL) +
    theme(legend.position = "bottom")
  if (plotly) {
    plotly::ggplotly(gg) |>
      plotly::layout(hovermode = "x unified", showlegend = FALSE) |>
      plotly::style(hovertemplate = "%{y:.2%}",
                    xhoverformat = if (mly) "%{x:.2f}" else "%{x:.0f}")
  } else {
    gg
  }
}

graph_simulations <- function(
  sim,
  vars = c("og", "dettep", "ib", "spp"),
  plotly = TRUE
) {
  cases <- setdiff(
    names(sim),
    c("q0.025", "q0.5", "q0.975", "variable", "year")
  )

  cases_named <- dplyr::distinct(sim, across(any_of(cases))) |>
    dplyr::mutate(
      v = do.call(
        function(...) str_c(..., sep = "; "),
        dplyr::across(
          dplyr::everything(),
          ~ stringr::str_c(dplyr::cur_column(), ":", .x)
        )
      )
    )

  sim <- sim |>
    dplyr::filter(variable %in% vars) |>
    dplyr::left_join(cases_named, by = cases)
  start_y <- start_year(sim)
  gg <- ggplot2::ggplot(sim) +
    ggplot2::geom_line(
      data = ~ dplyr::filter(.x, year >= start_y),
      ggplot2::aes(x = year, y = q0.5, col = v, group = v),
      linetype = "dotted"
    ) +
    ggplot2::geom_line(
      data = ~ dplyr::filter(.x, year <= start_y),
      ggplot2::aes(x = year, y = q0.5, col = v, group = v)
    ) +
    ggplot2::facet_wrap(ggplot2::vars(variable), scales = "free_y") +
    ofce::theme_ofce(base_family = "Nunito")
  if (plotly) {
    plotly::ggplotly(gg) |>
      plotly::layout(hovermode = "x unified") |>
      plotly::style(hovertemplate = "%{y:.2%}", xhoverformat = "%{x:.0f}")
  } else {
    gg
  }
}

meantx <- function(vec) {
  cc <- cumprod(vec + 1)
  (tail(cc, 1))^(1 / length(cc)) - 1
}

table_scenarios <- function(
  sim,
  scenario = NULL,
  years = NULL,
  bck = 1,
  fwd = 5,
  note_d = "",
  note_f = "",
  note_s = "",
  dec = 1
) {
  if (is.null(years)) {
    year <- start_year(sim)
  }
  debut <- year
  mscn <- if (is.null(scenario)) {
    unique(sim$scenario)
  } else {
    scenario
  }
  if (length(mscn) == 1) {
    table_simulation(
      sim,
      debut = debut,
      bck = bck,
      fwd = fwd,
      note_d = note_d,
      note_s = note_s,
      note_f = note_f,
      dec = dec
    )
  } else {
    map(
      mscn,
      ~ table_simulation(
        sim,
        scenario = .x,
        debut = debut,
        bck = bck,
        fwd = fwd,
        note_d = note_d,
        note_s = note_s,
        note_f = note_f,
        dec = dec
      )
    )
  }
}

table_simulation <- function(
  sim,
  scenario = unique(sim$scenario)[[1]],
  debut,
  bck = 1,
  fwd = 5,
  note_s = "",
  note_d = "",
  note_f = "",
  dec = 1
) {
  fin <- debut + fwd + 1
  p1.d <- debut - 4
  
  # valeurs affichées : trajectoire déterministe (deter) si présente,
  # sinon moyenne des tirages (simulations calculées avant la colonne deter)
  val <- if ("deter" %in% names(sim)) "deter" else "mean"
  sim1 <- sim |>
    filter(
      variable %in%
        c(
          "qpib",
          "og",
          "gpot",
          "txppib",
          "pibpot",
          "tcho",
          "r_app",
          "r_inst",
          "spp",
          "ib",
          "ib_po",
          "ib_dep",
          "ci",
          "ssp",
          "sep",
          "tpo",
          "tdeppp",
          "dettep"
        ),
      scenario == !!scenario
    ) |>
    pivot_wider(id_cols = year, names_from = variable, values_from = !!val) |>
    arrange(year) |>
    mutate(
      gpib = qpib / lag(qpib) - 1,
      gppib = txppib,
      gpibpot = pibpot / lag(pibpot) - 1,
      gpibpot = if_else(is.na(gpibpot), gpot, gpibpot),
      sspp = spp - tpo * og,
      ibsna = replace_na(ib, 0),
      ibc = cumsum(ibsna * (year > debut)),
      ibc = ifelse(year <= debut, NA, ibc),
      ogc = cumsum(og * (year > debut) )/cumsum((year>debut)),
      ogc = ifelse(year <= debut, NA, ogc),
      ci_ni = ci - gppib * dettep,
      bdn = (r_app - gpib - gppib)/(1 + gpib + gppib) * dettep
    ) |>
    select(-ibsna) |>
    ungroup() |>
    pivot_longer(cols = -c(year), names_to = "variable", values_to = "value")
  # an_moy <- sim1 |>
  #   mutate(ygrp = case_when(
  #     between(year, debut+1, debut+6) ~ str_c(debut+1, "-", debut+6),
  #     between(year, debut+7, debut+12) ~ str_c(debut+7, "-", debut+12),
  #     between(year, debut+13, debut+18) ~ str_c(debut+13, "-", debut+18),
  #     between(year, debut+19, debut+24) ~ str_c(debut+19, "-", debut+24),
  #     TRUE ~ "out")) |>
  #   filter(ygrp != "out") |>
  #   group_by(ygrp, variable) |>
  #   summarize(q0.5 = if_else(
  #     str_detect(first(variable), "^g"),
  #     meantx(q0.5),
  #     mean(q0.5)),
  #             .groups = "drop")
  an_moy <- sim1 |>
    filter(year == 2050) |>
    rename(ygrp = year) |>
    mutate(ygrp = "LT (2050)")
  max <- sim1 |>
    filter(
      variable %in% c("dettep", "sspp", "sep", "ibc", "spp", "tcho", "og", "ci") ) |>
    filter(year |> between(debut + 1, fin + 1 + 25)) |>
    mutate(value = ifelse(variable %in% c("og", "ibc"), -value, value)) |>
    group_by(variable) |>
    summarize(value = max(value)) |>
    mutate(value = ifelse(variable %in% c("og", "ibc"), -value, value)) |>
    mutate(year = "Max. 25-50")

  pos <- tribble(
    ~variable , ~label                             , ~note                                     , ~max  ,
    "gpib"    , "Croissance du PIB"                , "évolution en % annuel, volume"           , ""    ,
    "og"      , "Ecart de production"              , "% du PIB"                                , "min" ,
    "ogc"     , "Ecart de production cumulé moyen" , "% du PIB"                                , "min" ,
    "gpot"    , "Croissance du PIB potentiel"      , "évolution en % annuel, volume"           , ""    ,
    "gppib"   , "Inflation"                        , "déflateur du PIB, évolution en % annuel" , ""    ,
    "tcho"    , "Taux de chômage"                  , "% de la population active"               , ""    ,
    "sep"     , "Solde public"                     , "% du PIB"                                , ""    ,
    "spp"     , "Solde public primaire"            , "% du PIB"                                , ""    ,
    "sspp"    , "Solde public structurel primaire" , "% du PIB"                                , ""    ,
    "ib"      , "Impulsion budgétaire primaire"    , "% du PIB"                                , ""    ,
    "ib_po"   , "\u202F\u202F\u202Fdont PO"        , "% du PIB"                                , ""    ,
    "ib_dep"  , "\u202F\u202f\u202fdont dépense"   , "% du PIB"                                , ""    ,
    "ibc"     , "Cumul de l'impulsion primaire"    , "% du PIB"                                , "min" ,
    "r_inst"  , "Taux souverain courant"           , "10 ans, % annuel"                        , ""    ,
    "r_app"   , "Taux souverain pondéré"           , "% annuel"                                , ""    ,
    "dettep"  , "Dette publique"                   , "% du PIB"                                , ""    ,
    "ci"      , "Charge d'intérêts"                , "% du PIB"                                , ""    ,
    "ci_ni"   , "Ch. int. nette de l'inflation"    , "% du PIB"                                , ""    ,
    "bdn"     , "Effet boule de neige"             , "% du PIB"                                , "" )  |> 
    mutate(pos = 1:n())
  
  an_tot <- sim1 |>
    filter(year %in% (debut - bck):(fin)) |>
    mutate(year = as.character(year)) |>
    bind_rows(an_moy |> rename(year = ygrp)) |>
    bind_rows(max) |>
    pivot_wider(names_from = year, values_from = value) |>
    left_join(pos, by = "variable") |>
    filter(!is.na(pos)) |>
    arrange(pos) |>
    select(-pos, -variable) |>
    relocate(label)
  atrbts <- attr(sim, "baseline")
  if (!is.null(atrbts)) {
    mc <- atrbts$params$p$go_mc
    pays <- atrbts$globals$pays |>
      filter(id == atrbts$globals$country) |>
      pull(nom_fr)
    # note : dès que deter est présente, les valeurs affichées sont la
    # trajectoire déterministe, même si la simulation est stochastique
    val_txt <- if (val == "deter")
      " tirages), trajectoire déterministe (sans bruit)"
    else
      " tirages), valeurs moyennes"
    simulation <- glue(
      "{if_else(mc, str_c('stochastique (', atrbts$params$p$draws, val_txt), 'déterministe')}"
    )
    year1 <- atrbts$params$start_year + 1
  } else {
    pays <- ""
    mc <- 1
    simulation <- "?"
    year1 <- "?"
  }
  an_tot |>
    gt() |>
    fmt_number(
      columns = where(is.numeric),
      decimals = dec,
      scale_by = 100,
      sep_mark = " ",
      dec_mark = ","
    ) |>
    tab_style(
      style = cell_fill(color = "gray95"),
      locations = cells_body(columns = -(1:(bck + 2)))
    ) |>
    tab_style(
      style = cell_fill(color = "gray92"),
      locations = cells_body(columns = -(1:(bck + 2 + fwd)))
    ) |>
    tab_style(style = cell_borders('top'), cells_body(rows = c(4, 7, 10, 13, 14, 16, 17))) |>
    cols_label(label = "") |>
    cols_hide(c(note, max, `Max. 25-50`)) |>
    tab_footnote(
      "évolution en % annuel, volume",
      cells_body(label, note == "évolution en % annuel, volume")
    ) |>
    tab_footnote(
      "évolution en % annuel",
      cells_body(label, note == "évolution en % annuel")
    ) |>
    tab_footnote("% du PIB", cells_body(label, note == "% du PIB")) |>
    tab_footnote(
      "déflateur du PIB, évolution en % annuel",
      cells_body(label, note == "déflateur du PIB, évolution en % annuel")
    ) |>
    tab_footnote(
      "% de la population active",
      cells_body(label, note == "% de la population active")
    ) |>
    tab_footnote(
      "% du PIB potentiel",
      cells_body(label, note == "% du PIB potentiel")
    ) |>
    tab_footnote(
      "10 ans, % annuel",
      cells_body(label, note == "10 ans, % annuel")
    ) |>
    tab_footnote("% annuel", cells_body(label, note == "% annuel")) |>
    tab_footnote("Minimum", cells_body(`Max. 25-50`, max == "min")) |>
    tab_source_note(
      "Données {atrbts$globals$ameco_nom}{note_d}, première année de simulation : {year1}{note_s}" |>
        glue::glue()
    ) |>
    tab_source_note(
      "Simulation debtwatch v2.0 (github.com/OFCE/dwr), {pays}, {simulation}" |>
        glue::glue()
    ) |>
    tab_source_note("{note_f}" |> glue::glue()) |>
    ofce::ofce_tab_options() |>
    sub_missing() |>
    cols_align_decimal(-label) |>
    ofce::ofce_tab_options() |> 
    cols_width(
      label ~ px(220),
      `LT (2050)` ~ px(55),
      everything() ~ px(45)
    )
}

get_p <- function(sim, p = NULL) {
  ps <- attr(sim, "ps")
  if (is.null(p)) {
    return(ps)
  }
  return(purrr::transpose(ps) |> pluck(p))
}

table_diff <- function(
  sim,
  vars = NULL,
  from = start_year(sim),
  to = start_year(sim) + 5,
  diff = "",
  note_d = ", calage sur INSEE comtes annuels 2021 (juin 2022)",
  note_f = ""
) {
  if (is.null(vars)) {
    vars <- unique(sim$variable)
  }
  # valeurs affichées : trajectoire déterministe (deter) si présente,
  # sinon médiane des tirages (simulations calculées avant la colonne deter)
  val <- if ("deter" %in% names(sim)) "deter" else "q0.5"
  val_txt <- if (val == "deter")
    " tirages), trajectoire déterministe (sans bruit)"
  else
    " tirages, valeurs médiannes"
  atrbts <- attr(sim, "baseline")
  mc <- atrbts$params$p$go_mc
  pays <- atrbts$globals$pays |>
    filter(id == atrbts$globals$country) |>
    pull(nom_fr)
  if (is.null(names(vars))) {
    names(vars) <- vars
  }
  siml <- sim |>
    filter(between(year, from, to)) |>
    filter(variable %in% vars) |>
    mutate(
      variable = factor(
        variable,
        levels = vars,
        labels = names(vars),
        ordered = TRUE
      )
    )
  note_s <- ""
  if (diff != "") {
    note_s <- ", différence avec {diff}" |> glue::glue()
    siml <- siml |>
      group_by(variable) |>
      mutate("{val}" := .data[[val]] - .data[[val]][scenario == !!diff]) |>
      filter(scenario != !!diff)
  }

  simt <- siml |>
    group_by(variable, scenario) |>
    summarise(
      "{val}" := sum(.data[[val]]),
      year = str_c(min(year), "-", max(year)),
      .groups = "drop"
    )

  siml <- siml |>
    mutate(year = as.character(year)) |>
    bind_rows(simt) |>
    pivot_wider(
      id_cols = c(variable, scenario),
      values_from = !!val,
      names_from = year
    )

  siml |>
    group_by(variable) |>
    arrange(variable) |>
    gt() |>
    cols_label(scenario = "") |>
    fmt_number(columns = where(is.numeric), decimals = 1, scale_by = 100) |>
    tab_options(
      column_labels.padding.horizontal = px(5),
      data_row.padding = px(1),
      table.font.size = 12,
      source_notes.padding = px(1)
    ) |>
    tab_source_note(
      "Données {atrbts$globals$ameco_nom}{note_d}" |> glue::glue()
    ) |>
    tab_source_note(
      "Simulations debtwatch v2.0 (github.com/OFCE/dwr), {pays}" |> glue::glue()
    ) |>
    tab_source_note(
      "Simulations 
                    {if_else(mc, str_c('stochastique (', atrbts$params$p$draws, val_txt), 'déterministe')}" |>
        glue::glue()
    ) |>
    tab_source_note(
      "Première année de simulation : {atrbts$params$start_year+1}{note_s}" |>
        glue::glue()
    ) |>
    tab_source_note("{note_f}" |> glue::glue()) |>
    ofce::table_ofce()
}
