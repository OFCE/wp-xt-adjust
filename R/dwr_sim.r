forme_history <- function(data, probs = c(0.025, 0.5, 0.975)) {
  fh <- data |> dplyr::transmute(
    year = year,
    og = og, dettep,
    txppib, tpo,
    tdep=tdeppp*pibpot/qpib,
    spp,
    ci = r_app*dettep,
    sep = spp - ci,
    ssp = sep - tpo * og / (1 + og),
    ib_po = - tpo + lag(tpo),
    tdeppp,
    ib_dep = tdeppp - dplyr::lag(tdeppp), 
    ib = ib_po + ib_dep,
    qpib, pibpot, tcho, tvnairu, gpib, r_app, r_inst) |>
    tidyr::pivot_longer(cols = - year, names_to = "variable", values_to = "q0.5")
  se <- stringr::str_c("q", setdiff(probs, 0.5))
  dplyr::bind_cols(fh, dplyr::bind_cols(purrr::map(se, ~tibble::tibble(!!.x := fh$q0.5)))) |> 
    dplyr::arrange(year)
}

dataandparams <- function(country, start_year, periods = 2050-start_year, draws=100, globals, ameco=NULL) {
  if(is.null(ameco))
    ameco <- globals$ameco
  country_list <- ameco |> dplyr::distinct(country)
  
  countries <- set_names(globals$pays |> dplyr::pull(id))
  countries_ln_fr <- globals$pays |> dplyr::pull(nom_fr, name = id)
  
  data <- ameco |> 
    dplyr::filter(country==!!country, unit==0) |>
    dplyr::rename(var = ccode) |> 
    dplyr::select(-code, -TITLE, -COUNTRY, -`SUB-CHAPTER`, -UNIT, -csplit2, -csplit3, -unit, -is.prev, -country) |> 
    tidyr::drop_na(value) |>  
    tidyr::pivot_wider(names_from = var, values_from = value) 
  if(!"tvnairu" %in% names(data)) 
    data <- data |>
    mutate(tvnairu = slider::slide_dbl(tcho, ~mean(.x), .before = 3 ))
  if(!"pibpot" %in% names(data)) 
    data <- data |>
    mutate(pibpot = slider::slide_dbl(qpib, ~mean(.x), .before = 10),
           og = qpib/pibpot-1)
  data <- data |> 
    dplyr::mutate(dplyr::across(.cols=c(irl, irs, tvnairu), .fns = ~ dplyr::if_else(is.na(.x), .x[year==max(year[!is.na(.x)])], .x))) |> 
    dplyr::mutate(
      ppib = vpib/qpib,
      gpot = pibpot/dplyr::lag(pibpot) - 1,
      txppib = ppib/dplyr::lag(ppib) - 1, 
      gpib = qpib/dplyr::lag(qpib) - 1, 
      og = qpib/pibpot-1,
      r_app = ci/dplyr::lag(dette),
      r_inst = irl/100,
      vpibpot = ppib*pibpot,
      tpo = rec_po/vpib,
      tpopp = rec_po/vpibpot,
      tdep = dep_prim/vpib,
      tdeppp = dep_prim/vpibpot,
      cip = ci/vpib,
      soldep=solde/vpib,
      spp=sprim/vpib,
      taut = spp - tpo + tdep,
      tcho = tcho/100,
      tvnairu = tvnairu/100,
      ec = irl/100 - ((ppib/dplyr::lag(ppib,3))^0.33 - 1)- ((pibpot/dplyr::lag(pibpot,3))^0.33 - 1) ) |> 
    tibble::as_tibble()
  
  full_historical_data <- data |> 
    dplyr::transmute(year,
                     dettep = dette/vpib, r_app, r_inst,
                     spp, og, txppib, tpo, tdep, tdeppp, pibpot, qpib, tcho, tvnairu, gpib, ec) |> 
    tidyr::drop_na(dettep)
  
  historical_data <- full_historical_data |> 
    dplyr::filter(year<=start_year) 
  
  years <- c(min(historical_data$year), max(data$year))
  
  init <- data |> 
    dplyr::arrange(year) |> 
    dplyr::mutate(lagog=dplyr::lag(og),
                  lag2og=dplyr::lag(og),
                  lagtcho=dplyr::lag(tcho),
                  lag2tcho=dplyr::lag(lagtcho),
                  lagtvnairu=dplyr::lag(tvnairu),
                  lagppib=dplyr::lag(ppib),
                  lagpibpot=dplyr::lag(pibpot),
                  lag2pibpot=dplyr::lag(lagpibpot),
                  lag2ppib=dplyr::lag(lagppib),
                  lagqpib = dplyr::lag(qpib),
                  lag2qpib = dplyr::lag(lagqpib),
                  lagtdeppp = dplyr::lag(tdeppp),
                  lag2tdeppp = dplyr::lag(lagtdeppp),
                  lagtpo =dplyr::lag(tpo),
                  lagci = dplyr::lag(ci),
                  lagr_inst = dplyr::lag(r_inst),
                  lagdette = dplyr::lag(dette),
                  lag2dette = dplyr::lag(lagdette),
                  lagdettep = dplyr::lag(dette/vpib),
                  lagspp = dplyr::lag(spp),
                  lag2spp = dplyr::lag(lagspp),
                  lagcip = dplyr::lag(cip),
                  lag2cip = dplyr::lag(lagcip)) |> 
    dplyr::filter(year==start_year)
  params <- list(
    periods = periods,
    draws = as.numeric(draws),  
    infstar = 0.0175,
    nairu = 0.07,
    dstar = init$dette/init$vpib
  )
  
  p_def <- globals$defaults |>
    dplyr::filter(type=="p") |>
    dplyr::select(!!country, id) |> 
    dplyr::mutate(!!country:=as.numeric(.data[[country]])) |>
    dplyr::pull(!!country, name=id) |> 
    as.list()
  
  dstar <- historical_data |>
    dplyr::filter(year==min(start_year, max(year)-3)) |> 
    dplyr::pull(dettep)
  
  if(is.null(p_def[["dstar"]]))
    p_def$dstar <- dstar
  
  nairu <- historical_data |>
    dplyr::filter(year==min(start_year, max(year))) |> 
    dplyr::pull(tcho)
  
  if(is.null(p_def[["nairu"]]))
    p_def$nairu <- nairu
  
  dci <- globals$defaults |>
    dplyr::filter(type==start_year) |> 
    dplyr::select(id, !!country) |> 
    dplyr::mutate(!!country:=as.numeric(.data[[country]])) |>
    dplyr::pull(name = id) |> 
    as.list()
  
  zn <- function(x) ifelse(is.null(x), 0, x)
  p_init <- list(
    i_lagog = init$og,
    i_lag2og = init$lagog,
    i_lagtcho = init$tcho,
    i_lagtvnairu = init$tvnairu,
    i_lagppib = init$ppib,
    i_lag2ppib = init$lagppib,
    i_lag3ppib = init$lag2ppib,
    i_lagqpib = init$qpib,
    i_lag2qpib = init$lagqpib,
    i_lag3qpib = init$lag2qpib,
    i_lagpibpot = init$pibpot,
    i_lag2pibpot = init$lagpibpot,
    i_lag2dette = init$lagdette,
    i_lagtpo = init$tpo + zn(dci$i_tpo),
    i_lagtdeppp = init$tdeppp + zn(dci$i_tdeppp),
    i_lag2tdeppp = init$lagtdeppp,
    i_lagr_app = init$r_app + zn(dci$i_r_app),
    i_lagr_inst = init$r_inst,
    i_lagdette = init$dette,
    i_lagspp = init$spp + zn(dci$i_tpo) - zn(dci$i_tdeppp)/(1+init$og),
    i_lag2spp = init$lagspp,
    i_lagcip = init$cip  + zn(dci$i_r_app)*init$dette,
    i_lag2cip = init$lagcip,
    i_lagecart_c = init$ec,
    dette_oneoff = rep(0, periods),
    tx_deriv_dep = rep(0, periods),
    tx_deriv_po = rep(0, periods),
    gpot = rep(init$gpot, periods),
    taut = rep(init$taut, periods),
    add_og = rep(0, periods),
    add_ib = rep(0, periods),
    add_og_e = rep(0, periods),
    add_tpo = rep(0, periods),
    add_tdeppp = rep(0, periods),
    no_rule = rep(0, periods)
  )
  return(list(countries = countries, 
              countries_ln_fr = countries_ln_fr,
              params = params,
              p_init = p_init, 
              p_def = p_def,
              historical_data = historical_data, 
              full_historical_data = full_historical_data, 
              init = init,
              years = years,
              country=country,
              start_year=start_year, 
              periods=periods))
}

.dwr.long <- c("dette_oneoff", "phi", "no_phi", "gpot", "taut", "tx_deriv_dep", 
               "tx_deriv_po", "add_og", "add_rinst",
               "add_og_e", "add_ib", "no_rule", "add_inf",
               "add_inf_e", "add_rapp", "add_tdeppp", "add_tpo", "add_spstar", "multivar",
               "dettep_cc", "ec_cc", "og_cc", "spp_cc", "inf_cc")

.dwr.sub <- c("mkern", "pkern")

# facteurs de rescalage du bruit infra-annuel (dwrsm3g) : calibrés pour que
# les agrégats aux frontières d'année gardent la distribution de dwrsm3.
#  - k_ogn : l'injection annuelle de bruit sur og est la somme pondérée des
#    bruits mensuels (AR(1) racine appariée a^(1/n_sub)), les poids étant la
#    décroissance ECM intra-année (1-og_mce)^((n_sub-m)/n_sub) ;
#  - k_inf : l'inflation de décembre sur décembre est la somme des déviations
#    mensuelles d'inflation (AR(1) mensuel, racine appariée) ;
#  - exacts (égaux à 1) pour n_sub = 1 ; approximation pour inf_lag > 0
#    (racine dominante).
sub_noise_factors <- function(n_sub, ogn_ar = 0, og_mce = 0.29,
                              inf_mce = 0.68, inf_lag = 0) {
  if (n_sub == 1)
    return(list(k_ogn = 1, k_inf = 1))
  a <- min(max(ogn_ar, 0), 0.999)
  a_m <- a^(1/n_sub)
  q <- (1 - min(max(og_mce, 0), 0.999))^(1/n_sub)
  m <- seq_len(n_sub)
  w <- q^(n_sub - m)
  psi <- sum(outer(w, w) * a_m^abs(outer(m, m, "-")))
  k_ogn <- sqrt((1 - a) * (1 + a_m) / ((1 + a) * (1 - a_m) * psi))
  rho_a <- min(max(1 - inf_mce + inf_lag, -0.999), 0.999)
  rho_m <- sign(rho_a) * abs(rho_a)^(1/n_sub)
  k <- 1:(n_sub - 1)
  g <- n_sub + 2 * sum((n_sub - k) * rho_m^k)
  k_inf <- sqrt((1 - rho_m^2) / ((1 - rho_a^2) * g))
  list(k_ogn = k_ogn, k_inf = k_inf)
}

# dust_check_params
# si params$n_sub est présent (modèle dwrsm3g) : n_sub est entier côté dust,
# les noyaux mkern/pkern sont recadrés à n_sub (pas periods) et remplis de 1
# par défaut, les switchs mkern_use/pkern_use à 0
dust_fix_params <- function(params, long = .dwr.long, sub = .dwr.sub) {
  long_exo <- params |> purrr::keep(~length(.x)>1) |> names()
  # pas moyen de récupérer cette liste de l'objet dust model
  long_exo <- unique(c(long_exo, long))
  if(!is.null(params$n_sub))
    long_exo <- setdiff(long_exo, sub)
  ss <- purrr::map(
    rlang::set_names(long_exo), 
    ~{
      if(!is.null(params[[.x]])) 
        trimortrick(params[[.x]], params$periods)
      else
        rep(0,params$periods)
    })
  p <- purrr::list_modify(params, !!!ss)
  if(!is.null(p$n_sub)) {
    p$n_sub <- as.integer(p$n_sub)
    ss_sub <- purrr::map(
      rlang::set_names(sub),
      ~{
        if(!is.null(p[[.x]]))
          trimortrick(p[[.x]], p$n_sub)
        else
          rep(1, p$n_sub)
      })
    kf <- sub_noise_factors(
      p$n_sub,
      ogn_ar = p$ogn_ar %|||% 0,
      og_mce = p$og_mce %|||% 0.29,
      inf_mce = p$inf_mce %|||% 0.68,
      inf_lag = p$inf_lag %|||% 0)
    p <- purrr::list_modify(
      p,
      !!!ss_sub,
      k_ogn = p$k_ogn %|||% kf$k_ogn,
      k_inf = p$k_inf %|||% kf$k_inf,
      mkern_use = p$mkern_use %|||% 0,
      pkern_use = p$pkern_use %|||% 0)
  }
  p
}

# simulations ---------------
# lance une simulation dust : instancie le modèle, simule tous les steps
# (annuels, ou infra-annuels au pas 1/n_sub) et agrège en annuel si demandé ;
# renvoie le cube (états, tirages, steps) avec les états en dimnames.
# p doit être passé par dust_fix_params au préalable.
run_dust <- function(p, model, seed = NULL, detail = c("annual", "monthly")) {
  detail <- match.arg(detail)
  m <- model$new(pars=p, time=1, n_particles = p$draws, n_threads = 4L, seed = seed)
  state_names <- names(m$info()$index)
  # les variables de sortie sont indicées par _o
  out_i <- which(stringr::str_detect(state_names, "_o$"))
  m$set_index(out_i)
  out_names <- state_names[out_i]
  # pas infra-annuel (dwrsm3g) : on simule au pas 1/n_sub année
  n_sub <- p$n_sub
  if (is.null(n_sub)) {
    # on simule une année de plus
    res <- m$simulate(1:(p$periods+1))
  } else {
    n_sub <- as.integer(n_sub)
    res <- m$simulate(1:(p$periods*n_sub+1))
  }
  dimnames(res)[[1]] <- out_names
  # on drop le premier step qui ne contient pas d'info
  res <- res[,,-1, drop=FALSE]
  if (!is.null(n_sub) && detail == "annual")
    res <- aggregate_annual(res, n_sub, out_names)
  res
}

# simulations avec quantiles / moyenne, plus la trajectoire déterministe
# (bruit coupé) ajoutée en colonne deter ; deterministic = FALSE retrouve
# le comportement antérieur (ni colonne ni second run).
calc_sim_dust <- function(params, model, probs=c(0.025, 0.5, 0.975),
                          history = NULL,
                          long= .dwr.long,
                          full_mc = FALSE,
                          detail = c("annual", "monthly"),
                          deterministic = TRUE) {
  detail <- match.arg(detail)
  if (is.null(params$n_sub) && detail == "monthly")
    stop("detail = 'monthly' requiert params$n_sub (modèle dwrsm3g)")
  if (full_mc && detail == "monthly")
    stop("full_mc n'est pas encore adapté au détail mensuel")
  p <- dust_fix_params(params, long=long)
  n_sub <- p$n_sub
  if(is.null(params$seed))
    seed <- NULL
  else
    seed <- as.integer(params$seed)
  res <- run_dust(p, model, seed, detail)
  out_names <- dimnames(res)[[1]]
  # trajectoire déterministe (colonne deter) : même modèle sans bruit.
  # stochastic = 0 coupe le bruit côté odin (multiplicateur des écarts-types
  # du bruit) ; on éteint aussi les sigmas au cas où le modèle ne déclare
  # pas encore le paramètre. Un seul tirage suffit, ils seraient identiques.
  res_det <- NULL
  if (deterministic) {
    p_det <- purrr::list_modify(p, stochastic = 0, draws = 1,
                               ogn_sigma = 0, inf_sigma = 0)
    res_det <- run_dust(p_det, model, seed, detail)
  }
  se <- stringr::str_c("q", setdiff(probs, 0.5))
  if(params$draws>1)
    sim <- purrr::map_dfr(out_names, ~{
      res_ref <- abind::adrop(res[.x,,,drop=FALSE], drop=1)
      m <- matrixStats::colQuantiles(res_ref, probs=probs, drop=FALSE, na.rm=TRUE)
      mean <- matrixStats::colMeans2(res_ref, na.rm = TRUE)
      colnames(m) <- stringr::str_c("q", probs)
      tibble::as_tibble(m)  |>  
        dplyr::mutate(
          mean = !!mean,
          variable = stringr::str_remove(.x, "_o$") , 
          step = 1:nrow(m))
    })
  else 
  {
    rr <- tibble::as_tibble(t(abind::adrop(res, drop=2))) |> 
      dplyr::mutate(step = dplyr::row_number()) |> 
      tidyr::pivot_longer(cols = -step, names_to = "variable", values_to = "q0.5") |> 
      dplyr::mutate(variable = stringr::str_remove(variable,"_o$"))
    sim <- dplyr::bind_cols(
      rr,
      dplyr::bind_cols(purrr::map(se, ~tibble::tibble(!!.x := rr$q0.5)))) |> 
      mutate(mean = q0.5)
  }
  if(!is.null(res_det)) {
    # colonne deter : trajectoire sans bruit (tirage unique)
    dd <- tibble::as_tibble(t(abind::adrop(res_det, drop=2))) |> 
      dplyr::mutate(step = dplyr::row_number()) |> 
      tidyr::pivot_longer(cols = -step, names_to = "variable", values_to = "deter") |> 
      dplyr::mutate(variable = stringr::str_remove(variable,"_o$"))
    sim <- dplyr::left_join(sim, dd, by = c("variable", "step")) |> 
      dplyr::relocate(deter, .after = mean)
  }
  
  if(!is.null(history))
  {
    start_year <- max(history$year)
    # l'historique est déterministe par nature : deter = q0.5
    hist <- history |> mutate(mean = q0.5)
    if(!is.null(res_det))
      hist <- hist |> mutate(deter = q0.5)
    if (detail == "monthly") {
      # pas infra-annuel : step = indice du mois simulé
      sim <- sim |> 
        dplyr::mutate(year = start_year + 1 + (step-1) %/% n_sub,
                      month = 1 + (step-1) %% n_sub) |> 
        dplyr::bind_rows(hist) |> 
        dplyr::arrange(year) |> 
        dplyr::select(-step)
    } else {
      sim <- sim |> 
        dplyr::mutate(year = start_year+step) |> 
        dplyr::bind_rows(hist) |> 
        dplyr::arrange(year) |> 
        dplyr::select(-step)
    }
  }
  else if (detail == "monthly") {
    sim <- sim |>
      dplyr::mutate(year = 1 + (step-1) %/% n_sub,
                    month = 1 + (step-1) %% n_sub) |>
      dplyr::select(-step)
  }
  else 
    sim <- sim |>
    dplyr::rename(year=step)
  if(full_mc) {
    fsim <- map_dfr(1:p$draws, ~{
      tibble::as_tibble(t(res[,.x,])) |> 
        dplyr::mutate(step = dplyr::row_number()) |> 
        tidyr::pivot_longer(cols = -step, names_to = "variable", values_to = "sim") |> 
        dplyr::mutate(variable = stringr::str_remove(variable,"_o$"),
                      draw = .x)
    })
    
    if(!is.null(history)) {
      start_year <- max(history$year)
      fsim <- fsim |> 
        dplyr::mutate(year = start_year+step) |> 
        dplyr::arrange(draw, year) |> 
        dplyr::select(-step)
    } else 
      fsim <- fsim |>
        dplyr::rename(year=step)
    
    return(list(sim = sim, full_mc = fsim))      
  }
  return(sim)
}

.dwr.agg <- list(
  # ratios et taux annualisés : moyenne annuelle
  mean = c("og", "tcho", "tvnairu", "sppsd", "tpo", "ib", "ib_po", "ib_dep",
           "spstar", "r_app", "r_inst", "ec", "vog"),
  # stocks, cumulants et ratios de flux budgétaires : valeur de décembre
  # (fin d'année). dwrsm3g sort les ratios de flux (spp, ci, sep, ssp, tdep,
  # tdeppp) comme ratios de sommes YTD : leur valeur de décembre est le ratio
  # annuel Σ flux / Σ vpib, cohérent avec l'accumulation de dette ; taut est
  # constant intra-année (décembre = moyenne).
  dec = c("dettep", "dettepp", "qpib", "ppib", "loss", "loss_d_only", "loss_nd",
          "spp", "ci", "sep", "ssp", "tdep", "tdeppp", "taut"),
  # taux de croissance : composition géométrique (décembre sur décembre)
  geo = c("gpib", "gpot", "txppib")
)

# agrège un tableau de simulation infra-annuelle (états, tirages,
# periods*n_sub) en série annuelle (états, tirages, periods)
aggregate_annual <- function(res, n_sub, out_names, agg = .dwr.agg) {
  if (n_sub == 1)
    return(res)
  v <- stringr::str_remove(out_names, "_o$")
  per <- dim(res)[[3]] %/% n_sub
  i_dec <- which(v %in% agg$dec)
  i_geo <- which(v %in% agg$geo)
  i_mean <- setdiff(seq_along(v), c(i_dec, i_geo))
  # décembre par défaut, puis moyenne / composition géométrique
  out <- res[, , seq_len(per) * n_sub, drop = FALSE]
  # cube[s, d, p, m] = mois m de l'année p (attention au repliement col-major)
  cube <- aperm(`dim<-`(res, c(dim(res)[[1]], dim(res)[[2]], n_sub, per)),
                c(1, 2, 4, 3))
  if (length(i_mean) > 0)
    out[i_mean, , ] <- apply(cube[i_mean, , , , drop = FALSE], c(1, 2, 3), mean)
  if (length(i_geo) > 0)
    out[i_geo, , ] <- apply(cube[i_geo, , , , drop = FALSE], c(1, 2, 3),
                            function(x) prod(1 + x) - 1)
  out
}

add_previous <- function(ns, ps) {
  if(!purrr::is_empty(ps))
  {
    dplyr::left_join(ns, ps |> dplyr::select(year, variable, pq0.5 = q0.5), by=c("year", "variable"))
  }
  else 
    dplyr::mutate(ns, pq0.5 = NA_real_)
}

# applique le facteur pour l'affichage par exemple des %
# à renseigner dans le fichier sliders.xlsx
transform_params <- function(input, globals)
{
  from_input <- map(set_names(names(globals$sliders)), ~{
    type <- globals$sliders[[.x]][["type"]]
    if(type=="slider")
      return(input[[.x]]/globals$sliders[[.x]][["facteur"]])
    if(type=="pick")
      return(as.numeric(input[[.x]]))
    if(type=="pick_str")
      return(as.character(input[[.x]]))
    if(type=="date")
      return(lubridate::year(input[[.x]]))
    if(type=="check")
      return(as.logical(input[[.x]]))
  })
  return(from_input)
}

# inverse la transformation précédente
undo_transform <- function(p, globals) {
  to_undone <- purrr::map_dbl(
    rlang::set_names(names(globals$sliders)),
    ~globals$sliders[[.x]][["facteur"]])
  to_undone <- to_undone[!is.na(to_undone) & to_undone!=1]
  undone <- map(rlang::set_names(names(to_undone)), ~{
    p[[.x]]*to_undone[[.x]]
  })
  purrr::list_modify(p, !!!undone)
}

# généralise l'opérateur %||%
`%|||%` <- function (x, y) 
{
  if (purrr::is_empty(x)) 
    y
  else x
}

set_params <- function(inputs, globals, datas)
{
  if(!is.null(inputs$start_year))
    s_y <- inputs$start_year
  else
    s_y <- datas$start_year
  if(!is.null(inputs$end_year))
    e_y <- inputs$end_year
  else
    e_y <- datas$end_year
  periods <- e_y - s_y
  history <- NULL
  # les parametres
  p <- datas$p_def
  p <- purrr::list_modify(p, periods=periods)
  p <- purrr::list_modify(p, !!!datas$p_init)
  p <- purrr::list_modify(p, !!!inputs)
  
  dtog <- inputs$i_og%|||%0
  ddette <- inputs$i_dette%|||%0
  dtpo <- inputs$i_tpo%|||%0
  dtdeppp <- inputs$i_tdeppp%|||%0
  p$i_og <- NULL
  p$i_dette <- NULL
  p$i_tpo <- NULL
  p$i_tdeppp <- NULL
  p$i_lagog <- p$i_lagog%|||%0 + dtog 
  p$i_lagdette <- p$i_lagdette%|||%0*(1+ddette)
  p$i_lagpibpot <- p$i_lagqpib/(1 + p$i_lagog) 
  dr_app <- inputs$i_r_app%|||%0
  
  p$i_tpo <- NULL
  p$i_tdeppp <- NULL
  p$i_r_app <- NULL
  p$i_lagtdeppp <- p$i_lagtdeppp%|||%0 + dtdeppp 
  p$i_lagtpo <- p$i_lagtpo%|||%0 + dtpo
  p$i_lagr_app <- p$i_lagr_app%|||%0 + dr_app
  
  # du coup on modifie l'historique
  history <- forme_history(
    datas$historical_data |> 
      dplyr::filter(year>=inputs$start_hist))
  full_history <- forme_history(datas$full_historical_data) |>
    select(-q0.025, -q0.975) |>
    rename(full_h = q0.5) 
  ses <- names(history |> dplyr::select(dplyr::starts_with("q0.", ignore.case = FALSE)) |> dplyr::select(-q0.5))
  names(ses) <- ses
  
  identities <- purrr::map(ses, ~ function(x) x)
  new <- history |> 
    dplyr::filter(year==s_y) |>
    dplyr::select(year, q0.5, variable) |> 
    tidyr::pivot_wider(names_from = variable, values_from = q0.5) |> 
    dplyr::mutate(
      spp = spp + dtpo - dtdeppp/(1+og),
      sep = spp - ci,
      tpo = tpo + dtpo, 
      tdep = tdep + dtdeppp/(1+og),
      ib_dep = ib_dep + dtdeppp,
      ib_po = ib_po + dtpo,
      og = og + dtog,
      pibpot = qpib/(1+og),
      dettep = dettep * (1+ddette),
      r_app = r_app + dr_app,
      ci  = ci + dr_app * dettep) |> 
    tidyr::pivot_longer(cols=-year, values_to = "q0.5", names_to = "variable") |> 
    dplyr::mutate(dplyr::across(q0.5, .fns = identities, .names="{.fn}"))
  
  history <- history |> 
    dplyr::rows_update(new, by=c("year","variable"))
  # le potentiel
  if(!is.null(p$use_gpot)&&p$use_gpot)
    p$gpot <- trimlast(inputs$gpot, periods)
  else 
    p$gpot <- rep(inputs$gpot_sj,periods)
  # les depenses publiques et les po
  p$tx_deriv_dep <- c(rep(inputs$ddep/10, min(10, periods)), rep(0, max(periods-10, 0)))
  p$tx_deriv_po <- c(rep(inputs$dtpo/10, min(10, periods)), rep(0, max(periods-10, 0)))
  # les add_facteurs
  p$add_tpo <- trim0(inputs$add_tpo, periods)
  p$add_tdeppp <- trim0(inputs$add_tdeppp, periods)
  p$add_og <- trim0(inputs$add_og, periods)
  p$add_og_e <- trim0(inputs$add_og_e, periods)
  p$dette_oneoff <- trim0(inputs$dette_oneoff, periods)
  p$add_ib <- trim0(inputs$add_ib, periods)
  p$add_inf <- trim0(inputs$add_inf, periods)
  p$add_inf_e <- trim0(inputs$add_inf_e, periods)
  p$add_tcho <- trim0(inputs$add_tcho, periods)
  p$add_rapp <- trim0(inputs$add_rapp, periods)
  p$add_rinst <- trim0(inputs$add_rinst, periods)
  p$multivar <- trimlast(inputs$multivar, periods)
  p$no_rule <- trimlast(inputs$no_rule, periods)
  # checke pas de montecarlo
  if(!is.null(inputs$go_mc)&&!inputs$go_mc) {
    p$draws  <- 1
    p$ogn  <-  0
    p$ogn_sigma <- 0
    p$inf_sigma <- 0
  }
  
  # on change les vecteurs à la bonne longueur en les coupant ou étendant si nécessaire
  long_exo <- p |> purrr::keep(~length(.x)>1) |> names()
  # les noyaux infra-annuels sont recadrés à n_sub (dans dust_fix_params), pas à periods
  if(!is.null(p$n_sub))
    long_exo <- setdiff(long_exo, .dwr.sub)
  ss <- purrr::map(rlang::set_names(long_exo), ~trimortrick(p[[.x]], periods))
  p <- purrr::list_modify(p, !!!ss)
  
  return(list(p=p, h=history, fh=full_history, start_year=s_y, periods=periods, end_year = s_y + periods, start_hist = inputs$start_hist))
}

trim0 <- function(x, length) {
  if(length(x)>=length)
    return(x[1:length])
  return(c(x, rep(0, length-length(x))))
}
trimlast <- function(x, length) {
  if(length(x)>=length)
    return(x[1:length])
  return(c(x, rep(dplyr::last(x), length-length(x))))
}


calc_rule_params <- function(globals, params, draws=5000) {
  # on calcule la regle optimale (lineaire) si le switch est frappe
  # pour les lag ajouter "tpo_1dstar", "tpo_1sstar", "tpo_1og"
  # pour les termes quadratiques ajouter "tpo_dstar2", "tpo_sstar2", "tpo_og2"
  # on sort une expression avec la regle budgetaire prete a etre affichee
  controls_c <- list(tpo_sstar = c(0.005 , 0.9), tpo_dstar = c(-0.4, -0.005), tpo_og = c(-1.5, 0))
  controls <- purrr::map_dbl(controls_c, mean)
  par_fr <- best_FR_par(
    params, globals$model,
    controls = controls, 
    constraints = controls_c,
    loss = medianloss2,
    draws = draws ,
    dt = 20)
  if(!is.null(par_fr$error))
    return(list(error = TRUE))
  p <- purrr::list_modify(params, !!!par_fr, error=FALSE)
  return(p)
}


do_rule_text <- function(tpo_og, tpo_sstar, tpo_dstar) {
  if(is.null(tpo_og)||is.null(tpo_dstar)||is.null(tpo_sstar))
    return("")
  c1 <- signif(tpo_og,2)
  c2 <- signif(tpo_sstar,2)
  c3 <- ifelse(tpo_dstar>0, stringr::str_c("+", signif(tpo_dstar,2)), signif(tpo_dstar,2))
  glue::glue(
    "ib = {{c1}} \\times og + {{c2}} \\times (s_p - s^*) \\\\ {{c3}} \\times (d-d^*)",
    .open="{{", .close="}}")
}


trimortrick <- function(s,t)
{
  if(length(s)>=t)
    return(s[1:t])
  return(c(s, rep(dplyr::last(s), t-length(s))))
}

