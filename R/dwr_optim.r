onesim <- function(params, model, n_threads = 2L, 
                   vars = c("loss_nd", "og")) {
  p <- params
  p$seed <- NULL
  periods <- params$periods
  seed  <- if(is.null(params$seed)) NULL else as.integer(params$seed) 
  m <- model$new(pars=p, time=1, n_particles = params$draws, n_threads = n_threads, seed = seed)
  state_names <- names(m$info()$index)
  out_i <- which(state_names%in%str_c(vars, "_o"))
  m$set_index(out_i)
  res <- m$simulate(1:(periods+1))
  res <- res[,,-1, drop=FALSE]
  names <- state_names[out_i] |> str_remove("_o$")
  dimnames(res)[[1]] <- names
  if(params$draws>1)
    sim <- purrr::map(rlang::set_names(names), ~{
      matrixStats::colMedians(abind::adrop(res[.x,,,drop=FALSE], drop=1), drop=FALSE)
    })
  else 
    sim <-t(abind::adrop(res, drop=2)) |> as_tibble()
  return(sim)
}

medianloss1 <- function(params, model, controls, n_threads = 2) {
  p <- params
  m <- model$new(pars=p, time=0, n_particles = params$draws, n_threads = n_threads, seed = p$seed)
  state_names <- names(m$info()$index)
  out_i <- which(state_names=="loss_o")
  m$set_index(out_i)
  # on simule une annee de plus
  res <- m$run(params$periods+1)
  res <- matrixStats::rowMedians(res)
  lasso_loss <- sum(purrr::as_vector(params[controls])^2) * params$loss_lasso
  return(res+lasso_loss)
}

medianloss2 <- function(params, model, controls=c("tpo_og", "tpo_dstar", "tpo_sstar"), n_threads = 2L) {
  p <- params
  p$seed <- NULL
  periods <- params$periods
  seed  <- if(is.null(params$seed)) NULL else as.integer(params$seed) 
  m <- model$new(pars=p, time=0, n_particles = params$draws, n_threads = n_threads, seed = seed)
  state_names <- names(m$info()$index)
  out_i <- which(state_names%in%c("loss_nd_o", "dettep_o", "og_o"))
  m$set_index(out_i)
  # on simule une annee de plus
  res <- m$simulate(1:(periods+1))
  res <- res[,,-1, drop=FALSE]
  dimnames(res)[[1]] <- state_names[out_i]
  if(params$draws>1)
    sim <- purrr::map(rlang::set_names(state_names[out_i]), ~{
      matrixStats::colMedians(abind::adrop(res[.x,,,drop=FALSE], drop=1), drop=FALSE)
    })
  else 
    sim <-t(abind::adrop(res, drop=2)) |> as_tibble()
  loss_part1 <- sim$loss_nd_o[length(sim$loss_nd_o)]
  # loss_part1 <- sum(sim$og_o[sim$og_o<0])^2
  loss_part2 <- sum((sim$dettep_o[(params$loss_t+1):periods]-params$dstar)^2) * params$loss_d
  lasso_loss <- sum(purrr::as_vector(params[controls])^2) * params$loss_lasso
  return(loss_part1 + loss_part2 + lasso_loss)
}

pure_loss <- function(params, model, n_threads = 4L) {
  sim <- onesim(params, model, vars = c("loss_nd", "og"))
  mmin <- abs(min(sim$og))
  loss_exp <- params$loss_exp %||% 2
  loss_oge <- params$loss_oge %||% 0
  loss_ogpe <- params$loss_ogpe %||% 0
  og2_loss <- mmin * sum( abs(sim$og/mmin) ^ loss_exp )^(1/loss_exp)
  ogp2_loss <- mmin * sum( (-sim$og[sim$og<0]/mmin) ^ loss_exp )^(1/loss_exp)
  loss <- sim$loss_nd[length(sim$loss_nd)] + 
    loss_oge*og2_loss + loss_ogpe*ogp2_loss
  return(loss)
}

debt_dstar <- function(params, model, n_threads = 4L) {
  if(is.null(params$cons_d) || params$cons_d==0)
    return(0)
  sim <- onesim(params, model, vars = c("dettepp"))
  ll <- params$cons_dt:min(params$cons_dt+params$cons_d-1, params$periods)
  eq <- sim$dettepp[ll] - params$dstar
  return(eq)
}

sp_sstar <- function(params, model, n_threads = 4L) {
  if(is.null(params$cons_sp) || params$cons_sp==0)
    return(0)
  sim <- onesim(params, model, vars = c("spp", "sppsd"))
  ll <- params$cons_spt:min(params$cons_spt+params$cons_sp-1, params$periods)
  # sppstar <- p$add_spstar %||% 0
  eq <- sim$spp[ll] - sim$sppsd[ll] 
  return(eq)
}

constrainedMedianLoss2 <- function(params, model, controls, ccont, n_threads = 2) {
  if(is.null(ccont))
    return(medianloss2(params, model, controls, n_threads))
  ccc <- purrr::map(
    rlang::set_names(names(ccont)),
    ~min(max(params[[.x]],ccont[[.x]][[1]]), ccont[[.x]][[2]])
  )
  params <- list_modify(params,!!!ccc)
  medianloss2(params, model, names(ccont), n_threads =2 )
}

meanloss2 <- function(params, model, controls=c("tpo_og", "tpo_dstar", "tpo_sstar"), n_threads = 2) {
  p <- params
  periods <- params$periods
  m <- model$new(pars=p, step=1, n_particles = params$draws, n_threads = n_threads, seed = p$seed)
  state_names <- names(m$info()$index)
  out_i <- which(state_names%in%c("loss_nd_o", "dettep_o"))
  m$set_index(out_i)
  # on simule une annee de plus
  res <- m$simulate(1:(periods+1))
  res <- res[,,-1, drop=FALSE]
  dimnames(res)[[1]] <- state_names[out_i]
  if(params$draws>1)
    sim <- purrr::map(rlang::set_names(state_names[out_i]), ~{
      matrixStats::colMeans2(abind::adrop(res[.x,,,drop=FALSE], drop=1), drop=FALSE)
    })
  else 
    sim <-t(abind::adrop(res, drop=2)) |> as_tibble()
  
  loss_part1 <- sim$loss_nd_o[length(sim$loss_nd_o)]
  loss_part2 <- sum((sim$dettep_o[(params$loss_t+1):periods]-params$dstar)^2) * params$loss_d
  lasso_loss <- sum(purrr::as_vector(params[controls])^2) * params$loss_lasso
  return(log(loss_part1 + loss_part2 + lasso_loss))
}

medianloss2.1 <- function(params, model, controls=c("phi"), n_threads = 4) {
  p <- params
  p$seed <- NULL
  periods <- params$periods
  seed  <- if(is.null(params$seed)) NULL else as.integer(params$seed) 
  m <- model$new(pars=p, time=0, n_particles = params$draws, n_threads = n_threads, seed = seed)
  state_names <- names(m$info()$index)
  out_i <- which(state_names%in%c("loss_nd_o", "loss_d_only_o", "dettep_o", "og_o"))
  m$set_index(out_i)
  # on simule une annee de plus
  res <- m$simulate(1:(periods+1))
  res <- res[,,-1, drop=FALSE]
  dimnames(res)[[1]] <- state_names[out_i]
  if(params$draws>1)
    sim <- purrr::map(rlang::set_names(state_names[out_i]), ~{
      matrixStats::colMedians(abind::adrop(res[.x,,,drop=FALSE], drop=1), drop=FALSE)
    })
  else 
    sim <-t(abind::adrop(res, drop=2)) |> as_tibble()
  df <- ((1+params$loss_df)*rep(1/(1+params$loss_df), length(sim$og_o))) |> cumprod()
  loss_nd <- sum(sim$og_o*df)^2 + sum((sim$og_o*df)^2) * params$loss_og
  loss_donly <- sum((sim$dettep_o[(params$loss_t):periods]-params$dstar)^2) * params$loss_d
  lasso_loss <- sum(purrr::as_vector(params[controls])^2) * params$loss_lasso
  cli::cli_alert_info("ld:{signif(loss_nd, 4)} ld:{signif(loss_donly, 4)} ll:{signif(lasso_loss,4)}")
  cli::cli_alert_info("lnd:{signif(last(sim$loss_nd_o), 4)} ld:{signif(last(sim$loss_d_only_o), 4)}")
  return(loss_nd + loss_donly + lasso_loss)
}

medianloss2.2 <- function(params, model, controls=c("phi"), n_threads = 4, bwd = 10) {
  p <- params
  p$seed <- NULL
  periods <- params$periods
  seed  <- if(is.null(params$seed)) NULL else as.integer(params$seed) 
  m <- model$new(pars=p, time=1, n_particles = params$draws, n_threads = n_threads, seed = seed)
  state_names <- names(m$info()$index)
  out_i <- which(state_names%in%c("loss_nd_o", "loss_d_only_o", "loss_o", "dettep_o"))
  m$set_index(out_i)
  # on simule une annee de plus
  res <- m$simulate(1:(periods+1))
  res <- res[,,-1, drop=FALSE]
  dimnames(res)[[1]] <- state_names[out_i]
  if(params$draws>1)
    sim <- purrr::map(rlang::set_names(state_names[out_i]), ~{
      matrixStats::colMedians(abind::adrop(res[.x,,,drop=FALSE], drop=1), drop=FALSE)
    })
  else 
    sim <-t(abind::adrop(res, drop=2)) |> as_tibble()
  loss_d <- sim$loss_d_only_o[params$periods-bwd]
  loss_nd <- sim$loss_nd_o[params$periods-bwd]
  loss <- loss_d+loss_nd
  # cli::cli_alert_info("lnd:{signif(loss_nd, 4)} ld:{signif(loss_d, 4)} l:{signif(loss, 4)}")
  
  return(loss)
}

medianloss3 <- function(params, model, controls=c("tpo_og", "tpo_dstar", "tpo_sstar"), n_threads = 2) {
  p <- params
  p$draws <- NULL
  periods <- params$periods
  m <- model$new(pars=p, step=1, n_particles = params$draws, n_threads = n_threads, seed = p$seed)
  state_names <- names(m$info()$index)
  out_i <- which(state_names%in%c("og_o", "dettep_o"))
  m$set_index(out_i)
  # on simule une annee de plus
  res <- m$simulate(1:(periods+1))
  res <- res[,,-1, drop=FALSE]
  dimnames(res)[[1]] <- state_names[out_i]
  if(params$draws>1)
    sim <- purrr::map(rlang::set_names(state_names[out_i]), ~{
      matrixStats::colMedians(abind::adrop(res[.x,,,drop=FALSE], drop=1), drop=FALSE)
    })
  else 
    res
  
  loss_part1 <- sum(sim$og_o^2/ (1+params$df)^(0:(periods-1)))
  loss_part2 <- sum((sim$dettep_o[(params$loss_t+1):periods]-params$dstar)^2)
  log(loss_part1 + params$loss_d * loss_part2)
}

lassomedianloss4 <- function(params, model, controls=c("tpo_og", "tpo_dstar", "tpo_sstar"), n_threads = 2) {
  p <- params
  p$draws <- NULL
  periods <- params$periods
  m <- model$new(pars=p, step=1, n_particles = params$draws, n_threads = n_threads, seed = p$seed)
  state_names <- names(m$info()$index)
  out_i <- which(state_names%in%c("og_o", "dettep_o"))
  m$set_index(out_i)
  # on simule une annee de plus
  res <- m$simulate(1:(periods+1))
  res <- res[,,-1, drop=FALSE]
  dimnames(res)[[1]] <- state_names[out_i]
  if(params$draws>1)
    sim <- purrr::map(rlang::set_names(state_names[out_i]), ~{
      matrixStats::colMedians(abind::adrop(res[.x,,,drop=FALSE], drop=1), drop=FALSE)
    })
  else 
    res
  og4l <- sim$og_o^2/ (1+params$loss_df)^(0:(periods-1))
  d4l <- sim$dettep_o[(params$loss_t+1):periods]
  loss_part1 <- sum(og4l[sim$og<0])
  loss_part2 <- sum((d4l[d4l>params$dstar]-params$dstar)^2) * params$loss_d 
  lasso_loss <- sum(purrr::as_vector(params[controls])^2)
  log(loss_part1 + loss_part2) + log(1+lasso_loss) * params$loss_lasso
}

lassomedianloss5 <- function(params, model, controls=c("tpo_og", "tpo_dstar", "tpo_sstar"), n_threads = 2) {
  p <- params
  p$draws <- NULL
  periods <- params$periods
  m <- model$new(pars=p, step=1, n_particles = params$draws, n_threads = n_threads, seed = p$seed)
  state_names <- names(m$info()$index)
  out_i <- which(state_names%in%c("og_o", "dettep_o"))
  m$set_index(out_i)
  # on simule une annee de plus
  res <- m$simulate(1:(periods+1))
  res <- res[,,-1, drop=FALSE]
  dimnames(res)[[1]] <- state_names[out_i]
  if(params$draws>1)
    sim <- purrr::map(rlang::set_names(state_names[out_i]), ~{
      matrixStats::colMedians(abind::adrop(res[.x,,,drop=FALSE], drop=1), drop=FALSE)
    })
  else 
    res
  og4l <- sim$og_o^2/ (1+params$loss_df)^(0:(periods-1))
  d4l <- sim$dettep_o[(params$loss_t+1):periods]
  loss_part1 <- sum(og4l)
  loss_part2 <- sum((d4l-params$dstar)^2) * params$loss_d 
  lasso_loss <- sum(purrr::as_vector(params[controls])^2)
  log(loss_part1 + loss_part2) + log(1+lasso_loss) * params$loss_lasso
}

calc_phis <- function(globals, params=NULL, dt = 0, algorithm = "l2") {
  # on calcule les phi déterministes
  model <- globals$globals$model
  ps <- globals$params$p |> 
    list_modify(!!!params)
  if(algorithm == "l2")
    par_fr <- best_FR_par_with_phi(
      ps, model,
      dt = dt)
  if(algorithm == "cobyla")
    par_fr <- best_phis_nlcons(
      ps, model,
      dt = dt, method="cobyla")
  if(algorithm == "isres")
    par_fr <- best_phis_nlcons(
      ps, model,
      dt = dt, method="isres")
  else
    par_fr <- best_phis_nlcons(
      ps, model,
      dt = dt, method=algorithm)
  if(!is.null(par_fr$error))
    return(list(error = TRUE))
  p <- purrr::list_modify(ps, !!!par_fr, error=FALSE)
  return(p)
}

best_FR_par <- function(p, model, controls, constraints = NULL, method="Nelder-Mead", loss = medianloss2, draws = 1000, dt = 20) {
  lp <- p
  periods <- lp$loss_t+dt
  if(!is.null(p$seed))
    seed <- p$seed + 1
  else 
    seed <- as.integer(floor(runif(1,1,2*10^9)))
  lp <- purrr::list_modify(lp, periods=periods, draws=draws, seed = seed)
  lp <- dust_fix_params(lp)
  ffo <- function(cont) {
    if(is.null(constraints))
      ccc <- cont
    else {
      ccc <- purrr::map(
        rlang::set_names(names(constraints)),
        ~{
          min(max(cont[.x], constraints[[.x]][[1]]), constraints[[.x]][[2]])
        }
      )
    }
    err <- sum((as_vector(ccc)-cont)^2)
    loss(
      params = purrr::list_modify(lp, !!!ccc), 
      model = model, 
      controls = names(controls),
      n_threads = 2
    ) + err
  }
  
  opt <- try(
    optimx::optimx(
      par=as_vector(controls), 
      fn=\(x) ffo(x),
      method=method,
      control = list(kkt=FALSE, maxit = 2000)
    )
  )
  
  if("try-error"%in%class(opt))
    return(list(error = TRUE))
  
  if(opt$convcode!=0)
  {
    mess <- case_when(
      opt$convcode == 1 ~ "maxit reached",
      opt$convcode == 10 ~ "Nelder-Mead simplex degerenate")
    message("optimisation code {opt$convcode} [{mess}]" |> glue::glue())
  }
  set_names(c(opt[names(controls)]), names(controls))
}

best_FR_par_with_phi <- function(p, model, method="nlm", dt = 40) {
  null_controls <- unique(purrr:::keep(names(p), ~stringr::str_starts(.x, "tpo")))
  null_controls <- rlang::set_names(rep(0, length(null_controls)), null_controls)
  # on cherche les phi déteministes
  periods <- p$loss_t+dt+10
  lp_det <- purrr::list_modify(
    p, 
    periods = periods, 
    ogn_sigma = 0, 
    inf_sigma = 0, 
    ogn_ar = 0,
    draws = 1)
  lp_det <- dust_fix_params(lp_det)
  
  det_with_phi <- function(cont) 
    medianloss2.2(params = purrr::list_modify(lp_det, phi = c(cont, 0)), 
                  model = model, controls = "phi", bwd = 0) 
  
  phic <- lp_det$phi[1:(periods-1)]
  det_opt <- optimx::optimx(
    par= phic,
    fn=\(x) det_with_phi(x),
    method="nlm", control=list(maxit=1000000, abstol = 1e-8), itnmax=1000000 )
  phi <- unlist(det_opt[1:(lp_det$periods)])
  if(det_opt$convcode != 0)
    cli::cli_alert_warning("Echec de l'optimisation code {det_opt$convcode}")
  # phi <- det_opt$estimate[1:lp_det$periods]
  return(list(phi = c(phi, 0)))
}

best_phis_nlcons <- function(p, model, dt = 0, method = "cobyla") {
  period_d <- ifelse(!is.null(p$cons_d) & p$cons_d>0,  p$cons_dt+p$cons_d, 0)
  period_sp  <- ifelse(!is.null(p$cons_sp) & p$cons_sp>0, p$cons_spt+p$cons_sp, 0)
  periods <- max(period_d, period_sp)+dt
  lp_det <- purrr::list_modify(
    p, 
    periods = periods, 
    ogn_sigma = 0, 
    inf_sigma = 0, 
    ogn_ar = 0,
    tpo_dstar = 0, tpo_sstar = 0, tpo_og =0,
    draws = 1)
  lp_det <- dust_fix_params(lp_det)
  
  loss_phi <- function(cont) 
    pure_loss(params = purrr::list_modify(
      lp_det, phi = c(cont, rep(0, max(periods-p$ib_t, 0)))), 
      model = model)
  eq_dstar <- function(cont) 
    debt_dstar(params = purrr::list_modify(
      lp_det,
      phi = c(cont, rep(0, max(periods-p$ib_t, 0)))), model = model)
  eq_sstar <- function(cont) 
    sp_sstar(params = purrr::list_modify(
      lp_det,
      phi = c(cont, rep(0, max(periods-p$ib_t, 0)))), 
      model = model)
  
  eq_func <- function(x) c(eq_dstar(x), eq_sstar(x))
  phic <- lp_det$phi[1:min(p$ib_t, periods)]
  guess <- nloptr::nloptr(
    x0 = phic,
    lb = rep(-p$ibcap, length(phic)),
    ub = rep(+p$ibcap, length(phic)),
    eval_f = loss_phi,
    eval_g_eq = eq_func,
    opts = list(algorithm = "NLOPT_LN_COBYLA",
                maxeval = 10000, ftol_rel= 1e-3, xtol_rel = 1.e-3)  )
  phic <- guess$solution
  det_opt <- NULL
  if(method=="emir::bat") {
    config <- EmiR::config_abc(100, 100, absolute_tol = 1e-4)
    parameters <- map(
      1:length(phic), ~EmiR::parameter(str_c("phi", .x), -p$ibcap, p$ibcap))
    constraints <- list(EmiR::constraint(eq_func, "<="), EmiR::constraint(eq_func, ">="))
    det_pot <- EmiR::minimize(algorithm_id = "ABC", 
                   obj_func = loss_phi,
                   config = config,
                   parameters = parameters,
                   constraints = constraints,
                   constrained_method = "BARRIER")
  }
  if(method=="isres")
    det_opt <- nloptr::isres(
      x0 = phic,
      lower = rep(-p$ibcap, length(phic)),
      upper = rep(+p$ibcap, length(phic)),
      fn = loss_phi,
      heq = eq_func,
      nl.info = TRUE,
      xtol_rel = 1.e-4,
      maxeval = 1000000) 
  if(method=="auglag")
    det_opt <- nloptr::nloptr(
      x0 = phic,
      lb = rep(-p$ibcap, length(phic)),
      ub = rep(+p$ibcap, length(phic)),
      eval_f = loss_phi,
      eval_g_eq = eq_func,
      opts = list(
        algorithm = "NLOPT_LN_AUGLAG_EQ", 
        maxeval = 100000, ftol_rel= 1e-4, ftol_abs = 1e-6, xtol_rel = 1.e-4, xtol_abs = 1e-6,
        local_opts = list(
          algorithm="NLOPT_LN_COBYLA", ftol_rel= 1e-4, ftol_abs = 1e-6, xtol_rel = 1.e-4, xtol_abs = 1e-6) ) )
  if(method=="cobyla")
    det_opt <- nloptr::nloptr(
      x0 = phic,
      lb = rep(-p$ibcap, length(phic)),
      ub = rep(+p$ibcap, length(phic)),
      eval_f = loss_phi,
      eval_g_eq = eq_func,
      opts = list(algorithm = "NLOPT_LN_COBYLA",
                  maxeval = 100000, xtol_rel = 1.e-6,
                  xtol_abs = 1e-8)  )
  if(is.null(det_opt))
    return(list(phi = phic,
                og_cc  = NULL, ec_cc = NULL, dettep_cc = NULL, inf_cc = NULL, spp_cc = NULL,
                code = "erreur"))
  phi <- c(det_opt$solution[1:p$ib_t], rep(0, max(periods-p$ib_t, 0)) )
  final_p <- list_modify(lp_det, phi = phi) |> dust_fix_params()
  cc <- onesim(params = final_p, model = model, 
               vars = c("og", "dettep", "spp", "txppib", "ec"))
  return(list(phi = phi,
              og_cc  = cc$og,
              ec_cc = cc$ec,
              dettep_cc = cc$dettep,
              inf_cc = cc$txppib,
              spp_cc = cc$spp,
              code = det_opt$status,
              message = det_opt$message))
}

best_FR_par_with_phi_only <- function(p, model, method="nlm", loss=medianloss2, draws = 1000, dt = 40) {
  null_controls <- unique(purrr:::keep(names(p), ~stringr::str_starts(.x, "tpo")))
  null_controls <- rlang::set_names(rep(0, length(null_controls)), null_controls)
  lp_sto <- purrr::list_modify(p, periods = p$loss_t+dt, draws = draws, !!!null_controls)
  lp_sto <- dust_fix_params(lp_sto)
  phic <- lp_sto$phi
  with_phi <- function(cont) {
    medianloss1(
      params = purrr::list_modify(lp_sto, phi = cont), 
      model = model, 
      controls = "phi", 
      n_threads = 2) 
  }
  det_opt <- optimx::optimx(par = phic, 
                            fn = \(x) with_phi(x),
                            method= method)
  if(det_opt$convcode!=0)
    message("Convergence not achieved")
  phi <- unlist(det_opt[1:lp_sto$periods])
  names(phi) <- NULL
  list_modify(list(phi = phi), !!!null_controls)
}

