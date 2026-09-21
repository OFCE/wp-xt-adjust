targetloss <- function(params, model, 
                       targets=c("og", "tcho", ""), 
                       n_threads = 2L) {
    p <- params
    p$seed <- NULL
    periods <- params$periods
    seed  <- if(is.null(params$seed)) NULL else as.integer(params$seed) 
    m <- model$new(pars=p, time=0, n_particles = params$draws, n_threads = n_threads, seed = seed)
    state_names <- names(m$info()$index)
    out_i <- which(state_names%in%c("loss_nd_o", "dettep_o"))
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
    loss_part2 <- sum((sim$dettep_o[(params$loss_t+1):periods]-params$dstar)^2) * params$loss_d
    lasso_loss <- sum(purrr::as_vector(params[controls])^2) * params$loss_lasso
    return(loss_part1 + loss_part2 + lasso_loss)
  }