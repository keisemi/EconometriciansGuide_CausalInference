#'@export
generate_dgp_LM_discrete <-
  function(
    N,
    rounding
    ) {
    s <-
      rounding(
        2 *
        rbeta(
          n = N,
          shape1 = 2,
          shape2 = 4
        ) -
        1
      )
    dgp <-
      tibble::tibble(
        s = s,
        y =
          CausalInferenceTextbook::specify_mu_LM_flip(
            s = s
          ) +
        rnorm(
          n = N,
          mean = 0,
          sd = 0.1295
        ),
        d = (s >= 0)
      )
    return(dgp)
  }

#'@export
generate_dgp_LM_discrete_alternate <-
  function(
    N,
    rounding
  ) {
    s_raw <-
      2 *
      rbeta(
        n = N,
        shape1 = 2,
        shape2 = 4
      ) -
      1
    s <- rounding(s_raw)
    dgp <-
      tibble::tibble(
        y =
          CausalInferenceTextbook::specify_mu_LM_flip(
            s = s
          ) +
        rnorm(
          n = N,
          mean = 0,
          sd =
            0.1295 * (
              0.01 +
              abs(s_raw) *
              30
            )
        ),
        s = s,
        d = (s >= 0)
      )
    return(dgp)
  }


#' @export
specify_mu_LM_flip <-
  function(
    s
  ) {
    vec_mu <-
      (s < 0) * (
        0.26
        + 18.49 * (-s)
        - 54.8 * (-s)^2
        + 74.3 * (-s)^3
        - 45.02 * (-s)^4
        + 9.83 * (-s)^5
      ) +
      (s >= 0) * (
        3.70
        + 2.99 * (-s)
        + 3.28 * (-s)^2
        + 1.45 * (-s)^3
        + 0.22 * (-s)^4
        + 0.03 * (-s)^5
      )
    return(vec_mu)
  }

#' @export
return_OLS <-
  function (
    data
  ) {
    # モーメントの作成
    data$s_2 <- data$s^2
    data$s_3 <- data$s^3
    data$s_4 <- data$s^4

    result <-
      lm(
        data = data,
        formula = "y ~ 1 + d + s + s_2 + s_3 + s_4"
      )
    return(result)
}

#' @export
return_OLS_cluster <-
  function (
    data
  ) {
    # モーメントの作成
    data$s_2 <- data$s^2
    data$s_3 <- data$s^3
    data$s_4 <- data$s^4

    result <-
      miceadds::lm.cluster(
        data = data,
        formula = "y ~ 1 + d + s + s_2 + s_3 + s_4",
        cluster = "s",
        weights = NULL
      )
    return(result)
}

#' @export
call_generate_dgp_LM_discrete <- function(rounding) {
  set.seed(1)
  CausalInferenceTextbook::generate_dgp_LM_discrete(
    N = N,
    rounding = rounding
  )
}

#' @export
call_plot_data <- function(data) {
  f2 <-
    RDHonest::RDScatter(
      y ~ s,
      data = data,
      cutoff = 0,
      avg = Inf,
      xlab = "s",
      ylab = "y",
      propdotsize = TRUE
    )
  f2 +
    scale_size_area(
      max_size = 4
    ) +
    theme_classic()
}


#' @export
call_rdrobust <-
  function(
    data
  ) {
  result <-
    rdrobust::rdrobust(
      y = data$y,
      x = data$s
    )
  return(result)
}

#' @export
call_rdrobust_mass_off <-
  function(
    data
  ) {
  result <-
    rdrobust::rdrobust(
      y = data$y,
      x = data$s,
      masspoints = "off"
    )
  return(result)
}

#' @export
call_rdhonest <-
  function(
    M
  ) {
  result <-
    RDHonest::RDHonest(
      y ~ s,
      data = dgp_all$dgp_005,
      kern = "uniform",
      opt.criterion = "FLCI",
      M = M,
      sclass = "H"
    )
  return(result)
}

#' @export
call_rdhonest_data <-
  function(
    data
  ) {
  result <-
    RDHonest::RDHonest(
      y ~ s,
      data = data,
      kern = "uniform",
      opt.criterion = "FLCI",
      sclass = "H"
    )
  return(result)
}

#' @export
append_rdhonest_table <-
  function(
    table,
    case,
    result
  ) {
  return(
    rbind(
      table,
      data.frame(
        case = case,
        ci_lower = result$coefficients$conf.low,
        ci_upper = result$coefficients$conf.high,
        point_est = result$coefficients$estimate,
        se = result$coefficients$std.error,
        h = result$coefficients$bandwidth,
        M = result$coefficients$M
      )
    )
  )
}

#' @export
append_rdrobust_table <-
  function(
    table,
    case,
    result
  ) {
  return(rbind(
    table,
    data.frame(
      case = case,
      ci_lower = result$ci[3, 1],
      ci_upper = result$ci[3, 2],
      point_est = result$Estimate[1],
      se = result$se[3],
      h = result$bws[1],
      M = NA
    )
  ))
}

#' @export
append_OLS_table <-
  function(
    table,
    case,
    result
  ) {
  table_lm <-
    coef(
      summary(result)
    )
  table <-
    rbind(
      table,
      data.frame(
        case = case,
        ci_lower =
          table_lm[2] -
          table_lm[2, 2] *
          qt(0.975,
             summary(result)$df[2]),
        ci_upper =
          table_lm[2] +
          table_lm[2, 2] *
          qt(0.975,
             summary(result)$df[2]),
        point_est = table_lm[2],
        se = table_lm[2, 2],
        h = NA
      )
    )
  return(
    table
  )
}

#' @export
append_OLS_cluster_table <-
  function(
    table,
    case,
    result_cluster
  ) {
    invisible(capture.output(table_cl <- summary(result_cluster)))
    table <-
      rbind(
        table,
        data.frame(
          case = case,
          ci_lower =
            table_cl[2] -
            table_cl[2, 2] *
            qt(0.975,
               result_cluster$lm_res$df.residual),
          ci_upper =
            table_cl[2] +
            table_cl[2, 2] *
            qt(0.975,
               result_cluster$lm_res$df.residual),
          point_est = NA,
          se = table_cl[2, 2],
          h = NA
        )
      )
  return(
    table
  )
}
