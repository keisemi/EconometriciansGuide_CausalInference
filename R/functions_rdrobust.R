#' @export
specify_mu_lee <-
  function(
    s
  ) {
    vec_mu <-
      (s < 0) * (
        0.48
        + 1.27 * s
        + 7.18 * s^2
        + 20.21 * s^3
        + 21.54 * s^4
        + 7.33 * s^5
      ) +
      (s >= 0) * (
        0.52
        + 0.84 * s
        - 3.00 * s^2
        + 7.99 * s^3
        - 9.01 * s^4
        + 3.56 * s^5
      )
    return(vec_mu)
  }

#' @export
specify_mu_lee_fuzzy <-
  function(
    s,
    d
  ) {
    vec_mu <-
      (d == 0) * (
        0.48
        + 1.27 * s
        + 7.18 * s^2
        + 20.21 * s^3
        + 21.54 * s^4
        + 7.33 * s^5
      ) +
      (d == 1) * (
        0.72
        + 0.84 * s
        - 3.00 * s^2
        + 7.99 * s^3
        - 9.01 * s^4
        + 3.56 * s^5
      )
    return(vec_mu)
  }

#'@export
generate_dgp_lee <-
  function(
    N
  ) {
    dgp_lee <-
      tibble::tibble(
        s = 
          2 * 
          rbeta(
            n = N,
            shape1 = 2,
            shape2 = 4
          ) - 
          1,
        y =
          CausalInferenceTextbook::specify_mu_lee(
            s = s
          )
          + rnorm(
              n = N,
              mean = 0,
              sd = 0.1295
            ),
        d = (s >= 0)
      )
    return(dgp_lee)
}

#'@export
generate_dgp_lee_fuzzy <-
  function(
    N
  ) {
    dgp_lee <-
      tibble::tibble(
        s = 2*rbeta(n=N,shape1=2,shape2=4) - 1,
        z = (s >= 0),
        d = (stats::qnorm(0.3*(0.5+0.5*s) + 0.6*z) >= stats::rnorm(n=N)),
        y =
          CausalInferenceTextbook::specify_mu_lee_fuzzy(
            s = s,
            d = d)
          + rnorm(
            n = N,
            mean = 0,
            sd = 0.1295
          )
      )
    return(dgp_lee)
  }


#' @export
specify_mu_ludwig_miller_modified_plus <-
  function(
    s
  ) {
    s <- s/50
    vec_mu <-
      (
        4.0
        + 1.5 * 2.99 * s
        + 3.28 * s^2
        + 1.45 * s^3
        + 0.22 * s^4
        + 0.03 * s^5
      )
    return(vec_mu)
}

#'@export
specify_mu_ludwig_miller_modified_minus <-
  function(
    s
  ) {
    s <- s / 50
    vec_mu <-
      (
        3.0
        + 2.99 * s
        + 3.28 * s^2
        + 1.45 * s^3
        + 0.22 * s^4
        + 0.03 * s^5
      )
    return(vec_mu)
}

#'@export
specify_mu_ludwig_miller_modified <-
  function(
    s
  ) {
    s <- s / 50
    vec_mu <-
      (s >= 0) * (
        4.0
        + 1.5 * 2.99 * s
        + 3.28 * s^2
        + 1.45 * s^3
        + 0.22 * s^4
        + 0.03 * s^5
      ) +
      (s < 0) * (
        3.0
        + 2.99 * s
        + 3.28 * s^2
        + 1.45 * s^3
        + 0.22 * s^4
        + 0.03 * s^5
      )
    return(vec_mu)
}

