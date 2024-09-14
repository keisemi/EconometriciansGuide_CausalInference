#'@export
return_dgp_cluster <-
  function(
    N,
    N_C
  ) {
    X <-
      rep(
        0,
        length = N * N_C
      )
    C <-
      rep(
        1:N_C,N
      )
    X[C <= ceiling(N_C/6)] <- 1

    e_c <-
      rnorm(
        N_C
      )
    lmd <-
      rnorm(
        N * N_C,
        mean = 2
      )^2
    tau <-
      rep(
        0,
        length = N_C
      )
    e <-
      lmd * e_c[C] +
      rnorm(
        N * N_C,
        sd = 0.1
      )

    # tau[1] <- -2
    # tau[2:floor(N_C/2)] <- -1
    # tau[(ceiling(N_C/2)+1):N_C] <- 1
    # tau[N_C] <- 2
    Y <-
      0.5 +
      X * tau[C] +
      e
    return(
      data.frame(
        Y = Y,
        X = X,
        C = C
      )
    )
}

#'@export
compute_t_statistics_null_effect <-
  function(
      N,
      M,
      alpha,
      seed
  ) {
    set.seed(seed)

    z <-
      (
        runif(N) >= 0.5
      )

    df_list <-
      seq_len(M) %>%
      purrr::map(
        ~ tibble::tibble(
          z = z,
          y = rnorm(N)
        )
      )

    result_list <-
      df_list %>%
      purrr::map(
        ~ lm(
          formula = y ~ z,
          data = .
        )
      )

    t_list <-
      result_list %>%
      purrr::map(
        ~ summary(.) %>%
          coef() %>%
          .[
            "zTRUE",
            "t value"
          ]
      ) %>%
      purrr::reduce(c)

    return(t_list)
  }

#'@export
compute_t_statistics_alternative_effect <-
  function(
    N,
    M,
    alpha,
    seed
  ) {
    set.seed(seed)

    z <-
      (
        runif(N) >= 0.5
      )

    df_list <-
      seq_len(M-1) %>%
      purrr::map(
        ~ tibble::tibble(
          z = z,
          y = rnorm(N) + 0.3*z
        )
      )

    df_list <- c(
      df_list,
      seq(M-1,M) %>%
        purrr::map(
          ~ tibble::tibble(
            z = z,
            y = rnorm(N) + 0.5*z
          )
        )
    )

    result_list <-
      df_list %>%
      purrr::map(
        ~ lm(
          formula = y ~ z,
          data = .
        )
      )

    t_list <-
      result_list %>%
      purrr::map(
        ~ summary(.) %>%
          coef() %>%
          .[
            "zTRUE",
            "t value"
          ]
      ) %>%
      purrr::reduce(c)

    return(t_list)
  }

#'@export
compute_p_value_mixed_effect <-
  function(
    N,
    M,
    M_0,
    seed
  ) {
    z <-
      (
        runif(N) >= 0.5
      )

    df_list_1 <-
      seq_len(M - M_0) %>%
      purrr::map(
        ~ tibble::tibble(
            z = z,
            y = rnorm(N) + 0.2 * z
          )
      )

    df_list_0 <-
      seq_len(M_0) %>%
      purrr::map(
        ~ tibble::tibble(
            z = z,
            y = rnorm(N)
          )
      )

    result_list_1 <-
      df_list_1 %>%
      purrr::map(
        ~ lm(
            formula = y ~ z,
            data = .
          )
      )

    result_list_0 <-
      df_list_0 %>%
      purrr::map(
        ~ lm(
            formula = y ~ z,
            data = .
          )
      )

      p_list_1 <-
      result_list_1 %>%
      purrr::map(
        ~ summary(.) %>%
          coef() %>%
          .[
            "zTRUE",
            "Pr(>|t|)"
          ]
      ) %>%
      purrr::reduce(c)

    p_list_0 <-
      result_list_0 %>%
      purrr::map(
        ~ summary(.) %>%
          coef() %>%
          .[
            "zTRUE",
            "Pr(>|t|)"
          ]
      ) %>%
      purrr::reduce(c)

    return(
      list(
        p_list_1 = p_list_1,
        p_list_0 = p_list_0
      )
    )
  }

#'@export
compute_p_value_benjamini_hotchberg <-
  function(
    p_list_1,
    p_list_0,
    alpha
  ) {
    p_list_sorted <-
      c(
        p_list_1,
        p_list_0
      ) %>%
      sort()
    M <-
      length(p_list_sorted)
    i <- M
    flag_continue <- TRUE
    while (
      flag_continue & i > 1
    ) {
      test <- p_list_sorted[i] <= (i / M) * alpha
      if (
        test
      ) {
        flag_continue <- FALSE
      }
      i <- i - 1
    }
    return(
      p_list_sorted[i]
    )
  }

#'@export
compute_fdr <-
  function(
    p_list_1,
    p_list_0,
    p_value
  ) {
      p_list_rejected_1 <-
        p_list_1[
          p_list_1 <= p_value
        ]

      p_list_rejected_0 <-
        p_list_0[
          p_list_0 <= p_value
        ]

      fdr <-
        length(p_list_rejected_0) / (
          length(p_list_rejected_0) +
          length(p_list_rejected_1)
        )

      return(fdr)
  }

#'@export
compute_p_value_benjamini_yekutieli <-
  function(
    p_list_1,
    p_list_0,
    alpha
  ) {
    p_list_sorted <-
      c(
        p_list_1,
        p_list_0
      ) %>%
      sort()
    i <- length(p_list_sorted)
    flag_continue <- TRUE
    while (
      flag_continue & i > 1
    ) {
      test <-
        (
          p_list_sorted[i] <= (i / M) * alpha / sum(1 / seq_len(M))
        )
      if (
        test
      ) {
        flag_continue <- FALSE
      }
      i <- i - 1
    }
    return(
      p_list_sorted[i]
    )
  }
