#' @export
generate_df_no_covariates <-
  function(
    N,
    T,
    T0,
    mean_tau_i,
    sd_tau_i
    ) {
      id <-
        matrix(
          rep(
            1:N,
            T
          ),
          N,
          T
        )

      time <-
        t(
          matrix(
            rep(
              1:T,
              N
            ),
            T,
            N
          )
        )

      a_i <-
        rep(
          rnorm(
            n = N,
            mean = 5.4,
            sd = 0.15
          ),
          T
        )

      b_t <-
        rep(
          rnorm(
            n = T,
            mean = 5.4 * 0.02,
            sd = 0.15
          ),
          N
        )

      e_it <-
        matrix(
          rnorm(
            N * T,
            mean = 0,
            sd = 0.03
          ),
          N,
          T
        )

      U_i <-
        rep(
          runif(
            n = N,
            min = 0,
            max =1
          ),
          T
        )

      y0_it <-
        matrix(
          a_i,
          N,
          T
        ) +
        t(
          matrix(
            b_t,
            T,
            N
          )
        ) +
        e_it

      g_i <-
        (
          U_i <=
          exp(-14 + 2.5 * a_i) /
          (1 + exp(-14 + 2.5 * a_i))
        )

      g_i <-
        matrix(
          g_i,
          N,
          T
        )

      z_it <-
        t(
          matrix(
            rep(
              c(
                rep(
                  0,
                  T0
                ),
                rep(
                  1,
                  T - T0
                )
              ),
              N
            ),
            T,
            N
          )
        ) * g_i

      tau_i <-
        rep(
          rnorm(
            n = N,
            mean = mean_tau_i,
            sd = sd_tau_i
          ),
          T
        )

      tau_t <-
        t(
          matrix(
            rep(
              runif(
                n = T,
                min = 0.9,
                max = 1.1
              ),
              N
            ),
            T,
            N
          )
        )

      tau_it <-
        matrix(
          tau_i,
          N,
          T
        ) *
        tau_t

      y_it <-
        y0_it +
        z_it *
        tau_it

      df <-
        tibble::tibble(
          id = as.vector(t(id)),
          time = as.vector(t(time)),
          a_i = as.vector(t(a_i)),
          g_i = as.vector(t(g_i)),
          z_it = as.vector(t(z_it)),
          y0_it = as.vector(t(y0_it)),
          y_it = as.vector(t(y_it)),
          U_i = as.vector(t(U_i)),
          tau_i = as.vector(t(tau_i)),
          tau_t  = as.vector(t(tau_t)),
          tau_it = as.vector(t(tau_it))
        )
      return(df)
  }

#' @export
generate_df_multiperiod <-
  function(
    N,
    T0,
    T1,
    T,
    diff_trend = FALSE,
    mean_tau_i,
    sd_tau_i,
    scale_5,
    scale_6,
    scale_7,
    scale_8,
    slope_x1 = NA,
    slope_x2 = NA
  ) {
    id <- rep(1:N)
    a0 <- 5.4
    a_i <-
      rnorm(
        N,
        mean = 0,
        sd = 0.15
      )
    U_i <-
      runif(
        N,
        0,
        1
      )
    Ux_i <-
      runif(
        N,
        0,
        1
      )
    x1_i <- (Ux_i >= 0.3)
    x2_i <- (Ux_i >= 0.7)
    tau_i <-
      rnorm(
        N,
        mean = mean_tau_i,
        sd = sd_tau_i
      )

    group_i <- 0
    g5_i <- (
      U_i <= exp(-0.25) / (1 + exp(-0.25))
    )
    group_i <-
      group_i +
      g5_i * (T0 + 1)

    g6_i <- (
      (
        U_i > exp(-0.25) / (1 + exp(-0.25))
      ) & (
        U_i <= exp(-0.25 + 0.75 * (x1_i + 0)) / (1 + exp(-0.25 + 0.75 * (x1_i + 0)))
      )
    )
    group_i <-
      group_i +
      g6_i * (T0 + 2)

    g7_i <- (
      (
        U_i > exp(-0.25 + 0.75 * (x1_i + 0)) / (1 + exp(-0.25 + 0.75 * (x1_i + 0)))
      ) & (
        U_i <= exp(-0.25 + 0.75 * (x1_i + x2_i)) / (1 + exp(-0.25 + 0.75 * (x1_i + x2_i)))
      )
    )
    group_i <-
      group_i +
      g7_i * (T0 + 3)

    g8_i <- (
      (
        U_i > exp(-0.25 + 0.75 * (x1_i + x2_i)) / (1 + exp(-0.25 + 0.75 * (x1_i + x2_i)))
      ) & (
        U_i <= exp(-0.25 + 1 * (x1_i + x2_i)) / (1 + exp(-0.25 + 1 * (x1_i + x2_i)))
      )
    )
    group_i <-
      group_i +
      g8_i * (T0 + 4)

    for (
      t in seq(1, T)
    ) {
      time <- rep(t, N)
      if (diff_trend) {
        b_t <- (t / T) * (1 - x1_i - x2_i) + slope_x1 * (t / T) * x1_i + slope_x2 * (t / T) * x2_i
      } else {
        b_t <- t / T
      }
      e_it <-
        rnorm(
            N,
            mean = 0,
            sd = 0.03
        )
      tau_t <-
        runif(
          1,
          min = 0.9,
          max = 1.1
        )
      tau_it <-
        tau_t * (
          scale_5 * abs(tau_i) * g5_i +
          scale_6 * abs(tau_i) * g6_i +
          scale_7 * abs(tau_i) * g7_i +
          scale_8 * abs(tau_i) * g8_i
        )

      z_it <- rep(0, N)
      if (t >= T0 + 1) {
        z5_it <- g5_i
      } else {
        z5_it <- rep(0, N)
      }
      if (t >= T0 + 2) {
        z6_it <- g6_i
      } else {
        z6_it <- rep(0, N)
      }
      if (t >= T0 + 3) {
        z7_it <- g7_i
      } else {
        z7_it <- rep(0, N)
      }
      if (t >= T0 + 4) {
        z8_it <- g8_i
      } else {
        z8_it <- rep(0, N)
      }

      z_it <-
        z5_it +
        z6_it +
        z7_it +
        z8_it

      y0_it <-
        a0 +
        (g5_i + g6_i + g7_i + g8_i) * (-a_i) +
        (1 - g5_i - g6_i - g7_i - g8_i) * (a_i) +
        b_t +
        e_it
      y_it <-
        y0_it +
        z_it * tau_it

      df_period <-
        tibble::tibble(
          id = id,
          x1_i = x1_i,
          x2_i = x2_i,
          group_i = group_i,
          g5_i = g5_i,
          g6_i = g6_i,
          g7_i = g7_i,
          g8_i = g8_i,
          time = time,
          tau_it = tau_it,
          tau_t = rep(
                    tau_t,
                    N
                  ),
          z_it = z_it,
          z5_it = z5_it,
          z6_it = z6_it,
          z7_it = z7_it,
          z8_it = z8_it,
          y0_it = y0_it,
          y_it = y_it
        )
      if (
        t == 1
      ) {
        df_panel <- df_period
      } else {
        df_panel <-
          rbind(
            df_panel,
            df_period
          )
      }
    }
    return(df_panel)
  }

#' @export
generate_df_multiperiod_nyt <-
  function(
    N,
    T0,
    T1,
    T,
    diff_trend = FALSE,
    mean_tau_i,
    sd_tau_i,
    scale_5,
    scale_6,
    scale_7,
    scale_8,
    slope_x1 = NA,
    slope_x2 = NA
  ) {
    id <- rep(1:N)
    a0 <- 5.4
    a_i <-
      rnorm(
        N,
        mean = 0,
        sd = 0.15
      )
    U_i <-
      runif(
        N,
        0,
        1
      )
    Ux_i <-
      runif(
        N,
        0,
        1
      )
    x1_i <- (Ux_i >= 0.3)
    x2_i <- (Ux_i >= 0.7)
    tau_i <-
      rnorm(
        N,
        mean = mean_tau_i,
        sd = sd_tau_i
      )

    group_i <- 0
    g5_i <- (
      U_i <= exp(-0.25) / (1 + exp(-0.25))
    )
    group_i <-
      group_i +
      g5_i * (T0 + 1)

    g6_i <- (
      (
        U_i > exp(-0.25) / (1 + exp(-0.25))
      ) & (
        U_i <= exp(-0.25 + 0.75 * (x1_i + 0)) / (1 + exp(-0.25 + 0.75 * (x1_i + 0)))
      )
    )
    group_i <-
      group_i +
      g6_i * (T0 + 2)

    g7_i <- (
      (
        U_i > exp(-0.25 + 0.75 * (x1_i + 0)) / (1 + exp(-0.25 + 0.75 * (x1_i + 0)))
      ) & (
        U_i <= exp(-0.25 + 0.75 * (x1_i + x2_i)) / (1 + exp(-0.25 + 0.75 * (x1_i + x2_i)))
      )
    )
    group_i <-
      group_i +
      g7_i * (T0 + 3)

    g8_i <- (
      (
        U_i > exp(-0.25 + 0.75 * (x1_i + x2_i)) / (1 + exp(-0.25 + 0.75 * (x1_i + x2_i)))
      ) & (
        U_i <= exp(-0.25 + 1 * (x1_i + x2_i)) / (1 + exp(-0.25 + 1 * (x1_i + x2_i)))
      )
    )
    group_i <-
      group_i +
      g8_i * (T0 + 4)

    group_i <-
      group_i +
      (1 - g5_i - g6_i - g7_i - g8_i) * T
    g10_i <- (
      group_i == 10
    )

    for (
      t in seq(1, T)
    ) {
      time <- rep(t, N)
      if (diff_trend) {
        b_t <-
          (t / T) * (1 - x1_i - x2_i) +
          slope_x1 * (t / T) * x1_i +
          slope_x2 * (t / T) * x2_i
      } else {
        b_t <- t / T
      }
      e_it <-
        rnorm(
          N,
          mean = 0,
          sd = 0.03
        )
      tau_t <-
        runif(
          1,
          min = 0.9,
          max = 1.1
        )
      tau_it <-
        tau_t * (
          scale_5 * abs(tau_i) * g5_i +
          scale_6 * abs(tau_i) * g6_i +
          scale_7 * abs(tau_i) * g7_i +
          scale_8 * abs(tau_i) * g8_i
        )

      z_it <- rep(0, N)
      if (t >= T0 + 1) {
        z5_it <- g5_i
      } else {
        z5_it <- rep(0, N)
      }
      if (t >= T0 + 2) {
        z6_it <- g6_i
      } else {
        z6_it <- rep(0, N)
      }
      if (t >= T0 + 3) {
        z7_it <- g7_i
      } else {
        z7_it <- rep(0, N)
      }
      if (t >= T0 + 4) {
        z8_it <- g8_i
      } else {
        z8_it <- rep(0, N)
      }
      if (t == T) {
        z10_it <- g10_i
      } else {
        z10_it <- rep(0, N)
      }


      z_it <-
        z5_it +
        z6_it +
        z7_it +
        z8_it +
        z10_it
      if (t == T) {
        z_it <- 1
      }

      y0_it <-
        a0 +
        (g5_i + g6_i + g7_i + g8_i) * (-a_i) +
        (1 - g5_i - g6_i - g7_i - g8_i) * (a_i) +
        b_t +
        e_it
      y_it <-
        y0_it +
        z_it * tau_it

      df_period <-
        tibble::tibble(
          id = id,
          x1_i = x1_i,
          x2_i = x2_i,
          group_i = group_i,
          g5_i = g5_i,
          g6_i = g6_i,
          g7_i = g7_i,
          g8_i = g8_i,
          g10_i = g10_i,
          time = time,
          tau_it = tau_it,
          tau_t = rep(
                    tau_t,
                    N
                  ),
          z_it = z_it,
          z5_it = z5_it,
          z6_it = z6_it,
          z7_it = z7_it,
          z8_it = z8_it,
          z10_it = z10_it,
          y0_it = y0_it,
          y_it = y_it
        )
      if (t == 1) {
        df_panel <- df_period
      } else {
        df_panel <-
          rbind(
            df_panel,
            df_period
          )
      }
    }
    return(df_panel)
  }

#' @export
call_synthetic_control <-
  function(
    data,
    name_treated,
    year_start,
    window_income_age,
    window_beer,
    window_cigsale1,
    window_cigsale2,
    window_cigsale3,
    margin_ipop,
    sigf_ipop,
    bound_ipop
  ) {
    out <-
      data %>%
      tidysynth::synthetic_control(
        outcome = cigsale, # アウトカムの指定
        unit = state, # 個体を識別する変数
        time = year, # 期間を識別する変数
        i_unit = name_treated, # 処置個体の名前
        i_time = year_start, # 処置が発生した時点
        generate_placebos = TRUE # プラセボ分析の準備を行うか否か
      ) %>%
      tidysynth::generate_predictor(
        time_window = window_income_age,
        ln_income = mean(lnincome, na.rm = T),
        youth = mean(age15to24, na.rm = T)
      ) %>%
      tidysynth::generate_predictor(
        time_window = window_beer,
        beer_sales = mean(beer, na.rm = T)
      ) %>%
      tidysynth::generate_predictor(
        time_window = window_cigsale1,
        cigsale_1 = cigsale
      ) %>%
      tidysynth::generate_predictor(
        time_window = window_cigsale2,
        cigsale_2 = cigsale
      ) %>%
      tidysynth::generate_predictor(
        time_window = window_cigsale3,
        cigsale_3 = cigsale
      ) %>%
      tidysynth::generate_weights(
        optimization_window = window_fit,
        margin_ipop = margin_ipop,
        sigf_ipop = sigf_ipop,
        bound_ipop = bound_ipop
      ) %>%
      tidysynth::generate_control()
    return(out)
  }
