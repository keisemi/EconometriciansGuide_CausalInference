#' @export
generate_data_noncompliance_oneside <-
  function(
    outcome_potential,
    N_1,
    seed,
    exclusion
  ) {
    set.seed(seed)
    N <- nrow(outcome_potential)
    data_realized <-
      outcome_potential %>%
      dplyr::mutate(
        z = 
          (
            1:N %in% 
            sample(
              N, 
              N_1
            )
          ) %>%
          as.integer(),
        d = 
          ifelse(
            g == "nc", 
            0, 
            z
          ),
        y_1 = 
          y_1 + 
          (1 - exclusion) * y_z,
        y = 
          y_0 * (1 - d) + 
          y_1 * d
      ) %>%
      dplyr::select(
        -y_0,
        -y_1,
        -g
      )
    return(data_realized)
  }

#' @export
generate_data_noncompliance_twoside <-
  function(
    outcome_potential,
    N_1,
    seed
  ) {
    set.seed(seed)
    N <- nrow(outcome_potential)
    data_realized <-
      outcome_potential %>%
      dplyr::mutate(
        z = 
          (
            1:N %in% 
            sample(
              N, 
              N_1
              )
          ) %>%
          as.integer(),
        d = ifelse(
          g == "co",
          z,
          ifelse(
            g == "df",
            1 - z,
            ifelse (
              g == "at",
              1,
              0
            )
          )
        ),
        y = 
          y_0 * (1 - d) + 
          y_1 * d
      ) %>%
      dplyr::select(
        -y_0,
        -y_1,
        -g
      )
    return(data_realized)
  }

