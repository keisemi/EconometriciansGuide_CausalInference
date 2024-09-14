#' @export
generate_data_randomized <-
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
        y = 
          y_0 * 
          (1 - z) + 
          y_1 * 
          z
      ) %>%
      dplyr::select(
        -y_0,
        -y_1
      )
    return(data_realized)
  }

#' @export
calculate_difference_in_means <-
  function(
    data_realized
  ) {
    t <-
      data_realized %>%
      dplyr::summarise(
        y = 
          sum(y * z) / 
          sum(z) - 
          sum(y * (1 - z))
          / sum(1 - z)
      ) %>%
      dplyr::pull(y)
    return(t)
  }

#' @export
generate_data_stratified <-
  function(
    outcome_potential,
    N_1,
    seed
  ) {
    set.seed(seed)
    data_realized <-
      outcome_potential %>%
      dplyr::group_by(g) %>%
      dplyr::mutate(
        z = 
          (
            1:length(g) %in% 
            sample(
              length(g), 
              N_1[unique(g)]
            )
          ) %>%
          as.integer(),
        y = 
          y_0 * (1 - z) + 
          y_1 * z
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        -y_0,
        -y_1
      )
    return(data_realized)
  }

#' @export
calculate_difference_in_means_stratified <-
  function(
    data_realized,
    lambda
  ) {
    t <-
      data_realized %>%
      dplyr::group_by(g) %>%
      dplyr::summarise(
        y = 
          sum(y * z)
          / sum(z) - 
          sum(y * (1 - z)) /
          sum(1 - z)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::summarise(
        y = 
          sum(
            lambda[g] * y
            )
      ) %>%
      dplyr::pull(y)
    return(t)
  }

#' @export
generate_data_cluster <-
  function(
    outcome_potential,
    G_1,
    seed
  ) {
    set.seed(seed)
    z <- 
      sample(
        unique(
          outcome_potential$g
        ), 
        G_1
      )
    data_realized <-
      outcome_potential %>%
      dplyr::group_by(g) %>%
      dplyr::mutate(
        z = 
          g %in% z %>% 
          as.integer(),
        y = 
          y_0 * (1 - z) + 
          y_1 * z
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        -y_0,
        -y_1
      )
    return(data_realized)
  }
