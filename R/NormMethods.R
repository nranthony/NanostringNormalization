
LaneSumNorm <- function(df) {
  norm_df <- df |>
    dplyr::mutate(dplyr::across(tidyr::contains(".RCC"), function(x) (x/sum(x))))
  return(norm_df)
}

