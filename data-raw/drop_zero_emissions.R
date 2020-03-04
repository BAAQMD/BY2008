drop_zero_emissions <- function (ems_data, ..., .pb = NULL) {

  # update progress bar
  if ((!is.null(.pb)) && (.pb$i < .pb$n)) .pb$tick()$print()

  all_zero <-
    funtools::all_true(
      dplyr::pull(ems_data, ems_qty) == 0)

  if (all_zero) {
    return(NULL)
  }

  return(ems_data)

}
