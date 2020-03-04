library(inventory)

source(here::here("data-raw", "drop_zero_emissions.R"))

import_annual_data_ <- function (
  ...,
  verbose = getOption("verbose")
) {

  csv_path <-
    here::here(
      "data-raw",
      ...)

  csv_data <-
    csv_path %>%
    read_csv(
      verbose = verbose) %>%
    ensure(
      all_true(.$season == "Annual")) %>%
    select(
      -season,
      -cat_type)

  tidied_data <-
    csv_data %>%
    mutate_at(
      vars(year),
      ~ CY(.)) %>%
    mutate_at(
      vars(cat_id),
      ~ as.integer(.)) %>%
    rename(
      CO2_bio = BCO2,
      `HFC+PFC` = HFC,
      SO2 = SOx) %>%
    gather(
      pol_abbr,
      ems_qty,
      PM, TOG, NOx, SO2, CO, CO2, CH4, N2O, `HFC+PFC`, CO2_bio, SF6) %>%
    mutate(
      pol_abbr = as.character(pol_abbr),
      ems_unit = "ton/day") %>%
    select(
      year,
      cat_id,
      pol_abbr,
      ems_qty,
      ems_unit)

  validated_data <-
    tidied_data %>%
    ensure_distinct(
      year,
      cat_id,
      pol_abbr) %>%
    ensure(
      is.integer(.$cat_id)) %>%
    ensure(
      all_true(.$year %in% as.character(CY(1990:2030))))

  filtered_data <- local({

    grouped_data <-
      validated_data %>%
      group_by(
        cat_id,
        #cnty_abbr,
        pol_abbr)

    pb <- progress_estimated(n_groups(grouped_data))

    grouped_data %>%
      group_map(
        drop_zero_emissions,
        keep = TRUE,
        .pb = pb) %>%
      bind_rows()

  })

  return(filtered_data)

}

BY2008_P_data <-
  import_annual_data_(
    "BY2008_Pointtpd.csv") %>%
  mutate(
    cat_id = str_c("P", cat_id))

BY2008_A_data <-
  import_annual_data_(
    "BY2008_Areatpd.csv") %>%
  mutate(
    cat_id = str_c("A", cat_id))

BY2008_annual_emission_data <-
  bind_rows(
    BY2008_P_data,
    BY2008_A_data) %>%
  convert_units(
    from = "ton/day",
    to = "ton/yr") %>%
  select(
    year,
    cat_id,
    pol_abbr,
    ems_qty,
    ems_unit) %>%
  with_comment(
    "BY2008 area source emissions, by category, 1990—2030.")

usethis::use_data(
  BY2008_annual_emission_data,
  overwrite = TRUE)

delayedAssign(
  "BY2008_annual",
  {
    warning("`BY2008_annual` is deprecated. Please use `BY2008_annual_emission_data` instead.")
    get("BY2008_annual_emission_data")
  })

save(
  BY2008_annual,
  file = here::here("data", "BY2008_annual.rda"))