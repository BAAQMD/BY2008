library(inventory)

import_annual_data_ <- function (...) {

  csv_path <-
    here::here(
      "data-raw",
      ...)

  csv_data<-
    csv_path %>%
    read_csv(
      col_types = "icicdddddddddd") %>%
    ensure(
      all_true(.$season == "Annual")) %>%
    select(
      -season,
      -cat_type) %>%
    ensure(
    min(.$year) == 1990) %>%
    ensure(
      max(.$year) == 2030) %>%
    ensure_distinct(
      year,
      cat_id)

  tidied_data <-
    csv_data %>%
    rename(
      CO2_bio = BCO2,
      `HFC+PFC` = HFC,
      SO2 = SOx) %>%
    gather(
      pol_abbr,
      ems_qty,
      PM, TOG, NOx, SO2, CO, CO2, CH4, N2O, `HFC+PFC`, CO2_bio, SF6) %>%
    mutate(
      ems_qty = parse_double(ems_qty)) %>%
    filter(
      ems_qty > 0) %>%
    mutate(
      pol_abbr = as.character(pol_abbr),
      ems_unit = "ton/day") %>%
    select(
      year,
      cat_id,
      pol_abbr,
      ems_qty,
      ems_unit) %>%
    ensure_distinct(
      year,
      cat_id,
      pol_abbr) %>%
    ensure(
      is.integer(.$cat_id))

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

BY2008_annual <-
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

# Save the datasets to the same .Rda file
usethis::use_data(
  BY2008_annual,
  overwrite = TRUE)
