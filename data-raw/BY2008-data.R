library(inventory)

prj_path <- function (...) {
  file.path(rprojroot::find_root("DESCRIPTION"), ...)
}

import_annual_data_ <- function (...) {
  prj_path("data-raw", ...) %>%
    read_csv(col_types = "icicdddddddddd") %>%
    ensure(all_true(.$season == "Annual")) %>%
    select(-season, -cat_type) %>%
    rename(CO2_bio = BCO2, `HFC+PFC` = HFC, SO2 = SOx) %>%
    ensure(min(.$year) == 1990) %>%
    ensure(max(.$year) == 2030) %>%
    ensure_distinct(year, cat_id) %>%
    gather(pol_abbr, ems_qty, PM, TOG, NOx, SO2, CO, CO2, CH4, N2O, `HFC+PFC`, CO2_bio, SF6) %>%
    mutate(ems_qty = parse_double(ems_qty)) %>%
    filter(ems_qty > 0) %>%
    mutate(pol_abbr = as.character(pol_abbr), ems_unit = "tons/day") %>%
    select(year, cat_id, pol_abbr, ems_qty, ems_unit) %>%
    ensure_distinct(year, cat_id, pol_abbr) %>%
    ensure(is.integer(.$cat_id))
}

BY2008_P_data <-
  import_annual_data_("BY2008_Pointtpd.csv") %>%
  mutate(cat_id = str_c("P", cat_id))

BY2008_A_data <-
  import_annual_data_("BY2008_Areatpd.csv") %>%
  mutate(cat_id = str_c("A", cat_id))

BY2008_annual <-
  bind_rows(BY2008_P_data, BY2008_A_data) %>%
  convert_units(from = "tons/day", to = "tons/yr") %>%
  select(year, cat_id, pol_abbr, ems_qty, ems_unit) %>%
  with_comment("BY2008 area source emissions, by category, 1990—2030.")

# Save the datasets to the same .Rda file
devtools::use_data(
  BY2008_annual,
  overwrite = TRUE)
