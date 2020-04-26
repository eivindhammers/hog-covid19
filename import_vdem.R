vdem <- fread("V-Dem-CY-Full+Others-v10.csv") %>%
  tibble(.) %>%
  select(country_name, COWcode, year, v2exfemhog, v2x_libdem)

saveRDS(vdem, "vdem.rds")
