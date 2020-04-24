library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(directlabels)
library(data.table)
library(countrycode)

setwd("~/Dropbox/Forskning/Covid-19")

vdem <- fread("data/Country_Year_V-Dem_Full+others_CSV_v10/V-Dem-CY-Full+Others-v10.csv") %>%
  tibble(.) %>%
  mutate(ccode = countrycode(COWcode, "cown", "iso3c")) %>%
  select(ccode, country_name, year, v2exfemhog, v2x_libdem) %>%
  filter(!is.na(v2exfemhog))

covid_raw <- fread("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na = "") %>%
  tibble(.)

covid <- covid_raw %>%
  mutate(dateRep = as_date(dateRep, format = "%d/%m/%Y")) %>%
  group_by(geoId) %>%
  arrange(dateRep, .by_group = TRUE) %>%
  mutate(cases_cum = cumsum(cases),
         deaths_cum = cumsum(deaths),
         cases_rollavg = rollapply(cases, 7, mean, fill = NA),
         deaths_rollavg = rollapply(deaths, 7, mean, fill = NA),
         # Determine date where cases (deaths) reached 30 (3)
         cases_30 = as.numeric(cases_rollavg >= 30),
         cases_30 = replace(cases_30, cases_30 == 0, NA),
         cases_30 = na.locf(cases_30, na.rm = FALSE),
         cases_30_date = min(dateRep[cases_30 == 1], na.rm = TRUE),
         cases_30_days = dateRep - cases_30_date,
         deaths_3 = as.numeric(deaths_rollavg >= 3),
         deaths_3 = replace(deaths_3, deaths_3 == 0, NA),
         deaths_3 = na.locf(deaths_3, na.rm = FALSE),
         deaths_3_date = min(dateRep[deaths_3 == 1], na.rm = TRUE),
         deaths_3_days = dateRep - deaths_3_date)

# Merge covid with vdem
covid_vdem <- inner_join(covid, filter(vdem, year == 2019), by = c("countryterritoryCode" = "ccode"))

# Plot rolling average cases
filter(covid_vdem, cases_30 == 1) %>%
  ggplot(aes(x = cases_30_days, y = cases_rollavg, group = geoId, color = factor(v2exfemhog))) +
  scale_y_continuous(trans = "log10", limits = c(20, NA)) +
  scale_color_discrete(name = "Head of Government", labels = c("Male", "Female")) +
  geom_line() + 
  theme_minimal() +
  theme(legend.position = c(0.2, 0.9)) +
  xlim(0, NA)

covid_vdem_cross <- covid_vdem %>%
  group_by(countriesAndTerritories, countryterritoryCode, popData2018, v2exfemhog, v2x_libdem) %>%
  summarize(deaths = sum(deaths)) %>%
  mutate(log_deaths = log(deaths + 1)) %>%
  filter(!is.na(v2exfemhog))

# Regression of log deaths on female HOG dummy
lm(log_deaths ~ v2exfemhog, data = covid_vdem_cross) %>%
  summary(.)

# Box plot of log deaths, by HOG male/female
ggplot(covid_vdem_cross) +
  geom_boxplot(aes(factor(v2exfemhog), log_deaths)) +
  theme_minimal()
