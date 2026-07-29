###cpi_uncertainty

library(tidyverse)
library(countrycode)
library(haven)
library(readxl)
library(janitor)
library(stringr)

set.seed(313)

cpi_95_19_update <- readRDS("data/cpi95_19_update.rds")
cpi_2012error <- read_excel("data/CPI2019.xlsx", sheet ="CPI Timeseries 2012 - 2019")

cpi_2012error_clean <- cpi_2012error %>%
  row_to_names(row_number = 2) %>%
  select(-contains("Rank"),-contains("Source"),-Region, -contains("CPI"),-ISO3) %>%
  rename(country = Country) %>%
  gather("year", "cpi_error", 2:9) %>%
  mutate(year = str_replace_all(year, "[A-Za-z,]",""),
         year = as.numeric(year),
         cpi_error = as.numeric(cpi_error)) %>%
  mutate(ISO = countrycode(country, origin = 'country.name', destination = 'iso3c')) %>%
  mutate(country_name = countrycode::countrycode(ISO, "iso3c","country.name")) %>%
  mutate(country_name = ifelse(country == "Kosovo","Kosovo",
                               ifelse(country == "Yugoslavia","Yugoslavia",country_name))) %>%
  rename(country_text = country,
         country = country_name) %>%
  select(-country_text,-ISO)

cpi_95_19_error <- left_join(cpi_95_19_update,cpi_2012error_clean, by= c("country","year"))

cpi_95_19_error <- cpi_95_19_error %>%
  mutate(relative_error = cpi_error/cpi_100) %>%
  select(-cpi) %>%
  group_by(country) %>%
  mutate(h_re = max(relative_error,na.rm = T),
         h_re = ifelse(h_re =="-Inf",NA,h_re)) %>%
  ungroup() %>%
  mutate(cpi_error = ifelse(is.na(cpi_error), cpi_100*h_re,cpi_error)) %>%
  group_by(country) %>%
  mutate(cpi_100_m1 = lag(cpi_100)) %>%
  arrange(country,year)

cpi_pdist <- cpi_95_19_error %>%
  select(cpi_100,cpi_error) %>%
  pmap(function(cpi_100,cpi_error,...) rnorm(900, cpi_100,cpi_error))  #posterior distribution 900 draws

cpi_pdist_df <- cpi_pdist %>% 
  map_df(as_tibble)

cpi_pdist_df$iter_se <- rep(1:900, length.out=nrow(cpi_pdist_df)) 

cpi_95_19_error_900 <- cpi_95_19_error %>% group_by(country, year) %>% 
  expand(iter_cn = 1:900)  %>%
  group_by(country,year) %>% 
  arrange(iter_cn,.by_group = TRUE)  #arrange by country year iter

cpi_95_19_error_900 <- data.frame(cpi_95_19_error_900)
cpi_pdist_df <- data.frame(cpi_pdist_df)

cpi_se <- cbind(cpi_95_19_error_900, cpi_pdist_df)
cpi_se <- cpi_se %>%
  rename(cpi_post = value) %>%
  mutate(cpi_post = ifelse(is.nan(cpi_post),NA,cpi_post))

cpi <- left_join(cpi_se, cpi_95_19_error,by =c("country","year"))

cpi_clean <- cpi %>%
  select(-h_re,-relative_error) %>%
  mutate(cpi_post = cpi_post*-1,
        cpi_z = as.vector(scale(cpi_100*-1)), # reversed, positive--more corruption
         cpi_m1_z = as.vector(scale(cpi_100_m1*-1)), 
         cpi_post_z = as.vector(scale(cpi_post))) %>%
  group_by(country, iter_se) %>%
  arrange(year, .by_group = TRUE) %>% 
  mutate(cpi_postm1 = lag(cpi_post),
         cpi_postz_m1 = lag(cpi_post_z))

saveRDS(cpi_clean, "data/cpi_post.rds")
