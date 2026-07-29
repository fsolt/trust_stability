###Posterior distribution
library(tidyverse)
library(dplyr)
library(tidyr)

set.seed(313)

cntrl_ajps_se <- read.csv("data/cntrl_ajps_se_1985.csv",encoding = "UTF-8",stringsAsFactors  = F)


##libdem
pdist <- cntrl_ajps_se %>%
  select(Vdem_libdem,libdem_sd) %>%
pmap(function(Vdem_libdem,libdem_sd,...) rnorm(900, Vdem_libdem, libdem_sd))  #posterior distribution 900 draws

pdist_df <- pdist %>% 
  map_df(as_tibble)

pdist_df$iter_se <- rep(1:900, length.out=nrow(pdist_df)) 
pdist_df <- data.frame(pdist_df)

cntrl_ajps_se_pdist <- cntrl_ajps_se %>% group_by(country, year) %>% 
  expand(iter_cn = 1:900)  %>%
  group_by(country,year) %>% 
  arrange(iter_cn,.by_group = TRUE)  #arrange by country year iter

cntrl_ajps_se_pdist <- data.frame(cntrl_ajps_se_pdist)

cntrl_ajps_postse <- cbind(cntrl_ajps_se_pdist, pdist_df)
identical(cntrl_ajps_postse$iter_cn,cntrl_ajps_postse$iter_se)
cntrl_ajps_postse <- cntrl_ajps_postse %>%
  rename(Vdem_libdem_post = value)

saveRDS(cntrl_ajps_postse, "data/cntrl_ajps_libdempost.rds")


###polyarchi
pdist_poly <- cntrl_ajps_se %>%
  select(Vdem_polyarchy,polyarchy_sd) %>%
  pmap(function(Vdem_polyarchy,polyarchy_sd,...) rnorm(900, Vdem_polyarchy,polyarchy_sd))  #posterior distribution 900 draws

pdist_poly_df <- pdist_poly %>% 
  map_df(as_tibble)

pdist_poly_df$iter_se <- rep(1:900, length.out=nrow(pdist_poly_df)) 

pdist_poly_df <- data.frame(pdist_poly_df)

cntrl_ajps_poly_postse <- cbind(cntrl_ajps_se_pdist, pdist_poly_df)
identical(cntrl_ajps_poly_postse$iter_cn,cntrl_ajps_poly_postse$iter_se)
cntrl_ajps_poly_postse <- cntrl_ajps_poly_postse %>%
  rename(Vdem_polyarchy_post = value)

saveRDS(cntrl_ajps_poly_postse, "data/cntrl_ajps_polypost.rds")

cntrl_ajps_error <- left_join(cntrl_ajps_postse,cntrl_ajps_poly_postse, by = c("country","year","iter_cn","iter_se"))

cntrl_ajps_uncertainty <- left_join(cntrl_ajps_error,cntrl_ajps_se, by = c("country", "year")) %>%
  select(country, year, starts_with("Vdem"),everything())

libdem_cntrl_list

cntrl_ajps_uncertainty_region <- cntrl_ajps_uncertainty %>%
  group_by(country, iter_se) %>%
  arrange(year, .by_group = TRUE) %>% 
  mutate(Vdem_libdem_postm1 = lag(Vdem_libdem_post),
        Vdem_libdem_postm2 = lag(Vdem_libdem_post,2)) %>%
  ungroup() %>%
  mutate(Chgdem_post = Vdem_libdem_post - Vdem_libdem_postm1,
         upchgdem_post = ifelse(Chgdem_post > 0, Chgdem_post,0),
         downchgdem_post = ifelse(Chgdem_post < 0, Chgdem_post*(-1),0)) %>%
  ungroup() %>%
  group_by(Region_UN,year,iter_cn) %>%
  mutate(Region_libdem_post = mean(Vdem_libdem_post, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(country,iter_cn) %>%
  arrange(year, .by_group = TRUE) %>% 
  mutate(Region_libdem_postm1 = lag(Region_libdem_post))  %>%
  ungroup() %>%
  group_by(Region_UN,year, iter_cn)  %>%
  select(Region_UN, year, country, iter_se, Region_libdem_post,Region_libdem_postm1, starts_with("Vdem"),everything())

saveRDS(cntrl_ajps_uncertainty_region, "data/cntrl_ajps_uncertainty.rds")



