####R version 4.1.1 (2021-08-10)

#### Input: Muslims Population: muslism_prop_2010.csv from http://globalreligiousfutures.org/
####        Dependence on natural resource: WDIData.csv from WDI
####                                        Haber_Menaldo_2011_APSR_Dataset.xls from Haber_Menaldo(2011) 
####        GDP Data: WDIData.csv from WDI2020,
####                  WEOApr2020all.csv from IMF 
####                  pwt100.dta from World Table Penn World Table Version.10 
####        Measures of Public Support: claassen_replication_input.rda  
####                                    exp_claassen_input.rda
####                                    dcpo_input_update22.rda
#### Output:control_variable.csv, cls_cntrl.rda,exp_cntrl.rda, dcpo_cntrl.rda

###Load packages
if (!require(pacman)) install.packages("pacman")
library(pacman)
p_load(
  tidyverse,
  countrycode,
  readxl,
  here,
  mi,
  mitools
) 

set.seed(313)


#Get Muslim Population data
muslism_prop_2010 <- read.csv(here("data","muslism_prop_2010.csv")) %>%
  transmute(country_code = gsub( "\"|\"", "", iso_code),
            country_name = gsub( "\"|\"", "", name), 
            muslism_prop_2010 = as.numeric(gsub( "<|>|%|\"|\"", "", value)))  %>%
  mutate(country_name = trimws(country_name),
         country_code = countrycode::countrycode(country_code, origin = 'iso2c', destination = 'iso3c'),
         country_code = ifelse(country_name == "Kosovo", "XKX", 
                               ifelse(country_name == "Namibia", "NAM", country_code))) %>%
  filter(!is.na(country_code))

#IMF: GDP
gdp <- read.csv(here("data","WEOApr2020all.csv")) %>%
  filter(WEO.Subject.Code	== "PPPGDP") %>%
  select(ISO, Country, X1980:X2019)
colnames(gdp)[3:42] <- c("1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999",
                         "2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011",
                         "2012","2013","2014","2015","2016","2017","2018","2019")                            

long_gdp <- gdp %>% 
  gather(year, gdp, "1980":"2019") %>% 
  mutate(year = as.numeric(year),
         gdp = as.numeric(str_replace(gdp,",","")),
         Country = ifelse(ISO =="STP","São Tomé & Príncipe",Country),
         country_code = countrycode(Country, origin = 'country.name', destination = 'iso3c'),
         country_code = ifelse(Country == "Kosovo", "XKX", country_code))

####WB: GDP and Dependence

wdi_ind <- read.csv(here("data","WDIData.csv")) %>%
  filter(Indicator.Code == "NY.GDP.PCAP.PP.CD"|Indicator.Code =="SP.POP.TOTL"|Indicator.Code =="NY.GDP.TOTL.RT.ZS") %>%
  select(-c("Indicator.Name","Country.Code")) %>%
  rename(country = names(.[1])) %>%
  select(-X)



names(wdi_ind) <- gsub(names(wdi_ind), pattern = "X", replacement = "")  
  
wdi_ind_19602019  <- wdi_ind %>%
  gather(year, index, 3:62)  %>% 
  mutate(indicator = ifelse(Indicator.Code == "NY.GDP.PCAP.PP.CD", "wdi_gdppc",
                                 ifelse(Indicator.Code == "SP.POP.TOTL","pop",
                                        ifelse(Indicator.Code == "NY.GDP.TOTL.RT.ZS","dependence",NA)))) %>%
  select(-Indicator.Code) %>%
  spread(indicator, index) %>%
  mutate(ISO = countrycode(country, origin = 'country.name', destination = 'iso3c')) %>%
  mutate(pop = as.numeric(pop),
         year= as.numeric(year),
         dependence = as.numeric(dependence)) %>% 
  mutate(ISO = ifelse(country == "Kosovo", "XKX", ISO)) %>% 
  rename(country_code = ISO) %>% 
  filter(!is.na(country_code))

### Penn World Table: GDP

pwt100_gdp <- rio::import(here("data","pwt100.dta"))  %>%
  select(countrycode, country, year, pop, rgdpna) %>%
  rename(pen_gdp = rgdpna,
         country_name = country,
         pen_pop = pop)  %>%
  mutate(country_code = countrycode(country_name,  origin = 'country.name','iso3c'))

#### Haber and Menaldo (2011): Dependence

resource_dep <- read_excel(here("data","Haber_Menaldo_2011_APSR_Dataset.xls"), 
                           sheet = "Haber_Menaldo_2011_APSR_") %>%
  mutate( cnamehabmen = ifelse(cnamehabmen == "Pakisan", "Pakistan",cnamehabmen),
          country = countrycode(cnamehabmen, origin = 'country.name', destination = 'country.name'),) %>%
  select("country", "year", "Total_Resources_Income_PC") %>%
  rename (dependence_pc_hm = Total_Resources_Income_PC)  # Tanzania was input twice.

####MERGING regionUN, other variables
load(here("data/country_regionUN.rda"))

muslism_cnty <- na.omit(muslism_prop_2010$country_code)
cntry_year <- data.frame(country_code = rep(muslism_cnty, each = 61), year = rep((1960:2020),times = 234)) %>%
  mutate(country = countrycode(country_code,'iso3c','country.name'), 
         country = ifelse(country_code == "XKX", "Kosovo", country)) %>%
  distinct(country, country_code, year)

control_variable <- left_join(cntry_year,country_regionUN, by=c("country")) %>%
                    left_join( muslism_prop_2010, by="country_code") %>%
                    distinct() %>%
                    left_join(long_gdp %>% select(country_code,year,gdp), by = c("country_code","year")) %>%
                    left_join( wdi_ind_19602019 %>% select(-country), by = c("country_code","year"))  %>%
                    left_join(pwt100_gdp %>% select(country_code, year,pen_pop,pen_gdp), by= c("country_code","year")) %>%
                    mutate(gdp_percapita = (gdp*1000000000)/pop,
                          lggdp_percap = log(gdp_percapita),
                          pen_gdp_pc = pen_gdp/pen_pop ) %>%
                    rename(wb_pop = pop,
                           imf_gdp = gdp,
                           imf_gdp_pc = gdp_percapita,
                           imflg_gdppc = lggdp_percap,
                           wb_gdp_pc = wdi_gdppc)  %>%
                  left_join(resource_dep, by = c("country","year")) %>%
                  distinct()  %>%
                  mutate(dep_wdi_pc = (dependence/100)*wb_gdp_pc,
                         dep_pc_di = ifelse(dep_wdi_pc < 1000, 0, 1),
                         dep_hm_di = ifelse(dependence_pc_hm < 1000, 0, 1)) %>%
                         select(-imflg_gdppc)  #clean unused variabls

write.csv(control_variable,here("data","control_variable.csv"),fileEncoding = "UTF-8",row.names=FALSE)


######Impute GDP and dependence data
#control_variable <- read.csv(here("data","control_variable.csv"))

mdf <-  control_variable %>% 
  filter(year > 1985) %>%
  select(country,year,wb_gdp_pc, imf_gdp_pc, pen_gdp_pc) %>%  
  missing_data.frame()

show(mdf)
mdf <- change(mdf, y = c("year"), what = "type", to = "positive-continuous")
mdf <- change(mdf, y = c("wb_gdp_pc","imf_gdp_pc", "pen_gdp_pc"), what = "type", to = "positive-continuous")

mdf_mi <- mi(mdf, seed = 313) 
mdf_mi_list <- complete(mdf_mi, m= 900)
mdf_mi_list <- lapply(mdf_mi_list, function(df) 
  sapply(df, function(v) 
    if(any(class(v)=="factor")) v <- as.numeric(levels(v))[v] else v <- v) %>%
    data.frame) 
mdf_mi_list <- imputationList(mdf_mi_list)


mdf_dep <-  control_variable %>% 
  filter(year > 1985) %>%
  select(dep_pc_di,dep_hm_di) %>%  
  missing_data.frame()

show(mdf_dep)
mdf_dep <- change(mdf_dep, y = c("dep_pc_di","dep_hm_di"), what = "type", to = "binary")

mdf_dep_mi <- mi(mdf_dep, seed = 313) 
mdf_dep_mi_list <- complete(mdf_dep_mi, m= 900)
mdf_dep_mi_list <- lapply(mdf_dep_mi_list, function(df) 
  sapply(df, function(v) 
    if(any(class(v)=="factor")) v <- as.numeric(levels(v))[v] else v <- v) %>%
    data.frame) 
mdf_dep_mi_list <- imputationList(mdf_dep_mi_list)


#### Attain first year for correct data and expanded data
load(here::here("data","claassen_replication_input.rda"))

cls_cntry_firstyr <- claassen_replication_input[["data"]] %>% 
  select(c("country","year")) %>%
  unique()  %>% #1166
  group_by(country) %>%
  transmute(country = country,
            firstyear = min(year)) %>%
  unique() #137

cls_cntrl <- purrr::map(1:900, function(anEntry) {
  mdf_mi_list[["imputations"]][[anEntry]] %>% 
    select(wb_gdp_pc) %>% 
    cbind(control_variable %>% 
            filter( year > 1985) %>% 
            select(country_code,year,country,Region_UN,muslism_prop_2010)) %>%
    cbind(mdf_dep_mi_list[["imputations"]][[anEntry]] %>% select(dep_pc_di)) %>%
    left_join(cls_cntry_firstyr, by = "country") %>%
    rename(Res_cp_WDI_di = dep_pc_di, 
           Pr_Muslim = muslism_prop_2010) %>%
    mutate(lnGDP_imp = log(wb_gdp_pc)) %>%
    group_by(country) %>%
    mutate(lnGDP_imp_lag = dplyr::lag(lnGDP_imp,order_by = year),
           GDP_imp_grth = (lnGDP_imp - lnGDP_imp_lag)*100, 
           GDP_imp_grth_lag1 = dplyr::lag(GDP_imp_grth,order_by = year),
           Res_cp_WDI_di_lag1 = dplyr::lag(Res_cp_WDI_di,order_by = year)
    ) %>% 
    select(country,year, country_code,Region_UN,firstyear,lnGDP_imp,lnGDP_imp_lag,GDP_imp_grth,GDP_imp_grth_lag1,
           Pr_Muslim,Res_cp_WDI_di,Res_cp_WDI_di_lag1)
  
})

save(cls_cntrl,file = here("data","cls_cntrl.rda"))


####expanded first year
load(here("data","exp_claassen_input.rda"))

exp_cntry_firstyr <- exp_claassen_input[["data"]] %>% 
  select(c("country","year")) %>%
  unique()  %>% #1443
  group_by(country) %>%
  transmute(country = country,
            firstyear = min(year)) %>%
  unique() #145

exp_cntrl <- purrr::map(1:900, function(anEntry) {
  mdf_mi_list[["imputations"]][[anEntry]] %>% 
    select(wb_gdp_pc) %>% 
    cbind(control_variable %>% 
            filter( year > 1985) %>% 
            select(country_code,year,country,Region_UN,muslism_prop_2010)) %>%
    cbind(mdf_dep_mi_list[["imputations"]][[anEntry]] %>% select(dep_pc_di)) %>%
    left_join(exp_cntry_firstyr, by = "country") %>%
    rename(Res_cp_WDI_di = dep_pc_di, 
           Pr_Muslim = muslism_prop_2010) %>%
    mutate(lnGDP_imp = log(wb_gdp_pc)) %>%
    group_by(country) %>%
    mutate(lnGDP_imp_lag = dplyr::lag(lnGDP_imp,order_by = year),
           GDP_imp_grth = (lnGDP_imp - lnGDP_imp_lag)*100, 
           GDP_imp_grth_lag1 = dplyr::lag(GDP_imp_grth,order_by = year),
           Res_cp_WDI_di_lag1 = dplyr::lag(Res_cp_WDI_di,order_by = year)
    ) %>% 
    select(country,year, country_code,Region_UN,firstyear,lnGDP_imp,lnGDP_imp_lag,GDP_imp_grth,GDP_imp_grth_lag1,
           Pr_Muslim,Res_cp_WDI_di,Res_cp_WDI_di_lag1)
  
})

save(exp_cntrl,file = here("data","exp_cntrl.rda"))


#####DCPO first year and control
load(here("data","dcpo_input_update22.rda"))


min(dcpo_input_update22[["data"]]$year) #1988
max(dcpo_input_update22[["data"]]$year) #2020
length(unique(dcpo_input_update22[["data"]]$country))   #148

dcpo_cntry_firstyr <- dcpo_input_update22[["data"]] %>%
  select(c("country","year")) %>%
  unique()  %>% 
  group_by(country) %>%
  transmute(country = country,
            firstyear = min(year)) %>%
  unique() 


dcpo_cntrl <-  purrr::map(1:900, function(anEntry) {
  mdf_mi_list[["imputations"]][[anEntry]] %>% 
    select(wb_gdp_pc) %>% 
    cbind(control_variable %>% 
            filter( year > 1985) %>% 
            select(country_code,year,country,Region_UN,muslism_prop_2010)) %>%
    cbind(mdf_dep_mi_list[["imputations"]][[anEntry]] %>% select(dep_pc_di)) %>%
    left_join(dcpo_cntry_firstyr, by = "country") %>%
    rename(Res_cp_WDI_di = dep_pc_di, 
           Pr_Muslim = muslism_prop_2010) %>%
    mutate(lnGDP_imp = log(wb_gdp_pc)) %>%
    group_by(country) %>%
    mutate(lnGDP_imp_lag = dplyr::lag(lnGDP_imp,order_by = year),
           GDP_imp_grth = (lnGDP_imp - lnGDP_imp_lag)*100, 
           GDP_imp_grth_lag1 = dplyr::lag(GDP_imp_grth,order_by = year),
           Res_cp_WDI_di_lag1 = dplyr::lag(Res_cp_WDI_di,order_by = year)
    ) %>% 
    select(country,year, country_code,Region_UN,firstyear,lnGDP_imp,lnGDP_imp_lag,GDP_imp_grth,GDP_imp_grth_lag1,
           Pr_Muslim,Res_cp_WDI_di,Res_cp_WDI_di_lag1)
  
})

save(dcpo_cntrl,file = here("data","dcpo_cntrl.rda"))


