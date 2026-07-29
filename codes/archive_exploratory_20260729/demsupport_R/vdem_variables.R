
#### R version 4.1.1 (2021-08-10)
###  Input Data:vdem8_posterior.dta       from Vdem8
###             vdem10_1985.csv           from Vdem10
###             cpi_95_19_error.csv       from Corruption Perception Index 
###  Output Data:vdem8_1985_post.rds      vdem8_regime.rda
###             vdem10_1985_se_cntry.rds, vdem10_regime.rda
###             cls_libdem_list.rda       posterior distribution of V-Dem8 Liberal Democracy Index 
###             cls_poly_list.rda         posterior distribution of V-Dem8 electoral democracy index
###             cls_liberal_list.rda      posterior distribution of V-Dem8 liberal component index
###             libdem_list.rda           posterior distribution of V-Dem10 Liberal Democracy Index 
###             poly_list.rda             posterior distribution of V-Dem10 electoral democracy index
###             lib_list.rda              posterior distribution of V-Dem10 liberal component index

###Load packages
if (!require(pacman)) install.packages("pacman")
library(pacman)
p_load(
    here,
    purrr,
    countrycode,
    tidyverse,
    stringr,
    haven
) 

set.seed(313)



################################### Vdem8 variables #########################
###################################loading vdem8 posterior distribution########
########################################### ##################################

vdem8_post <- read_dta(here("data","vdem8_posterior.dta"))
vdem8_post <- vdem8_post  %>%
    mutate(vdem_polyarchy = as.numeric(vdem_polyarchy),
           vdem_libdem = as.numeric(vdem_libdem),
           vdem_region = as.numeric(vdem_region),
           vdem_regime = as.numeric(vdem_regime),
           vdem_liberal = as.numeric(vdem_liberal))

vdem8_na <- vdem8_post %>%
    filter_at(vars(starts_with("vdem_")),any_vars(is.na(.)))

################## check missing and fill up former USSR countries values
##For Armenia 1985-1989 from Russia

Armenia <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Armenia",
           country_text_id = "ARM",
           country_id = 105,
           cowcode = 371)

vdem8_post <- rbind(vdem8_post,Armenia)
vdem8_post <- vdem8_post %>%
    mutate(cowcode = ifelse(country_name == "Armenia" & year == 1990,371,cowcode ))

##For Tajikistan 1985-1989 from Russia

Tajikistan <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Tajikistan",
           country_text_id = "TJK",
           country_id = 133,
           cowcode = 702)

vdem8_post <- rbind(vdem8_post,Tajikistan)
vdem8_post <- vdem8_post %>%
    mutate(cowcode = ifelse(country_name == "Tajikistan" & year == 1990,702,cowcode ))


##Azerbaijan 1985-1989 from Russia

Azerbaijan <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Azerbaijan",
           country_text_id = "AZE",
           country_id = 106,
           cowcode = 373)

vdem8_post <- rbind(vdem8_post,Azerbaijan)


##Belarus 1985-1989 from Russia

Belarus <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Belarus",
           country_text_id = "BLR",
           country_id = 107,
           cowcode = 370)

vdem8_post <- rbind(vdem8_post,Belarus)
vdem8_post <- vdem8_post %>%
    mutate(cowcode = ifelse(country_name == "Belarus" & year == 1990,370,cowcode ))

##Kazakhstan 1985-1990

Kazakhstan <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1990)) %>%
    mutate(country_name = "Kazakhstan",
           country_text_id = "KAZ",
           country_id = 121,
           cowcode = 705)

vdem8_post <- rbind(vdem8_post,Kazakhstan)
test1 <- vdem8_post %>% 
    filter(country_name == "Kazakhstan")

vdem8_post <- vdem8_post %>%
    filter(!(country_name == "Kazakhstan" & year == 1990 & is.na(vdem_libdem)))

##Estonia  1985-1989

Estonia <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1990)) %>%
    mutate(country_name = "Estonia",
           country_text_id = "EST",
           country_id = 161,
           cowcode = 366)

vdem8_post <- rbind(vdem8_post,Estonia)

vdem8_post <- vdem8_post  %>%
    filter(!(country_name == "Estonia" & year == 1990 & is.na(vdem_libdem)))

##Georgia 1985-1989

Georgia  <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Georgia",
           country_text_id = "GEO",
           country_id = 118,
           cowcode = 372)

vdem8_post <- rbind(vdem8_post,Georgia )


##Kyrgyzstan 1985-1989

Kyrgyzstan  <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Kyrgyzstan",
           country_text_id = "KGZ",
           country_id = 122,
           cowcode = 703)

vdem8_post <- rbind(vdem8_post,Kyrgyzstan )

##Latvia 1985-1989

Latvia  <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Latvia",
           country_text_id = "LVA",
           country_id = 84,
           cowcode = 367)

vdem8_post <- rbind(vdem8_post,Latvia  )

## Lithuania 1985-1989
Lithuania    <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Lithuania",
           country_text_id = "LTU",
           country_id = 173,
           cowcode = 368)

vdem8_post <- rbind(vdem8_post,Lithuania)

## Moldova 1985-1989

Moldova    <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Moldova",
           country_text_id = "MDA",
           country_id = 126,
           cowcode = 359)

vdem8_post <- rbind(vdem8_post,Moldova)

## Ukraine  1985-1989

Ukraine    <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Ukraine",
           country_text_id = "UKR",
           country_id = 100,
           cowcode = 369)

vdem8_post <- rbind(vdem8_post,Ukraine)

##Uzbekistan 1985-1989
Uzbekistan    <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Uzbekistan",
           country_text_id = "UZB",
           country_id = 140,
           cowcode = 704)

vdem8_post <- rbind(vdem8_post,Uzbekistan)

##	Turkmenistan 1985-1990 from	Russia  1990 no demo information

Turkmenistan    <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1990)) %>%
    mutate(country_name = "Turkmenistan",
           country_text_id = "TKM",
           country_id = 136,
           cowcode = 701)

vdem8_post <- rbind(vdem8_post,Turkmenistan)

vdem8_post <- vdem8_post %>%
    filter(!(country_name == "Turkmenistan" & is.na(vdem_polyarchy)))


######add Turkmenistan

#North Macedonia 1985-1990  from Serbia 

Macedonia    <- vdem8_post %>%
    filter(country_name == "Serbia" & year %in% c(1985:1993)) %>%
    mutate(country_name = "Macedonia",
           country_text_id = "MKD",
           country_id = 176,
           cowcode = 343)

vdem8_post <- rbind(vdem8_post,Macedonia)


vdem8_post <- vdem8_post %>%
    filter(!(country_name == "Macedonia" & is.na(vdem_polyarchy)))


##Bosnia & Herzegovina 1985-1991  


BIH    <- vdem8_post %>%
    filter(country_name == "Serbia" & year %in% c(1985:1991)) %>%
    mutate(country_name = "Bosnia & Herzegovina",
           country_text_id = "BIH",
           country_id = 150,
           cowcode = 346)

vdem8_post <- rbind(vdem8_post,BIH)

vdem8_post <- vdem8_post %>%
    mutate(country_name = ifelse(country_text_id == "BIH", "Bosnia & Herzegovina", country_name))


##Croatia 1985-1990

Croatia   <- vdem8_post %>%
    filter(country_name == "Serbia" & year %in% c(1985:1990)) %>%
    mutate(country_name = "Croatia",
           country_text_id = "HRV",
           country_id = 154,
           cowcode = 344)

vdem8_post <- rbind(vdem8_post,Croatia)

##Montenegro 1985-1998   

Montenegro   <- vdem8_post %>%
    filter(country_name == "Serbia" & year %in% c(1985:1998)) %>%
    mutate(country_name = "Montenegro",
           country_text_id = "MNE",
           country_id = 183,
           cowcode = 341)

vdem8_post <- rbind(vdem8_post,Montenegro)

vdem8_post <- vdem8_post %>%
    filter(!(country_name == "Montenegro" & year == 1998 & is.na(vdem_libdem)))


#Slovenia  1985-1988  

Slovenia   <- vdem8_post %>%
    filter(country_name == "Serbia" & year %in% c(1985:1988)) %>%
    mutate(country_name = "Slovenia",
           country_text_id = "SVN",
           country_id = 202,
           cowcode = 349)

vdem8_post <- rbind(vdem8_post,Slovenia)

##Kosovo 1985-1998

Kosovo   <- vdem8_post %>%
    filter(country_name == "Serbia" & year %in% c(1985:1998)) %>%
    mutate(country_name = "Kosovo",
           country_text_id = "XKX",
           country_id = 43,
           cowcode = 347)

vdem8_post <- rbind(vdem8_post,Kosovo)

####	Slovakia 1985-1992 from	Czechia(Czech Republic)
Slovakia   <- vdem8_post %>%
    filter(country_name == "Czech Republic" & year %in% c(1985:1993)) %>%
    mutate(country_name = "Slovakia",
           country_text_id = "SVK",
           country_id = 201,
           cowcode = 317)
vdem8_post <- rbind(vdem8_post,Slovakia)
vdem8_post <- vdem8_post %>%
    filter(!(country_name == "Slovakia" & year == 1993 & is.na(vdem_libdem)))

###################################################
vdem8_post_check <- vdem8_post %>%
    mutate(ISO = countrycode(country_name, origin = 'country.name', destination = 'iso3c')) %>%
    mutate(country0 = iconv(country_name, "latin1", "UTF-8", sub=''),
           country = countrycode::countrycode(country0, "country.name", "country.name")) %>%
    mutate(ISO = ifelse(country_text_id == "XKX", "XKX",ISO),
           country = ifelse(country_text_id == "STP", "São Tomé & Príncipe",country),
           country_name = ifelse(country_text_id == "STP", "Sao Tome and Principe", country_name),
           ISO = ifelse( country_text_id == "STP", "STP",ISO)) %>%
    filter(!country_text_id=="PSG")  %>% # omit gaza
    dplyr::select(-country0)  %>%
    dplyr::select(country, country_text_id,country_name, ISO, year,everything()) 

saveRDS(vdem8_post_check, here("data","vdem8_1985_post.rds"))

vdem8_regime <- readRDS(here("data","vdem8_1985_post.rds")) %>% 
    dplyr::select(country, year, vdem_regime)
save(vdem8_regime,file = here("data","vdem8_regime.rda"))



#######################Create Vdem8 Related Variables 
vdem8_1985_post <- readRDS(here("data","vdem8_1985_post.rds"))

#Vdem_libdem
long_libdem <- vdem8_1985_post %>% 
    dplyr::select(!starts_with("polyarchy_v"))   %>%
    dplyr::select(!starts_with("liberal_v"))   %>%
    gather(libdem_iter, libdem_post, libdem_v11:libdem_v900)  %>%
    dplyr::select(country, year,starts_with("libdem")) %>%
    mutate(liter = as.numeric(str_replace(libdem_iter, "\\D+", ""))) %>%
    mutate(liter = liter -1 ) %>%
    dplyr::select(-libdem_iter) %>%
    dplyr::select(country, year, libdem_post, liter) %>% 
    dplyr::group_by(liter,country) %>%
    dplyr::arrange(year,.by_group = TRUE) 


cls_libdem_list <- list()
for (i in 1:900) {
    cls_libdem_list[[i]] <- long_libdem %>% 
        filter(liter ==  i)
}

cls_libdem_list <- cls_libdem_list %>%
    map(~.x %>%
            rename(Vdem_libdem = libdem_post) ) 

save(cls_libdem_list,file = here("data","cls_libdem_list.rda"))

#Vdem_polyarchy
long_poly <- vdem8_1985_post %>% 
    dplyr::select(!starts_with("libdem_"))  %>%
    dplyr::select(!starts_with("liberal_v")) %>%
    gather(poly_iter, poly_post, polyarchy_v11:polyarchy_v900) %>%
    dplyr::select(country, year, starts_with("poly"),everything() ) %>%
    mutate(piter = as.numeric(str_replace(poly_iter, "\\D+", ""))) %>%
    mutate(piter = piter -1 ) %>%
    dplyr::select(-poly_iter) %>%
    dplyr::select(country, year, poly_post, piter) %>% 
    dplyr::group_by(piter,country) %>%
    dplyr::arrange(year,.by_group = TRUE) 

cls_poly_list <- list()
for (i in 1:900) {
    cls_poly_list[[i]] <- long_poly %>% 
        filter(piter ==  i)
}

save(cls_poly_list,file = here("data","cls_poly_list.rda"))


#Vdem_liberal
long_liberal <- vdem8_1985_post %>% 
    dplyr::select(!starts_with("libdem_"))  %>%
    dplyr::select(!starts_with("polyarchy_v")) %>%
    gather(liter, liberal_post, liberal_v11:liberal_v900) %>%
    dplyr::select(country, year, starts_with("vdem"),everything() ) %>%
    mutate(iter = as.numeric(str_replace(liter, "\\D+", ""))) %>%
    mutate(iter = iter -1 ) %>%
    dplyr::select(-liter) %>%
    dplyr::select(country, year, liberal_post, iter) %>% 
    dplyr::group_by(iter,country) %>%
    dplyr::arrange(year,.by_group = TRUE) 


cls_liberal_list <- list()
for (i in 1:900) {
    cls_liberal_list[[i]] <- long_liberal %>% 
        filter(iter ==  i)
}

save(cls_liberal_list,file = here("data","cls_liberal_list.rda"))




################################### Vdem10 variables #########################
################################### loading vdem10 with standard error########
########################################### ##################################
vdem10_1985 <- read.csv(here("data","vdem10_1985.csv"))

## fillup vdem missing
##For Armenia 1985-1989 from Russia

#ARM  Armenia   
Armenia <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Armenia",
           country_text_id = "ARM",
           country_id = 105,
           COWcode = 371)

vdem10_1985 <- rbind(vdem10_1985,Armenia)
vdem10_1985 <- vdem10_1985 %>%
    mutate(COWcode = ifelse(country_name == "Armenia" & year == 1990,371,COWcode ))

##For Tajikistan 1985-1989 from Russia

#TJK             Tajikistan          133 702      
Tajikistan <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Tajikistan",
           country_text_id = "TJK",
           country_id = 133,
           COWcode = 702)

vdem10_1985 <- rbind(vdem10_1985,Tajikistan)
vdem10_1985 <- vdem10_1985 %>%
    mutate(COWcode = ifelse(country_name == "Tajikistan" & year == 1990,702,COWcode ))


#Azerbaijan 1985-1989 from Russia

#AZE             Azerbaijan          106 NA       1990
Azerbaijan <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Azerbaijan",
           country_text_id = "AZE",
           country_id = 106,
           COWcode = 373)

vdem10_1985 <- rbind(vdem10_1985,Azerbaijan)
vdem10_1985 <- vdem10_1985 %>%
    mutate(COWcode = ifelse(country_name == "Azerbaijan" & year == 1990,373,COWcode ))

#Belarus 1985-1989 from Russia

# BLR             Belarus             107 370      1991
Belarus <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Belarus",
           country_text_id = "BLR",
           country_id = 107,
           COWcode = 370)

vdem10_1985 <- rbind(vdem10_1985,Belarus)
vdem10_1985 <- vdem10_1985 %>%
    mutate(COWcode = ifelse(country_name == "Belarus" & year == 1990,370,COWcode ))


#Kazakhstan 1985-1990

# KAZ             Kazakhstan          121 705      1991
Kazakhstan <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1990)) %>%
    mutate(country_name = "Kazakhstan",
           country_text_id = "KAZ",
           country_id = 121,
           COWcode = 705)

vdem10_1985 <- rbind(vdem10_1985,Kazakhstan)
vdem10_1985 <- vdem10_1985 %>%
    mutate(COWcode = ifelse(country_name == "Kazakhstan" & year == 1990,705,COWcode)) %>%
    filter(!(country_name == "Kazakhstan" & year == 1990 & is.na(Vdem_libdem)))

#Estonia  1985-1989

# EST             Estonia             161 366      1991
Estonia <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Estonia",
           country_text_id = "EST",
           country_id = 161,
           COWcode = 366)

vdem10_1985 <- rbind(vdem10_1985,Estonia)
vdem10_1985 <- vdem10_1985 %>%
    mutate(COWcode = ifelse(country_name == "Estonia" & year == 1990,366,COWcode ))

#Georgia 1985-1989
# GEO             Georgia             118 372      1996
Georgia  <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Georgia",
           country_text_id = "GEO",
           country_id = 118,
           COWcode = 372)

vdem10_1985 <- rbind(vdem10_1985,Georgia )

#Kyrgyzstan 1985-1989

# KGZ             Kyrgyzstan          122 703      1996
Kyrgyzstan  <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Kyrgyzstan",
           country_text_id = "KGZ",
           country_id = 122,
           COWcode = 703)

vdem10_1985 <- rbind(vdem10_1985,Kyrgyzstan )

#Latvia 1985-1989

# LVA             Latvia               84 367      1998
Latvia  <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Latvia",
           country_text_id = "LVA",
           country_id = 84,
           COWcode = 367)

vdem10_1985 <- rbind(vdem10_1985,Latvia  )

#Lithuania 1985-1989

# LTU             Lithuania           173 368      1998
Lithuania    <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Lithuania",
           country_text_id = "LTU",
           country_id = 173,
           COWcode = 368)

vdem10_1985 <- rbind(vdem10_1985,Lithuania)

#Moldova 1985-1989
#  MDA             Moldova             126 359      1998
Moldova <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Moldova",
           country_text_id = "MDA",
           country_id = 126,
           COWcode = 359)

vdem10_1985 <- rbind(vdem10_1985,Moldova)

#Ukraine  1985-1989

#UKR             Ukraine             100 369      1998
Ukraine    <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Ukraine",
           country_text_id = "UKR",
           country_id = 100,
           COWcode = 369)

vdem10_1985 <- rbind(vdem10_1985,Ukraine)

#Uzbekistan 1985-1989

#UZB             Uzbekistan          140 704      1999
Uzbekistan    <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(country_name = "Uzbekistan",
           country_text_id = "UZB",
           country_id = 140,
           COWcode = 704)

vdem10_1985 <- rbind(vdem10_1985,Uzbekistan)

####	Turkmenistan 1985-1990 from	Russia  1990 no demo information

#TKM             Turkmenistan        136 701      1991
Turkmenistan    <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1990)) %>%
    mutate(country_name = "Turkmenistan",
           country_text_id = "TKM",
           country_id = 136,
           COWcode = 701)

vdem10_1985 <- rbind(vdem10_1985,Turkmenistan)

vdem10_1985 <- vdem10_1985 %>%
    mutate(Vdem_liberal = ifelse(country_name == "Turkmenistan"& year == 1990 & !is.na(Vdem_polyarchy),0.122,Vdem_liberal),
           Vdem_corruption = ifelse(country_name == "Turkmenistan"&year == 1990 & !is.na(Vdem_polyarchy), 0.842, Vdem_corruption), 
           liberal_sd = ifelse(country_name == "Turkmenistan"&year == 1990 & !is.na(Vdem_polyarchy), 0.031, liberal_sd), 
           corr_sd = ifelse(country_name == "Turkmenistan"&year == 1990 & !is.na(Vdem_polyarchy), 0.038, corr_sd))

vdem10_1985 <- vdem10_1985 %>%
    filter(!(country_name == "Turkmenistan" & is.na(Vdem_polyarchy)))
######add Turkmenistan

#North Macedonia 1985-1990  from Serbia

#MKD            North Macedonia         176 343      2000
Macedonia    <- vdem10_1985 %>%
    filter(country_name == "Serbia" & year %in% c(1985:1990)) %>%
    mutate(country_name = "North Macedonia",
           country_text_id = "MKD",
           country_id = 176,
           COWcode = 343)

vdem10_1985 <- rbind(vdem10_1985,Macedonia)

##Bosnia & Herzegovina 1985-1991   	Serbia 1990's GDP 8733.346,  Bosnia & Herzegovina 542.3951

#BIH             Bosnia and Herzegovina        150 346      2001
BIH   <- vdem10_1985 %>%
    filter(country_name == "Serbia" & year %in% c(1985:1991)) %>%
    mutate(country_name = "Bosnia & Herzegovina",
           country_text_id = "BIH",
           country_id = 150,
           COWcode = 346)

vdem10_1985 <- rbind(vdem10_1985,BIH)

vdem10_1985_try <- vdem10_1985 %>%
    mutate(country_name = as.character(country_name))

vdem10_1985_cntry <- vdem10_1985_try %>%
    mutate(country_name = ifelse(country_text_id == "BIH", "Bosnia & Herzegovina", country_name))

##Croatia 1985-1990

#HRV             Croatia             154 344      1992
Croatia   <- vdem10_1985_cntry %>%
    filter(country_name == "Serbia" & year %in% c(1985:1990)) %>%
    mutate(country_name = "Croatia",
           country_text_id = "HRV",
           country_id = 154,
           COWcode = 344)

vdem10_1985_cntry <- rbind(vdem10_1985_cntry,Croatia)


##Montenegro 1985-1998   

#MNE             Montenegro          183 341      2006
Montenegro   <- vdem10_1985_cntry %>%
    filter(country_name == "Serbia" & year %in% c(1985:1998)) %>%
    mutate(country_name = "Montenegro",
           country_text_id = "MNE",
           country_id = 183,
           COWcode = 341)

vdem10_1985_cntry <- rbind(vdem10_1985_cntry,Montenegro) 
vdem10_1985_cntry <- vdem10_1985_cntry %>%
    filter(!(country_name == "Montenegro" & year == 1998 & is.na(Vdem_libdem)))

#Slovenia  1985-1988  
#SVN             Slovenia            202 349      1992

Slovenia   <- vdem10_1985_cntry %>%
    filter(country_name == "Serbia" & year %in% c(1985:1988)) %>%
    mutate(country_name = "Slovenia",
           country_text_id = "SVN",
           country_id = 202,
           COWcode = 349)

vdem10_1985_cntry <- rbind(vdem10_1985_cntry,Slovenia)

#Kosovo 1985-1998
#XKX             Kosovo               43 347      2008

Kosovo   <- vdem10_1985_cntry %>%
    filter(country_name == "Serbia" & year %in% c(1985:1998)) %>%
    mutate(country_name = "Kosovo",
           country_text_id = "XKX",
           country_id = 43,
           COWcode = 347)

vdem10_1985_cntry <- rbind(vdem10_1985_cntry,Kosovo)

####	Slovakia 1985-1992 from	Czechia(Czech Republic)

#SVK             Slovakia            201 317      1993

Slovakia   <- vdem10_1985_cntry %>%
    filter(country_name == "Czech Republic" & year %in% c(1985:1992)) %>%
    mutate(country_name = "Slovakia",
           country_text_id = "SVK",
           country_id = 201,
           COWcode = 317)

vdem10_1985_cntry <- rbind(vdem10_1985_cntry,Slovakia)


vdem10_1985_check <- vdem10_1985_cntry %>%
    mutate(ISO = countrycode(country_name, origin = 'country.name', destination = 'iso3c')) %>%
    mutate(country0 = iconv(country_name, "latin1", "UTF-8", sub=''),
           country = countrycode::countrycode(country0, "country.name", "country.name")) %>%
    mutate(ISO = ifelse(country_text_id == "XKX", "XKX",ISO)) %>%
    filter(!country_text_id=="PSG")  %>% # omit gaza
    dplyr::select(-country0)

saveRDS(vdem10_1985_check, here("data","vdem10_1985_se_cntry.rds"))


vdem10_regime <- readRDS(here("data","vdem10_1985_se_cntry.rds")) %>% 
    dplyr::select(country, year, Vdem_regime)
save(vdem10_regime,file = here("data","vdem10_regime.rda"))


########libdem_list
pdist <- vdem10_1985_check %>%
    dplyr::select(Vdem_libdem,libdem_sd) %>%
    pmap(function(Vdem_libdem,libdem_sd,...) rnorm(900, Vdem_libdem, libdem_sd))  #posterior distribution 900 draws

pdist_df <- pdist %>% 
    map_df(as_tibble)

pdist_df$iter_se <- rep(1:900, length.out=nrow(pdist_df)) 
pdist_df <- data.frame(pdist_df)

cntry_year <- vdem10_1985_check %>% dplyr::group_by(country, year) %>% 
    tidyr::expand(iter_cn = 1:900)  %>%
    dplyr::group_by(country,year) %>% 
    dplyr::arrange(iter_cn,.by_group = TRUE)  #arrange by country year iter

cntry_year_pdist <- data.frame(cntry_year)

cntry_year_libdem <- cbind(cntry_year_pdist, pdist_df)

data_temp <- cntry_year_libdem  %>% 
    dplyr::group_by(iter_cn,country) %>%
    dplyr::arrange(year,.by_group = TRUE) 

libdem_list <- list()
for (i in 1:900) {
    libdem_list[[i]] <- data_temp %>% 
        filter(iter_cn ==  i)
}

save(libdem_list, file = here::here("data","libdem_list.rda"))

##############polyarchi_list
pdist_poly <- vdem10_1985_check %>%
    dplyr::select(Vdem_polyarchy,polyarchy_sd) %>%
    pmap(function(Vdem_polyarchy,polyarchy_sd,...) rnorm(900, Vdem_polyarchy,polyarchy_sd))  #posterior distribution 900 draws

pdist_poly_df <- pdist_poly %>% 
    map_df(as_tibble)

pdist_poly_df$iter_se <- rep(1:900, length.out=nrow(pdist_poly_df)) 

pdist_poly_df <- data.frame(pdist_poly_df)

cntrl_poly <- cbind(cntry_year_pdist, pdist_poly_df)

cntrl_poly <- cntrl_poly %>%
    rename(Vdem_polyarchy_post = value)

data_temp <- cntrl_poly %>% 
    dplyr::group_by(iter_cn,country) %>%
    dplyr::arrange(year,.by_group = TRUE) 

poly_list <- list()
for (i in 1:900) {
    poly_list[[i]] <- data_temp %>% 
        filter(iter_cn == i)
}

save(poly_list, file = here::here("data","poly_list.rda"))


############liberal_ist
libpdist <- vdem10_1985_check %>%
    dplyr::select(Vdem_liberal,liberal_sd) %>%
    pmap(function(Vdem_liberal,liberal_sd,...) rnorm(900, Vdem_liberal, liberal_sd))  #posterior distribution 900 draws

libpdist_df <- libpdist %>% 
    map_df(as_tibble)

libpdist_df$iter_se <- rep(1:900, length.out=nrow(libpdist_df)) 
libpdist_df <- data.frame(libpdist_df)

cntrl_liberal <- cbind(cntry_year_pdist, libpdist_df)
cntrl_liberal <- cntrl_liberal %>%
    rename(Vdem_liberal_post = value)

data_temp <- cntrl_liberal %>% 
    dplyr::group_by(iter_cn,country) %>%
    dplyr::arrange(year,.by_group = TRUE) 

lib_list <- list()
for (i in 1:900) {
    lib_list[[i]] <- data_temp %>% 
        filter(iter_cn == i)
}

save(lib_list, file = here::here("data","lib_list.rda"))

############create cpi_post and Cpi_list
cpi_95_19_error <- read_csv(here("data","cpi_95_19_error.csv"))


#create relative standard error for CPI. 
cpi_95_19_error <- cpi_95_19_error %>%   
    mutate(relative_error = cpi_error/cpi_100) %>%
    dplyr::group_by(country) %>%
    mutate(h_re = max(relative_error,na.rm = T),
           h_re = ifelse(h_re =="-Inf",NA,h_re)) %>%
    ungroup() %>%
    mutate(cpi_error = ifelse(is.na(cpi_error), cpi_100*h_re,cpi_error)) %>%
    dplyr::group_by(country) %>%
    mutate(cpi_100_m1 = lag(cpi_100)) %>%
    dplyr::arrange(country,year)

cpi_pdist <- cpi_95_19_error %>%
    dplyr::select(cpi_100,cpi_error) %>%
    pmap(function(cpi_100,cpi_error,...) rnorm(900, cpi_100,cpi_error))  #posterior distribution 900 draws

cpi_pdist_df <- cpi_pdist %>% 
    map_df(as_tibble)

cpi_pdist_df$iter_se <- rep(1:900, length.out=nrow(cpi_pdist_df)) 

cpi_95_19_error_900 <- cpi_95_19_error %>% dplyr::group_by(country, year) %>% 
    tidyr::expand(iter_cn = 1:900)  %>%
    dplyr::group_by(country,year) %>% 
    dplyr::arrange(iter_cn,.by_group = TRUE)  #arrange by country year iter

cpi_95_19_error_900 <- data.frame(cpi_95_19_error_900)
cpi_pdist_df <- data.frame(cpi_pdist_df)

cpi_se <- cbind(cpi_95_19_error_900, cpi_pdist_df)
cpi_se <- cpi_se %>%
    rename(cpi_post = value) %>%
    mutate(cpi_post = ifelse(is.nan(cpi_post),NA,cpi_post))

cpi <- left_join(cpi_se, cpi_95_19_error,by =c("country","year"))

cpi_post <- cpi %>%
    dplyr::select(-h_re,-relative_error) %>%
    mutate(cpi_z = as.vector(scale(cpi_100*-1)), # reversed, positive--more corruption
           cpi_m1_z = as.vector(scale(cpi_100_m1*-1)), 
           cpi_post_z = as.vector(scale(cpi_post*-1))) %>%
    dplyr::group_by(country, iter_se) %>%
    dplyr::arrange(year, .by_group = TRUE) %>% 
    mutate(cpi_postm1 = lag(cpi_post),
           cpi_postm1_z = as.vector(scale(cpi_postm1)))

saveRDS(cpi_post, file = here::here("data","cpi_post.rds"))

cpi_post_z <- cpi_post %>%
    dplyr::select(country, year,iter_cn,iter_se,cpi_post,cpi_post_z)

data_temp <- cpi_post_z %>% 
    dplyr::group_by(iter_cn,country) %>%
    dplyr::arrange(year,.by_group = TRUE) 

cpi_list <- list()
for (i in 1:900) {
    cpi_list[[i]] <- data_temp %>% 
        filter(iter_cn ==  i)
}

save(cpi_list, file = here::here("data","cpi_list.rda"))
