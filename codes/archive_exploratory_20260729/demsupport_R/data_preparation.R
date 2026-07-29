# Function and packages preload ####

if (!require(pacman))
    install.packages("pacman")
library(pacman)
# load all the packages you will use below
p_load(here,
       purrr,
       countrycode,
       tidyverse,
       stringr,
       haven,
       readxl,
       mi,
       mitools,
       rstan,
       osfr)

set.seed(313)

reformat_dcpo_output <- function(x, parameter_name) {
    df_temp <- x %>% 
        as_tibble(.name_repair = ~ls_country) %>% 
        mutate(year = first_year + row_number() - 1) %>% 
        pivot_longer(cols = all_of(ls_country),
                     names_to = "country",
                     values_to = parameter_name) %>%
        arrange(country, year)
    return(df_temp)
}

###Input Data:  vdem8_posterior.dta from Vdem8
###             vdem10_1985.csv     from Vdem10
###             cpi_95_19_error.csv from Corruption Perception Index
###Output Data: vdem8_1985_post.rds       vdem8_regime.rda
###             vdem10_1985_se_cntry.rds, vdem10_regime.rda
###             cls_libdem_list.rda       posterior distribution of V-Dem8 Liberal Democracy Index
###             cls_poly_list.rda         posterior distribution of V-Dem8 electoral democracy index
###             cls_liberal_list.rda      posterior distribution of V-Dem8 liberal component index
###             libdem_list.rda           posterior distribution of V-Dem10 Liberal Democracy Index
###             poly_list.rda             posterior distribution of V-Dem10 electoral democracy index
###             lib_list.rda              posterior distribution of V-Dem10 liberal component index



# Vdem8 variables #####
## Loading vdem8 posterior distribution #####

vdem8_post <- read_dta(here("data", "vdem8_posterior.dta"))
vdem8_post <- vdem8_post  %>%
    mutate(
        vdem_polyarchy = as.numeric(vdem_polyarchy),
        vdem_libdem = as.numeric(vdem_libdem),
        vdem_region = as.numeric(vdem_region),
        vdem_regime = as.numeric(vdem_regime),
        vdem_liberal = as.numeric(vdem_liberal)
    )

vdem8_na <- vdem8_post %>%
    filter_at(vars(starts_with("vdem_")), any_vars(is.na(.)))

## Check missing and fill up former USSR countries values #####
## For Armenia 1985-1989 from Russia

Armenia <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Armenia",
        country_text_id = "ARM",
        country_id = 105,
        cowcode = 371
    )

vdem8_post <- rbind(vdem8_post, Armenia)
vdem8_post <- vdem8_post %>%
    mutate(cowcode = ifelse(country_name == "Armenia" &
                                year == 1990, 371, cowcode))

## For Tajikistan 1985-1989 from Russia

Tajikistan <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Tajikistan",
        country_text_id = "TJK",
        country_id = 133,
        cowcode = 702
    )

vdem8_post <- rbind(vdem8_post, Tajikistan)
vdem8_post <- vdem8_post %>%
    mutate(cowcode = ifelse(country_name == "Tajikistan" &
                                year == 1990, 702, cowcode))


## Azerbaijan 1985-1989 from Russia

Azerbaijan <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Azerbaijan",
        country_text_id = "AZE",
        country_id = 106,
        cowcode = 373
    )

vdem8_post <- rbind(vdem8_post, Azerbaijan)


## Belarus 1985-1989 from Russia

Belarus <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Belarus",
        country_text_id = "BLR",
        country_id = 107,
        cowcode = 370
    )

vdem8_post <- rbind(vdem8_post, Belarus)
vdem8_post <- vdem8_post %>%
    mutate(cowcode = ifelse(country_name == "Belarus" &
                                year == 1990, 370, cowcode))

## Kazakhstan 1985-1990

Kazakhstan <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1990)) %>%
    mutate(
        country_name = "Kazakhstan",
        country_text_id = "KAZ",
        country_id = 121,
        cowcode = 705
    )

vdem8_post <- rbind(vdem8_post, Kazakhstan)
test1 <- vdem8_post %>%
    filter(country_name == "Kazakhstan")

vdem8_post <- vdem8_post %>%
    filter(!(country_name == "Kazakhstan" &
                 year == 1990 & is.na(vdem_libdem)))

## Estonia  1985-1989

Estonia <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1990)) %>%
    mutate(
        country_name = "Estonia",
        country_text_id = "EST",
        country_id = 161,
        cowcode = 366
    )

vdem8_post <- rbind(vdem8_post, Estonia)

vdem8_post <- vdem8_post  %>%
    filter(!(country_name == "Estonia" &
                 year == 1990 & is.na(vdem_libdem)))

## Georgia 1985-1989

Georgia  <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Georgia",
        country_text_id = "GEO",
        country_id = 118,
        cowcode = 372
    )

vdem8_post <- rbind(vdem8_post, Georgia)


## Kyrgyzstan 1985-1989

Kyrgyzstan  <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Kyrgyzstan",
        country_text_id = "KGZ",
        country_id = 122,
        cowcode = 703
    )

vdem8_post <- rbind(vdem8_post, Kyrgyzstan)

## Latvia 1985-1989

Latvia  <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Latvia",
        country_text_id = "LVA",
        country_id = 84,
        cowcode = 367
    )

vdem8_post <- rbind(vdem8_post, Latvia)

## Lithuania 1985-1989
Lithuania    <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Lithuania",
        country_text_id = "LTU",
        country_id = 173,
        cowcode = 368
    )

vdem8_post <- rbind(vdem8_post, Lithuania)

## Moldova 1985-1989

Moldova    <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Moldova",
        country_text_id = "MDA",
        country_id = 126,
        cowcode = 359
    )

vdem8_post <- rbind(vdem8_post, Moldova)

## Ukraine  1985-1989

Ukraine    <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Ukraine",
        country_text_id = "UKR",
        country_id = 100,
        cowcode = 369
    )

vdem8_post <- rbind(vdem8_post, Ukraine)

##Uzbekistan 1985-1989
Uzbekistan    <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Uzbekistan",
        country_text_id = "UZB",
        country_id = 140,
        cowcode = 704
    )

vdem8_post <- rbind(vdem8_post, Uzbekistan)

## Turkmenistan 1985-1990 from	Russia  1990 no demo information

Turkmenistan    <- vdem8_post %>%
    filter(country_name == "Russia" & year %in% c(1985:1990)) %>%
    mutate(
        country_name = "Turkmenistan",
        country_text_id = "TKM",
        country_id = 136,
        cowcode = 701
    )

vdem8_post <- rbind(vdem8_post, Turkmenistan)

vdem8_post <- vdem8_post %>%
    filter(!(country_name == "Turkmenistan" &
                 is.na(vdem_polyarchy)))

## North Macedonia 1985-1990  from Serbia

Macedonia    <- vdem8_post %>%
    filter(country_name == "Serbia" & year %in% c(1985:1993)) %>%
    mutate(
        country_name = "Macedonia",
        country_text_id = "MKD",
        country_id = 176,
        cowcode = 343
    )

vdem8_post <- rbind(vdem8_post, Macedonia)


vdem8_post <- vdem8_post %>%
    filter(!(country_name == "Macedonia" & is.na(vdem_polyarchy)))


## Bosnia & Herzegovina 1985-1991


BIH    <- vdem8_post %>%
    filter(country_name == "Serbia" & year %in% c(1985:1991)) %>%
    mutate(
        country_name = "Bosnia & Herzegovina",
        country_text_id = "BIH",
        country_id = 150,
        cowcode = 346
    )

vdem8_post <- rbind(vdem8_post, BIH)

vdem8_post <- vdem8_post %>%
    mutate(country_name = ifelse(country_text_id == "BIH", "Bosnia & Herzegovina", country_name))


## Croatia 1985-1990

Croatia   <- vdem8_post %>%
    filter(country_name == "Serbia" & year %in% c(1985:1990)) %>%
    mutate(
        country_name = "Croatia",
        country_text_id = "HRV",
        country_id = 154,
        cowcode = 344
    )

vdem8_post <- rbind(vdem8_post, Croatia)

## Montenegro 1985-1998

Montenegro   <- vdem8_post %>%
    filter(country_name == "Serbia" & year %in% c(1985:1998)) %>%
    mutate(
        country_name = "Montenegro",
        country_text_id = "MNE",
        country_id = 183,
        cowcode = 341
    )

vdem8_post <- rbind(vdem8_post, Montenegro)

vdem8_post <- vdem8_post %>%
    filter(!(country_name == "Montenegro" &
                 year == 1998 & is.na(vdem_libdem)))


## Slovenia  1985-1988

Slovenia   <- vdem8_post %>%
    filter(country_name == "Serbia" & year %in% c(1985:1988)) %>%
    mutate(
        country_name = "Slovenia",
        country_text_id = "SVN",
        country_id = 202,
        cowcode = 349
    )

vdem8_post <- rbind(vdem8_post, Slovenia)

## Kosovo 1985-1998

Kosovo   <- vdem8_post %>%
    filter(country_name == "Serbia" & year %in% c(1985:1998)) %>%
    mutate(
        country_name = "Kosovo",
        country_text_id = "XKX",
        country_id = 43,
        cowcode = 347
    )

vdem8_post <- rbind(vdem8_post, Kosovo)

## Slovakia 1985-1992 from	Czechia(Czech Republic)
Slovakia   <- vdem8_post %>%
    filter(country_name == "Czech Republic" &
               year %in% c(1985:1993)) %>%
    mutate(
        country_name = "Slovakia",
        country_text_id = "SVK",
        country_id = 201,
        cowcode = 317
    )
vdem8_post <- rbind(vdem8_post, Slovakia)
vdem8_post <- vdem8_post %>%
    filter(!(country_name == "Slovakia" &
                 year == 1993 & is.na(vdem_libdem)))

## Check results ####
vdem8_post_check <- vdem8_post %>%
    mutate(ISO = countrycode(country_name, origin = 'country.name', destination = 'iso3c')) %>%
    # Dealing with the warning message due to the country names
    mutate(
        country0 = iconv(country_name, "latin1", "UTF-8", sub = ''),
        country = countrycode::countrycode(country0, "country.name", "country.name")
    ) %>%
    mutate(
        ISO = ifelse(country_text_id == "XKX", "XKX", ISO),
        country = ifelse(country_text_id == "STP", "São Tomé & Príncipe", country),
        country_name = ifelse(country_text_id == "STP", "Sao Tome and Principe", country_name),
        ISO = ifelse(country_text_id == "STP", "STP", ISO)
    ) %>%
    filter(!country_text_id == "PSG")  %>% # omit gaza
    dplyr::select(-country0)  %>%
    dplyr::select(country, country_text_id, country_name, ISO, year, everything())

saveRDS(vdem8_post_check, here("data", "vdem8_1985_post.rds"))

vdem8_regime <- readRDS(here("data", "vdem8_1985_post.rds")) %>%
    dplyr::select(country, year, vdem_regime)
save(vdem8_regime, file = here("data", "vdem8_regime.rda"))



## Create Related Variables #####

vdem8_1985_post <- readRDS(here("data", "vdem8_1985_post.rds"))

### Vdem_libdem #####
long_libdem <- vdem8_1985_post %>%
    dplyr::select(!starts_with("polyarchy_v"))   %>%
    dplyr::select(!starts_with("liberal_v"))   %>%
    gather(libdem_iter, libdem_post, libdem_v11:libdem_v900)  %>%
    dplyr::select(country, year, starts_with("libdem")) %>%
    mutate(liter = as.numeric(str_replace(libdem_iter, "\\D+", ""))) %>%
    mutate(liter = liter - 1) %>%
    dplyr::select(-libdem_iter) %>%
    dplyr::select(country, year, libdem_post, liter) %>%
    dplyr::group_by(liter, country) %>%
    dplyr::arrange(year, .by_group = TRUE)

cls_libdem_list <- long_libdem %>% 
    rename(Vdem_libdem = libdem_post) %>% 
    group_by(liter) %>% 
    group_split()


save(cls_libdem_list, file = here("data", "cls_libdem_list.rda"))

### Vdem_polyarchy #####
long_poly <- vdem8_1985_post %>%
    dplyr::select(!starts_with("libdem_"))  %>%
    dplyr::select(!starts_with("liberal_v")) %>%
    gather(poly_iter, poly_post, polyarchy_v11:polyarchy_v900) %>%
    dplyr::select(country, year, starts_with("poly"), everything()) %>%
    mutate(piter = as.numeric(str_replace(poly_iter, "\\D+", ""))) %>%
    mutate(piter = piter - 1) %>%
    dplyr::select(-poly_iter) %>%
    dplyr::select(country, year, poly_post, piter) %>%
    dplyr::group_by(piter, country) %>%
    dplyr::arrange(year, .by_group = TRUE)

cls_poly_list <- long_poly %>% 
    group_by(piter) %>% 
    group_split()


save(cls_poly_list, file = here("data", "cls_poly_list.rda"))


### Vdem_liberal #####
long_liberal <- vdem8_1985_post %>%
    dplyr::select(!starts_with("libdem_"))  %>%
    dplyr::select(!starts_with("polyarchy_v")) %>%
    gather(liter, liberal_post, liberal_v11:liberal_v900) %>%
    dplyr::select(country, year, starts_with("vdem"), everything()) %>%
    mutate(iter = as.numeric(str_replace(liter, "\\D+", ""))) %>%
    mutate(iter = iter - 1) %>%
    dplyr::select(-liter) %>%
    dplyr::select(country, year, liberal_post, iter) %>%
    dplyr::group_by(iter, country) %>%
    dplyr::arrange(year, .by_group = TRUE)


cls_liberal_list <- long_liberal %>% 
    group_by(iter) %>% 
    group_split()

save(cls_liberal_list, file = here("data", "cls_liberal_list.rda"))


# Vdem10 variables ####
# loading vdem10 with standard error

vdem10_1985 <- read.csv(here("data", "vdem10_1985.csv"))

## Fillup vdem missing ####
## Armenia 1985-1989 from Russia
# ARM  Armenia

Armenia <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Armenia",
        country_text_id = "ARM",
        country_id = 105,
        COWcode = 371
    )

vdem10_1985 <- rbind(vdem10_1985, Armenia)
vdem10_1985 <- vdem10_1985 %>%
    mutate(COWcode = ifelse(country_name == "Armenia" &
                                year == 1990, 371, COWcode))

## Tajikistan 1985-1989 from Russia
#TJK             Tajikistan          133 702

Tajikistan <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Tajikistan",
        country_text_id = "TJK",
        country_id = 133,
        COWcode = 702
    )

vdem10_1985 <- rbind(vdem10_1985, Tajikistan)
vdem10_1985 <- vdem10_1985 %>%
    mutate(COWcode = ifelse(country_name == "Tajikistan" &
                                year == 1990, 702, COWcode))


## Azerbaijan 1985-1989 from Russia
## AZE             Azerbaijan          106 NA       1990

Azerbaijan <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Azerbaijan",
        country_text_id = "AZE",
        country_id = 106,
        COWcode = 373
    )

vdem10_1985 <- rbind(vdem10_1985, Azerbaijan)
vdem10_1985 <- vdem10_1985 %>%
    mutate(COWcode = ifelse(country_name == "Azerbaijan" &
                                year == 1990, 373, COWcode))

##Belarus 1985-1989 from Russia
## BLR             Belarus             107 370      1991

Belarus <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Belarus",
        country_text_id = "BLR",
        country_id = 107,
        COWcode = 370
    )

vdem10_1985 <- rbind(vdem10_1985, Belarus)
vdem10_1985 <- vdem10_1985 %>%
    mutate(COWcode = ifelse(country_name == "Belarus" &
                                year == 1990, 370, COWcode))


## Kazakhstan 1985-1990
## KAZ             Kazakhstan          121 705      1991

Kazakhstan <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1990)) %>%
    mutate(
        country_name = "Kazakhstan",
        country_text_id = "KAZ",
        country_id = 121,
        COWcode = 705
    )

vdem10_1985 <- rbind(vdem10_1985, Kazakhstan)
vdem10_1985 <- vdem10_1985 %>%
    mutate(COWcode = ifelse(country_name == "Kazakhstan" &
                                year == 1990, 705, COWcode)) %>%
    filter(!(country_name == "Kazakhstan" &
                 year == 1990 & is.na(Vdem_libdem)))

## Estonia  1985-1989
## EST             Estonia             161 366      1991

Estonia <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Estonia",
        country_text_id = "EST",
        country_id = 161,
        COWcode = 366
    )

vdem10_1985 <- rbind(vdem10_1985, Estonia)
vdem10_1985 <- vdem10_1985 %>%
    mutate(COWcode = ifelse(country_name == "Estonia" &
                                year == 1990, 366, COWcode))

##Georgia 1985-1989
## GEO             Georgia             118 372      1996

Georgia  <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Georgia",
        country_text_id = "GEO",
        country_id = 118,
        COWcode = 372
    )

vdem10_1985 <- rbind(vdem10_1985, Georgia)

##Kyrgyzstan 1985-1989
## KGZ             Kyrgyzstan          122 703      1996

Kyrgyzstan  <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Kyrgyzstan",
        country_text_id = "KGZ",
        country_id = 122,
        COWcode = 703
    )

vdem10_1985 <- rbind(vdem10_1985, Kyrgyzstan)

## Latvia 1985-1989
## LVA             Latvia               84 367      1998

Latvia  <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Latvia",
        country_text_id = "LVA",
        country_id = 84,
        COWcode = 367
    )

vdem10_1985 <- rbind(vdem10_1985, Latvia)

## Lithuania 1985-1989
## LTU             Lithuania           173 368      1998

Lithuania    <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Lithuania",
        country_text_id = "LTU",
        country_id = 173,
        COWcode = 368
    )

vdem10_1985 <- rbind(vdem10_1985, Lithuania)

## Moldova 1985-1989
##  MDA             Moldova             126 359      1998

Moldova <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Moldova",
        country_text_id = "MDA",
        country_id = 126,
        COWcode = 359
    )

vdem10_1985 <- rbind(vdem10_1985, Moldova)

## Ukraine  1985-1989
##UKR             Ukraine             100 369      1998

Ukraine    <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Ukraine",
        country_text_id = "UKR",
        country_id = 100,
        COWcode = 369
    )

vdem10_1985 <- rbind(vdem10_1985, Ukraine)

## Uzbekistan 1985-1989
## UZB             Uzbekistan          140 704      1999

Uzbekistan    <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1989)) %>%
    mutate(
        country_name = "Uzbekistan",
        country_text_id = "UZB",
        country_id = 140,
        COWcode = 704
    )

vdem10_1985 <- rbind(vdem10_1985, Uzbekistan)

## Turkmenistan 1985-1990 from	Russia  1990 no demo information
## TKM             Turkmenistan        136 701      1991

Turkmenistan    <- vdem10_1985 %>%
    filter(country_name == "Russia" & year %in% c(1985:1990)) %>%
    mutate(
        country_name = "Turkmenistan",
        country_text_id = "TKM",
        country_id = 136,
        COWcode = 701
    )

vdem10_1985 <- rbind(vdem10_1985, Turkmenistan)

vdem10_1985 <- vdem10_1985 %>%
    mutate(
        Vdem_liberal = ifelse(
            country_name == "Turkmenistan" &
                year == 1990 & !is.na(Vdem_polyarchy),
            0.122,
            Vdem_liberal
        ),
        Vdem_corruption = ifelse(
            country_name == "Turkmenistan" &
                year == 1990 & !is.na(Vdem_polyarchy),
            0.842,
            Vdem_corruption
        ),
        liberal_sd = ifelse(
            country_name == "Turkmenistan" &
                year == 1990 & !is.na(Vdem_polyarchy),
            0.031,
            liberal_sd
        ),
        corr_sd = ifelse(
            country_name == "Turkmenistan" &
                year == 1990 & !is.na(Vdem_polyarchy),
            0.038,
            corr_sd
        )
    )

vdem10_1985 <- vdem10_1985 %>%
    filter(!(country_name == "Turkmenistan" &
                 is.na(Vdem_polyarchy)))


## North Macedonia 1985-1990  from Serbia
## MKD            North Macedonia         176 343      2000

Macedonia    <- vdem10_1985 %>%
    filter(country_name == "Serbia" & year %in% c(1985:1990)) %>%
    mutate(
        country_name = "North Macedonia",
        country_text_id = "MKD",
        country_id = 176,
        COWcode = 343
    )

vdem10_1985 <- rbind(vdem10_1985, Macedonia)

## Bosnia & Herzegovina 1985-1991   	Serbia 1990's GDP 8733.346,  Bosnia & Herzegovina 542.3951
## BIH             Bosnia and Herzegovina        150 346      2001

BIH   <- vdem10_1985 %>%
    filter(country_name == "Serbia" & year %in% c(1985:1991)) %>%
    mutate(
        country_name = "Bosnia & Herzegovina",
        country_text_id = "BIH",
        country_id = 150,
        COWcode = 346
    )

vdem10_1985 <- rbind(vdem10_1985, BIH)

vdem10_1985_try <- vdem10_1985 %>%
    mutate(country_name = as.character(country_name))

vdem10_1985_cntry <- vdem10_1985_try %>%
    mutate(country_name = ifelse(country_text_id == "BIH", "Bosnia & Herzegovina", country_name))

## Croatia 1985-1990
## HRV             Croatia             154 344      1992
Croatia   <- vdem10_1985_cntry %>%
    filter(country_name == "Serbia" & year %in% c(1985:1990)) %>%
    mutate(
        country_name = "Croatia",
        country_text_id = "HRV",
        country_id = 154,
        COWcode = 344
    )

vdem10_1985_cntry <- rbind(vdem10_1985_cntry, Croatia)


## Montenegro 1985-1998
## MNE             Montenegro          183 341      2006

Montenegro   <- vdem10_1985_cntry %>%
    filter(country_name == "Serbia" & year %in% c(1985:1998)) %>%
    mutate(
        country_name = "Montenegro",
        country_text_id = "MNE",
        country_id = 183,
        COWcode = 341
    )

vdem10_1985_cntry <- rbind(vdem10_1985_cntry, Montenegro)
vdem10_1985_cntry <- vdem10_1985_cntry %>%
    filter(!(country_name == "Montenegro" &
                 year == 1998 & is.na(Vdem_libdem)))

## Slovenia  1985-1988
## SVN             Slovenia            202 349      1992

Slovenia   <- vdem10_1985_cntry %>%
    filter(country_name == "Serbia" & year %in% c(1985:1988)) %>%
    mutate(
        country_name = "Slovenia",
        country_text_id = "SVN",
        country_id = 202,
        COWcode = 349
    )

vdem10_1985_cntry <- rbind(vdem10_1985_cntry, Slovenia)

## Kosovo 1985-1998
## XKX             Kosovo               43 347      2008

Kosovo   <- vdem10_1985_cntry %>%
    filter(country_name == "Serbia" & year %in% c(1985:1998)) %>%
    mutate(
        country_name = "Kosovo",
        country_text_id = "XKX",
        country_id = 43,
        COWcode = 347
    )

vdem10_1985_cntry <- rbind(vdem10_1985_cntry, Kosovo)

## Slovakia 1985-1992 from	Czechia(Czech Republic)
## SVK             Slovakia            201 317      1993

Slovakia   <- vdem10_1985_cntry %>%
    filter(country_name == "Czech Republic" &
               year %in% c(1985:1992)) %>%
    mutate(
        country_name = "Slovakia",
        country_text_id = "SVK",
        country_id = 201,
        COWcode = 317
    )

vdem10_1985_cntry <- rbind(vdem10_1985_cntry, Slovakia)

## Check results ####

vdem10_1985_check <- vdem10_1985_cntry %>%
    mutate(ISO = countrycode(country_name, origin = 'country.name', destination = 'iso3c')) %>%
    mutate(
        country0 = iconv(country_name, "latin1", "UTF-8", sub = ''),
        country = countrycode::countrycode(country0, "country.name", "country.name")
    ) %>%
    mutate(ISO = ifelse(country_text_id == "XKX", "XKX", ISO)) %>%
    filter(!country_text_id == "PSG")  %>% # omit gaza
    dplyr::select(-country0)

saveRDS(vdem10_1985_check, here("data", "vdem10_1985_se_cntry.rds"))

## Create related variables ####
vdem10_regime <-
    readRDS(here("data", "vdem10_1985_se_cntry.rds")) %>%
    dplyr::select(country, year, Vdem_regime)
save(vdem10_regime, file = here("data", "vdem10_regime.rda"))


## libdem_list

pdist <- vdem10_1985_check %>%
    dplyr::select(Vdem_libdem, libdem_sd) %>%
    pmap(function(Vdem_libdem, libdem_sd, ...)
        rnorm(900, Vdem_libdem, libdem_sd))  #posterior distribution 900 draws

pdist_df <- pdist %>%
    map_df(as_tibble)

pdist_df$iter_se <- rep(1:900, length.out = nrow(pdist_df))
pdist_df <- data.frame(pdist_df)

cntry_year <-
    vdem10_1985_check %>% dplyr::group_by(country, year) %>%
    tidyr::expand(iter_cn = 1:900)  %>%
    dplyr::group_by(country, year) %>%
    dplyr::arrange(iter_cn, .by_group = TRUE)  #arrange by country year iter

cntry_year_pdist <- data.frame(cntry_year)

cntry_year_libdem <- cbind(cntry_year_pdist, pdist_df)

data_temp <- cntry_year_libdem  %>%
    dplyr::group_by(iter_cn, country) %>%
    dplyr::arrange(year, .by_group = TRUE)

libdem_list <- data_temp %>% 
    group_by(iter_cn) %>% 
    group_split()

save(libdem_list, file = here("data", "libdem_list.rda"))

## polyarchi_list

pdist_poly <- vdem10_1985_check %>%
    dplyr::select(Vdem_polyarchy, polyarchy_sd) %>%
    pmap(function(Vdem_polyarchy, polyarchy_sd, ...)
        rnorm(900, Vdem_polyarchy, polyarchy_sd))  #posterior distribution 900 draws

pdist_poly_df <- pdist_poly %>%
    map_df(as_tibble)

pdist_poly_df$iter_se <- rep(1:900, length.out = nrow(pdist_poly_df))

pdist_poly_df <- data.frame(pdist_poly_df)

cntrl_poly <- cbind(cntry_year_pdist, pdist_poly_df)

cntrl_poly <- cntrl_poly %>%
    rename(Vdem_polyarchy_post = value)

data_temp <- cntrl_poly %>%
    dplyr::group_by(iter_cn, country) %>%
    dplyr::arrange(year, .by_group = TRUE)

poly_list <- data_temp %>% 
    group_by(iter_cn) %>% 
    group_split()
    

save(poly_list, file = here("data", "poly_list.rda"))


## liberal_ist

libpdist <- vdem10_1985_check %>%
    dplyr::select(Vdem_liberal, liberal_sd) %>%
    pmap(function(Vdem_liberal, liberal_sd, ...)
        rnorm(900, Vdem_liberal, liberal_sd))  #posterior distribution 900 draws

libpdist_df <- libpdist %>%
    map_df(as_tibble)

libpdist_df$iter_se <- rep(1:900, length.out = nrow(libpdist_df))
libpdist_df <- data.frame(libpdist_df)

cntrl_liberal <- cbind(cntry_year_pdist, libpdist_df)
cntrl_liberal <- cntrl_liberal %>%
    rename(Vdem_liberal_post = value)

data_temp <- cntrl_liberal %>%
    dplyr::group_by(iter_cn, country) %>%
    dplyr::arrange(year, .by_group = TRUE)

lib_list <- data_temp %>% 
    group_by(iter_cn) %>% 
    group_split()

save(lib_list, file = here("data", "lib_list.rda"))

## create cpi_post and Cpi_list

cpi_95_19_error <- read_csv(here("data", "cpi_95_19_error.csv"))


## create relative standard error for CPI.
cpi_95_19_error <- cpi_95_19_error %>%
    mutate(relative_error = cpi_error / cpi_100) %>%
    dplyr::group_by(country) %>%
    mutate(h_re = max(relative_error, na.rm = T),
           h_re = ifelse(h_re == "-Inf", NA, h_re)) %>%
    ungroup() %>%
    mutate(cpi_error = ifelse(is.na(cpi_error), cpi_100 * h_re, cpi_error)) %>%
    dplyr::group_by(country) %>%
    mutate(cpi_100_m1 = lag(cpi_100)) %>%
    dplyr::arrange(country, year)

cpi_pdist <- cpi_95_19_error %>%
    dplyr::select(cpi_100, cpi_error) %>%
    pmap(function(cpi_100, cpi_error, ...)
        rnorm(900, cpi_100, cpi_error))  #posterior distribution 900 draws

cpi_pdist_df <- cpi_pdist %>%
    map_df(as_tibble)

cpi_pdist_df$iter_se <- rep(1:900, length.out = nrow(cpi_pdist_df))

cpi_95_19_error_900 <-
    cpi_95_19_error %>% dplyr::group_by(country, year) %>%
    tidyr::expand(iter_cn = 1:900)  %>%
    dplyr::group_by(country, year) %>%
    dplyr::arrange(iter_cn, .by_group = TRUE)  #arrange by country year iter

cpi_95_19_error_900 <- data.frame(cpi_95_19_error_900)
cpi_pdist_df <- data.frame(cpi_pdist_df)

cpi_se <- cbind(cpi_95_19_error_900, cpi_pdist_df)
cpi_se <- cpi_se %>%
    rename(cpi_post = value) %>%
    mutate(cpi_post = ifelse(is.nan(cpi_post), NA, cpi_post))

cpi <- left_join(cpi_se, cpi_95_19_error, by = c("country", "year"))

cpi_post <- cpi %>%
    dplyr::select(-h_re, -relative_error) %>%
    mutate(
        cpi_z = as.vector(scale(cpi_100 * -1)),
        # reversed, positive--more corruption
        cpi_m1_z = as.vector(scale(cpi_100_m1 * -1)),
        cpi_post_z = as.vector(scale(cpi_post * -1))
    ) %>%
    dplyr::group_by(country, iter_se) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    mutate(cpi_postm1 = lag(cpi_post),
           cpi_postm1_z = as.vector(scale(cpi_postm1)))

saveRDS(cpi_post, file = here("data", "cpi_post.rds"))

cpi_post_z <- cpi_post %>%
    dplyr::select(country, year, iter_cn, iter_se, cpi_post, cpi_post_z)

data_temp <- cpi_post_z %>%
    dplyr::group_by(iter_cn, country) %>%
    dplyr::arrange(year, .by_group = TRUE)

cpi_list <- data_temp %>% 
    group_by(iter_cn) %>% 
    group_split()

save(cpi_list, file = here("data", "cpi_list.rda"))


# Control variables ####

####
#### Input: Muslims Population muslism_prop_2010.csv  http://globalreligiousfutures.org/
####        Dependence on natural resource revenues from WDI (WDIData.csv) and Haber_Menaldo(2011) (Haber_Menaldo_2011_APSR_Dataset.xls)
####        GDP:WDI2020(WDIData.csv ),IMF(WEOApr2020all.csv) and World Table Penn World Table Version.10 (pwt100.dta)
####        claassen_replication_input.rda
####        exp_claassen_input.rda
####        dcpo_input_raw.csv
#### Output:control_variable.csv, cls_cntrl.rda,exp_cntrl.rda, dcpo_cntrl.rda


## Muslim Population data ####

muslism_prop_2010 <-
    read.csv(here("data", "muslism_prop_2010.csv")) %>%
    transmute(
        country_code = gsub("\"|\"", "", iso_code),
        country_name = gsub("\"|\"", "", name),
        muslism_prop_2010 = as.numeric(gsub("<|>|%|\"|\"", "", value))
    )  %>%
    mutate(
        country_name = trimws(country_name),
        country_code = countrycode::countrycode(country_code, origin = 'iso2c', destination = 'iso3c'),
        country_code = ifelse(
            country_name == "Kosovo",
            "XKX",
            ifelse(country_name == "Namibia", "NAM", country_code)
        )
    ) %>%
    filter(!is.na(country_code))

## IMF: GDP ####

gdp <- read.csv(here("data", "WEOApr2020all.csv")) %>%
    filter(WEO.Subject.Code	== "PPPGDP") %>%
    select(ISO, Country, X1980:X2019)
colnames(gdp)[3:42] <-
    c(
        "1980",
        "1981",
        "1982",
        "1983",
        "1984",
        "1985",
        "1986",
        "1987",
        "1988",
        "1989",
        "1990",
        "1991",
        "1992",
        "1993",
        "1994",
        "1995",
        "1996",
        "1997",
        "1998",
        "1999",
        "2000",
        "2001",
        "2002",
        "2003",
        "2004",
        "2005",
        "2006",
        "2007",
        "2008",
        "2009",
        "2010",
        "2011",
        "2012",
        "2013",
        "2014",
        "2015",
        "2016",
        "2017",
        "2018",
        "2019"
    )

long_gdp <- gdp %>%
    gather(year, gdp, "1980":"2019") %>%
    mutate(
        year = as.numeric(year),
        gdp = as.numeric(str_replace(gdp, ",", "")),
        Country = ifelse(ISO == "STP", "São Tomé & Príncipe", Country),
        country_code = countrycode(Country, origin = 'country.name', destination = 'iso3c'),
        country_code = ifelse(Country == "Kosovo", "XKX", country_code)
    )

## GDP ####
### WB: GDP and Dependence ####

wdi_ind <- read.csv(here("data", "WDIData.csv")) %>%
    filter(
        Indicator.Code == "NY.GDP.PCAP.PP.CD" |
            Indicator.Code == "SP.POP.TOTL" |
            Indicator.Code == "NY.GDP.TOTL.RT.ZS"
    ) %>%
    select(-c("Indicator.Name", "Country.Code")) %>%
    rename(country = names(.[1])) %>%
    select(-X)

names(wdi_ind) <-
    gsub(names(wdi_ind), pattern = "X", replacement = "")

wdi_ind_19602019  <- wdi_ind %>%
    gather(year, index, 3:62)  %>%
    mutate(indicator = ifelse(
        Indicator.Code == "NY.GDP.PCAP.PP.CD",
        "wdi_gdppc",
        ifelse(
            Indicator.Code == "SP.POP.TOTL",
            "pop",
            ifelse(Indicator.Code == "NY.GDP.TOTL.RT.ZS", "dependence", NA)
        )
    )) %>%
    select(-Indicator.Code) %>%
    spread(indicator, index) %>%
    mutate(ISO = countrycode(country, origin = 'country.name', destination = 'iso3c')) %>%
    mutate(
        pop = as.numeric(pop),
        year = as.numeric(year),
        dependence = as.numeric(dependence)
    ) %>%
    mutate(ISO = ifelse(country == "Kosovo", "XKX", ISO)) %>%
    rename(country_code = ISO) %>%
    filter(!is.na(country_code))

### Penn World Table: GDP ####

pwt100_gdp <- rio::import(here("data", "pwt100.dta"))  %>%
    select(countrycode, country, year, pop, rgdpna) %>%
    rename(pen_gdp = rgdpna,
           country_name = country,
           pen_pop = pop)  %>%
    mutate(country_code = countrycode(country_name,  origin = 'country.name', 'iso3c'))

#### Haber and Menaldo (2011): Dependence

resource_dep <-
    read_excel(here("data", "Haber_Menaldo_2011_APSR_Dataset.xls"),
               sheet = "Haber_Menaldo_2011_APSR_") %>%
    mutate(
        cnamehabmen = ifelse(cnamehabmen == "Pakisan", "Pakistan", cnamehabmen),
        country = countrycode(cnamehabmen, origin = 'country.name', destination = 'country.name'),
    ) %>%
    select("country", "year", "Total_Resources_Income_PC") %>%
    rename (dependence_pc_hm = Total_Resources_Income_PC)  # Tanzania was input twice.

## MERGING regionUN, other variables
load(here("data/country_regionUN.rda"))

muslism_cnty <- na.omit(muslism_prop_2010$country_code)
cntry_year <-
    data.frame(country_code = rep(muslism_cnty, each = 61),
               year = rep((1960:2020), times = 234)) %>%
    mutate(
        country = countrycode(country_code, 'iso3c', 'country.name'),
        country = ifelse(country_code == "XKX", "Kosovo", country)
    ) %>%
    distinct(country, country_code, year)

control_variable <-
    left_join(cntry_year, country_regionUN, by = c("country")) %>%
    left_join(muslism_prop_2010, by = "country_code") %>%
    distinct() %>%
    left_join(long_gdp %>% select(country_code, year, gdp),
              by = c("country_code", "year")) %>%
    left_join(wdi_ind_19602019 %>% select(-country),
              by = c("country_code", "year"))  %>%
    left_join(
        pwt100_gdp %>% select(country_code, year, pen_pop, pen_gdp),
        by = c("country_code", "year")
    ) %>%
    mutate(
        gdp_percapita = (gdp * 1000000000) / pop,
        lggdp_percap = log(gdp_percapita),
        pen_gdp_pc = pen_gdp / pen_pop
    ) %>%
    rename(
        wb_pop = pop,
        imf_gdp = gdp,
        imf_gdp_pc = gdp_percapita,
        imflg_gdppc = lggdp_percap,
        wb_gdp_pc = wdi_gdppc
    )  %>%
    left_join(resource_dep, by = c("country", "year")) %>%
    distinct()  %>%
    mutate(
        dep_wdi_pc = (dependence / 100) * wb_gdp_pc,
        dep_pc_di = ifelse(dep_wdi_pc < 1000, 0, 1),
        dep_hm_di = ifelse(dependence_pc_hm < 1000, 0, 1)
    ) %>%
    select(-imflg_gdppc)  #clean unused variabls

write.csv(
    control_variable,
    here("data", "control_variable.csv"),
    fileEncoding = "UTF-8",
    row.names = FALSE
)

control_variable <- read.csv(here("data", "control_variable.csv"))

mdf <-  control_variable %>%
    filter(year > 1985) %>%
    select(country, year, wb_gdp_pc, imf_gdp_pc, pen_gdp_pc) %>%
    missing_data.frame()

# show(mdf)

mdf <-
    change(mdf,
           y = c("year"),
           what = "type",
           to = "positive-continuous")
mdf <-
    change(
        mdf,
        y = c("wb_gdp_pc", "imf_gdp_pc", "pen_gdp_pc"),
        what = "type",
        to = "positive-continuous"
    )

mdf_mi <- mi(mdf, seed = 313)
mdf_mi_list <- complete(mdf_mi, m = 900)
mdf_mi_list <- lapply(mdf_mi_list, function(df)
    sapply(df, function(v)
        if (any(class(v) == "factor"))
            v <- as.numeric(levels(v))[v]
        else
            v <- v) %>%
        data.frame)
mdf_mi_list <- imputationList(mdf_mi_list)


mdf_dep <-  control_variable %>%
    filter(year > 1985) %>%
    select(dep_pc_di, dep_hm_di) %>%
    missing_data.frame()

# show(mdf_dep)

mdf_dep <-
    change(
        mdf_dep,
        y = c("dep_pc_di", "dep_hm_di"),
        what = "type",
        to = "binary"
    )

mdf_dep_mi <- mi(mdf_dep, seed = 313)
mdf_dep_mi_list <- complete(mdf_dep_mi, m = 900)
mdf_dep_mi_list <- lapply(mdf_dep_mi_list, function(df)
    sapply(df, function(v)
        if (any(class(v) == "factor"))
            v <- as.numeric(levels(v))[v]
        else
            v <- v) %>%
        data.frame)

mdf_dep_mi_list <- imputationList(mdf_dep_mi_list)


## First year for correct data and expanded data ####

load(here("data", "claassen_replication_input.rda"))

cls_cntry_firstyr <- claassen_replication_input[["data"]] %>%
    select(c("country", "year")) %>%
    unique()  %>% #1166
    group_by(country) %>%
    transmute(country = country,
              firstyear = min(year)) %>%
    unique() #137

cls_cntrl <- purrr::map(1:900, function(anEntry) {
    mdf_mi_list[["imputations"]][[anEntry]] %>%
        select(wb_gdp_pc) %>%
        cbind(
            control_variable %>%
                filter(year > 1985) %>%
                select(country_code, year, country, Region_UN, muslism_prop_2010)
        ) %>%
        cbind(mdf_dep_mi_list[["imputations"]][[anEntry]] %>% select(dep_pc_di)) %>%
        left_join(cls_cntry_firstyr, by = "country") %>%
        rename(Res_cp_WDI_di = dep_pc_di,
               Pr_Muslim = muslism_prop_2010) %>%
        mutate(lnGDP_imp = log(wb_gdp_pc)) %>%
        group_by(country) %>%
        mutate(
            lnGDP_imp_lag = dplyr::lag(lnGDP_imp, order_by = year),
            GDP_imp_grth = (lnGDP_imp - lnGDP_imp_lag) * 100,
            GDP_imp_grth_lag1 = dplyr::lag(GDP_imp_grth, order_by = year),
            Res_cp_WDI_di_lag1 = dplyr::lag(Res_cp_WDI_di, order_by = year)
        ) %>%
        select(
            country,
            year,
            country_code,
            Region_UN,
            firstyear,
            lnGDP_imp,
            lnGDP_imp_lag,
            GDP_imp_grth,
            GDP_imp_grth_lag1,
            Pr_Muslim,
            Res_cp_WDI_di,
            Res_cp_WDI_di_lag1
        )
    
})

save(cls_cntrl, file = here("data", "cls_cntrl.rda"))


## Expanded first year ####

load(here("data", "exp_claassen_input.rda"))

exp_cntry_firstyr <- exp_claassen_input[["data"]] %>%
    select(c("country", "year")) %>%
    unique()  %>% #1443
    group_by(country) %>%
    transmute(country = country,
              firstyear = min(year)) %>%
    unique() #145

exp_cntrl <- purrr::map(1:900, function(anEntry) {
    mdf_mi_list[["imputations"]][[anEntry]] %>%
        select(wb_gdp_pc) %>%
        cbind(
            control_variable %>%
                filter(year > 1985) %>%
                select(country_code, year, country, Region_UN, muslism_prop_2010)
        ) %>%
        cbind(mdf_dep_mi_list[["imputations"]][[anEntry]] %>% select(dep_pc_di)) %>%
        left_join(exp_cntry_firstyr, by = "country") %>%
        rename(Res_cp_WDI_di = dep_pc_di,
               Pr_Muslim = muslism_prop_2010) %>%
        mutate(lnGDP_imp = log(wb_gdp_pc)) %>%
        group_by(country) %>%
        mutate(
            lnGDP_imp_lag = dplyr::lag(lnGDP_imp, order_by = year),
            GDP_imp_grth = (lnGDP_imp - lnGDP_imp_lag) * 100,
            GDP_imp_grth_lag1 = dplyr::lag(GDP_imp_grth, order_by = year),
            Res_cp_WDI_di_lag1 = dplyr::lag(Res_cp_WDI_di, order_by = year)
        ) %>%
        select(
            country,
            year,
            country_code,
            Region_UN,
            firstyear,
            lnGDP_imp,
            lnGDP_imp_lag,
            GDP_imp_grth,
            GDP_imp_grth_lag1,
            Pr_Muslim,
            Res_cp_WDI_di,
            Res_cp_WDI_di_lag1
        )
    
})

save(exp_cntrl, file = here("data", "exp_cntrl.rda"))


# DCPO first year and control ####

dcpo_input_raw <- read_csv(here("data", "dcpo_input_raw.csv")) ##dcpo_input_raw includes Bahrain 2009 2014.
# min(dcpo_input_raw$year) #1988
# max(dcpo_input_raw$year) #2020
 length(unique(dcpo_input_raw$country))   #158

dcpo_cntry_firstyr <- dcpo_input_raw %>%
    distinct(country, year) %>%
    group_by(country) %>%
    transmute(country = country,
              firstyear = min(year)) %>%
    unique() #158

dcpo_cntrl <-  purrr::map(1:900, function(anEntry) {
    mdf_mi_list[["imputations"]][[anEntry]] %>%
        select(wb_gdp_pc) %>%
        cbind(
            control_variable %>%
                filter(year > 1985) %>%
                select(country_code, year, country, Region_UN, muslism_prop_2010)
        ) %>%
        cbind(mdf_dep_mi_list[["imputations"]][[anEntry]] %>% select(dep_pc_di)) %>%
        left_join(dcpo_cntry_firstyr, by = "country") %>%
        rename(Res_cp_WDI_di = dep_pc_di,
               Pr_Muslim = muslism_prop_2010) %>%
        mutate(lnGDP_imp = log(wb_gdp_pc)) %>%
        group_by(country) %>%
        mutate(
            lnGDP_imp_lag = dplyr::lag(lnGDP_imp, order_by = year),
            GDP_imp_grth = (lnGDP_imp - lnGDP_imp_lag) * 100,
            GDP_imp_grth_lag1 = dplyr::lag(GDP_imp_grth, order_by = year),
            Res_cp_WDI_di_lag1 = dplyr::lag(Res_cp_WDI_di, order_by = year)
        ) %>%
        select(
            country,
            year,
            country_code,
            Region_UN,
            firstyear,
            lnGDP_imp,
            lnGDP_imp_lag,
            GDP_imp_grth,
            GDP_imp_grth_lag1,
            Pr_Muslim,
            Res_cp_WDI_di,
            Res_cp_WDI_di_lag1
        )
    
})

save(dcpo_cntrl, file = here("data", "dcpo_cntrl.rda"))

# Data Merging ####

osf_retrieve_file("uvn4g") %>%
    osf_download(here("data"))  ##claassen_m5_3k_07-27-15-40.rda

## Corrected Data ####
### Creating Correct_theta_list ####

load(here("data", "claassen_m5_3k_07-27-15-40.rda"))
load(here("data", "claassen_replication_input.rda"))
claassen_m5_theta <-
    rstan::extract(claassen_m5, pars = "theta")  ##137*30

claassen_replication <- claassen_replication_input$data %>%
    janitor::clean_names() %>%
    DCPOtools::with_min_yrs(2)

# length(unique(claassen_replication$country)) ##137
# min(claassen_replication$year) #1988
# max(claassen_replication$year) #2017

ls_year <- 1988:2017
ls_country <- claassen_replication$country %>% unique()
first_year <- min(claassen_replication$year)
correct_cls_theta_list <- purrr::map(1:900, function(anEntry) {
    claassen_m5_theta$theta[anEntry, , ] %>%
        reformat_dcpo_output("theta")
}) # year 30, country 137

save(correct_cls_theta_list,
     file = here("data", "correct_cls_theta_list.rda"))


### Correct Analysis Data for Models in AJPS ####
# load(here("data","correct_cls_theta_list.rda"))

load(here("data", "cls_libdem_list.rda"))
load(here("data", "cls_cntrl.rda"))
load(here("data", "vdem8_regime.rda"))
load(here("data", "country_regionUN.rda"))

# merge variables
cls_ajps_cntrl_list <-  purrr::map(1:900, function(anEntry) {
    cls_libdem_list[[anEntry]] %>%
        mutate(Libdem_VD = Vdem_libdem * 100) %>%    ###negative values, after 100, so big.
        left_join(country_regionUN, by = c("country")) %>%
        group_by(country) %>%
        mutate(
            Libdem_m1 = dplyr::lag(Libdem_VD, n = 1, order_by = year),
            Libdem_m2 = dplyr::lag(Libdem_VD, n = 2, order_by = year)
        ) %>%
        ungroup() %>%
        mutate(
            ChgDem = Libdem_VD - Libdem_m1,
            UpChgDem = ifelse(ChgDem > 0, ChgDem, 0),
            DwnChgDem = ifelse(ChgDem < 0, ChgDem * (-1), 0)
        ) %>%
        ungroup() %>%
        group_by(Region_UN, year) %>%
        mutate(Libdem_regUN = mean(Libdem_VD, na.rm = TRUE)) %>%
        ungroup() %>%
        group_by(country) %>%
        mutate(Libdem_regUN_m1 = dplyr::lag(Libdem_regUN, n = 1, order_by = year)) %>%
        ungroup() %>%
        left_join(cls_cntrl[[anEntry]], by = c("year", "country", "Region_UN")) %>%
        filter(year > 1986) %>%
        filter(country %in% correct_cls_theta_list[[1]]$country)
})

# create trimmed variables.
correct_cls_ajps <- purrr::map(1:900, function(anEntry) {
    cls_ajps_cntrl_list[[anEntry]] %>%
        left_join(correct_cls_theta_list[[anEntry]], by = c("year", "country"))  %>%
        left_join(vdem8_regime %>%  rename(Regime_VD = vdem_regime),
                  by = c("country", "year")) %>%
        mutate(
            SupDem_trim = ifelse(year < firstyear, NA, theta),
            theta_dem_trim = ifelse(
                is.na(SupDem_trim),
                NA,
                ifelse(Regime_VD > 1 &
                           !is.na(SupDem_trim), theta, 0)
            ),
            theta_aut_trim = ifelse(
                is.na(SupDem_trim),
                NA,
                ifelse(Regime_VD < 2 &
                           !is.na(SupDem_trim), theta, 0)
            )
        ) %>%
        select(country, year, theta, contains("trim"), everything())
})

save(correct_cls_ajps, file = here("data", "correct_cls_ajps.rda"))


### Correct Analysis Data for Models in APSR ####
load(here("data", "cls_poly_list.rda"))
load(here("data", "cls_liberal_list.rda"))
load(here("data", "cpi_list.rda"))

## Standardize cls_libdem_list Libdem_z
cls_libdem_z <- cls_libdem_list %>%
    map( ~ .x %>%
             mutate(Libdem_z = as.vector(scale(Vdem_libdem))))
## Standardize cls_poly_list  Polyarchy_z
cls_polyarchy_z <- cls_poly_list %>%
    map( ~ .x %>%
             mutate(Polyarchy_z = as.vector(scale(poly_post))) %>%
             select(-poly_post))
## Standardize cls_liberal_list Liberal_z
cls_lib_z <- cls_liberal_list %>%
    map( ~ .x %>%
             mutate(Liberal_z = as.vector(scale(liberal_post))) %>%
             select(-liberal_post))
## corruption_z Corrup_TI_z
cpi_list_z <- cpi_list %>%
    map( ~ .x %>%
             rename(Corrup_TI_z = cpi_post_z) %>%
             select(-cpi_post))

# merge variables
cls_apsr_cntrl_list <-  purrr::map(1:900, function(anEntry) {
    cls_libdem_z[[anEntry]] %>%
        group_by(country) %>%
        mutate(Libdem_m1 = dplyr::lag(Libdem_z, 1, order_by =  year)) %>%
        ungroup() %>%
        mutate(ChgDem = Libdem_z - Libdem_m1) %>%
        ungroup()  %>%
        left_join(cls_cntrl[[anEntry]], by = c("year", "country")) %>%
        left_join(cls_polyarchy_z[[anEntry]], by = c("country", "year", "liter" =
                                                         "piter")) %>%
        left_join(cls_lib_z[[anEntry]], by = c("year", "country", "liter" =
                                                   "iter")) %>%
        left_join(cpi_list_z[[anEntry]], by = c("year", "country", "liter" = "iter_cn")) %>%
        filter(year > 1986) %>%
        filter(country %in% correct_cls_theta_list[[1]]$country)
})

# create trimmed data
correct_cls_apsr <- purrr::map(1:900, function(anEntry) {
    cls_apsr_cntrl_list[[anEntry]] %>%
        left_join(correct_cls_theta_list[[anEntry]], by = c("year", "country"))  %>%
        mutate(SupDem_trim = ifelse(year < firstyear, NA, theta)) %>%
        select(-Vdem_libdem, -liter, -iter_se) %>%
        select(country,
               year,
               firstyear,
               theta,
               SupDem_trim,
               contains("z"),
               everything())
})

save(correct_cls_apsr, file = here("data", "correct_cls_apsr.rda"))



## Expanded Correct Data ####
### Expanded Correct Theta_list ####

osf_retrieve_file("gk7nu") %>%
    osf_download(here("data"))  # exp_claassen_m5_3k_07-23-10-50.rda

load(here("data", "exp_claassen_m5_3k_07-23-10-50.rda"))
load(here("data", "exp_claassen_input.rda"))

exp_claassen_m5_theta <- rstan::extract(claassen_m5, pars = "theta")
ls_year <- 1988:2020
ls_country <- exp_claassen_input$data$country %>% unique() #145
first_year <- min(exp_claassen_input$data$year) #1988

expcor_cls_theta_list <- purrr::map(1:900, function(anEntry) {
    exp_claassen_m5_theta$theta[anEntry, , ] %>%
        reformat_dcpo_output("theta")
}) #year 33, country 145

save(expcor_cls_theta_list,
     file = here("data", "expcor_cls_theta_list.rda"))


### Expanded Correct Analysis Data for Models in AJPS ####

load(here("data", "libdem_list.rda"))
load(here("data", "exp_cntrl.rda"))
load(here("data", "vdem10_regime.rda"))
load(here("data", "country_regionUN.rda"))

# merge variables
exp_ajps_cntrl_list <-  purrr::map(1:900, function(anEntry) {
    libdem_list[[anEntry]] %>%
        rename(Vdem_libdem = value) %>%
        mutate(Libdem_VD = Vdem_libdem * 100) %>%
        left_join(country_regionUN, by = c("country")) %>%
        group_by(country) %>%
        mutate(
            Libdem_m1 = dplyr::lag(Libdem_VD, n = 1, order_by = year),
            Libdem_m2 = dplyr::lag(Libdem_VD, n = 2, order_by = year)
        ) %>%
        ungroup() %>%
        mutate(
            ChgDem = Libdem_VD - Libdem_m1,
            UpChgDem = ifelse(ChgDem > 0, ChgDem, 0),
            DwnChgDem = ifelse(ChgDem < 0, ChgDem * (-1), 0)
        ) %>%
        ungroup() %>%
        dplyr::group_by(Region_UN, year) %>%
        mutate(Libdem_regUN = mean(Libdem_VD, na.rm = TRUE)) %>%
        ungroup() %>%
        dplyr::group_by(country) %>%
        mutate(Libdem_regUN_m1 = dplyr::lag(Libdem_regUN, n = 1, order_by = year)) %>%
        ungroup() %>%
        left_join(exp_cntrl[[anEntry]], by = c("year", "country", "Region_UN")) %>%
        filter(year > 1986) %>%
        filter(country %in% expcor_cls_theta_list[[1]]$country)
})


# create trimmed variables.
expcor_cls_ajps <- purrr::map(1:900, function(anEntry) {
    exp_ajps_cntrl_list[[anEntry]] %>%
        left_join(expcor_cls_theta_list[[anEntry]], by = c("year", "country"))  %>%
        left_join(vdem10_regime %>%  rename(Regime_VD = Vdem_regime),
                  by = c("country", "year")) %>%
        mutate(
            SupDem_trim = ifelse(year < firstyear, NA, theta),
            theta_dem_trim = ifelse(
                is.na(SupDem_trim),
                NA,
                ifelse(Regime_VD > 1 &
                           !is.na(SupDem_trim), theta, 0)
            ),
            theta_aut_trim = ifelse(
                is.na(SupDem_trim),
                NA,
                ifelse(Regime_VD < 2 &
                           !is.na(SupDem_trim), theta, 0)
            )
        ) %>%
        select(country, year, theta, contains("trim"), everything())
})

save(expcor_cls_ajps, file = here("data", "expcor_cls_ajps.rda"))


### Expanded Correct Analysis Data for Models in APSR  ####

load(here("data", "libdem_list.rda"))
load(here("data", "poly_list.rda"))
load(here("data", "lib_list.rda"))
load(here("data", "cpi_list.rda"))

## Standardize Vdem_libdem Vdem_libdem_z
libdem_list_z <- libdem_list %>%
    map( ~ .x %>%
             rename(Vdem_libdem = value) %>%
             mutate(Vdem_libdem_z = as.vector(scale(Vdem_libdem))))
## Standardize Vdem_polyarchy_z  Vdem_liberal_z
polyarchy_list_z <- poly_list %>%
    map(
        ~ .x %>%
            rename(Vdem_polyarchy = Vdem_polyarchy_post) %>%
            mutate(Vdem_polyarchy_z = as.vector(scale(Vdem_polyarchy)))
    )
## Standardize Vdem_liberal_z
lib_list_z <- lib_list %>%
    map(
        ~ .x %>%
            rename(Vdem_liberal = Vdem_liberal_post) %>%
            mutate(Vdem_liberal_z = as.vector(scale(Vdem_liberal)))
    )
## corruption_z
cpi_list_z <- cpi_list %>%
    map( ~ .x %>%
             rename(corruption_z = cpi_post_z) %>%
             select(-cpi_post))


# merge variables
exp_apsr_cntrl_list <- purrr::map(1:900, function(anEntry) {
    libdem_list_z[[anEntry]] %>%
        group_by(country) %>%
        mutate(Libdem_m1 = dplyr::lag(Vdem_libdem_z, 1, order_by =  year)) %>%
        ungroup() %>%
        mutate(ChgDem = Vdem_libdem_z - Libdem_m1) %>%
        left_join(polyarchy_list_z[[anEntry]],
                  by = c("year", "country", "iter_cn", "iter_se")) %>%
        left_join(lib_list_z[[anEntry]], by = c("year", "country", "iter_cn", "iter_se")) %>%
        left_join(cpi_list_z[[anEntry]], by = c("year", "country", "iter_cn", "iter_se")) %>%
        select(-Vdem_libdem, -Vdem_polyarchy, -Vdem_liberal, -iter_se) %>%
        left_join(exp_cntrl[[anEntry]], by = c("year", "country")) %>%
        filter(year > 1986) %>%
        filter(country %in% expcor_cls_theta_list[[1]]$country) %>%
        rename(
            Libdem_z = Vdem_libdem_z,
            Polyarchy_z = Vdem_polyarchy_z,
            Liberal_z = Vdem_liberal_z,
            Corrup_TI_z = corruption_z
        )
})


# create trimmed data
expcor_cls_apsr <- purrr::map(1:900, function(anEntry) {
    exp_apsr_cntrl_list[[anEntry]] %>%
        left_join(expcor_cls_theta_list[[anEntry]], by = c("year", "country"))  %>%
        mutate(SupDem_trim = ifelse(year < firstyear, NA, theta)) %>%
        select(country,
               year,
               firstyear,
               theta,
               SupDem_trim,
               contains("z"),
               everything())
    
})


save(expcor_cls_apsr, file = here("data", "expcor_cls_apsr.rda"))
