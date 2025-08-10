# Variance Calculation ------------------------------------------------------------------

## Beck-Katz panel-corrected standard errors

vcovBK_se <- function(x) {
    plm::vcovBK(x, cluster = "time") %>% 
        diag() %>% 
        sqrt()
}


vcovHC_se <-  function(x) {
    plm::vcovHC(x, method="arellano", cluster="group") %>%  #default setting
        diag() %>% 
        sqrt()
}


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



get_legend<-function(myggplot){
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}


# Method of composition ------------------------------------------------------------

vcovBK <- function(mod) {
    plm::vcovBK(mod, cluster = "time") 
}

plmHC <- function (mod) {
    plm::vcovHC(mod, method="arellano", cluster="group")
}


## For Ajps pooled ols, using vcov=vcovBK
methodComposition1 <- function (data, model, vcov=vcovBK, rsq=TRUE) {
    aData <- data[[1]]
    aMod <- eval(parse(text = model))
    
    coefdf <- as.data.frame(matrix(nrow=length(data), ncol=length(coef(aMod))))
    R2 <- data.frame(rsq=rep(NA, length(data)), adjrsq=rep(NA, length(data)))
    
    for (s in seq_along(data)) {
        ## (1) Sample from p(x)
        data_sample <- data[[s]]
        ## (2) Sample from p(B|x,y):
        ##     (a) Estimate B_s and Cov(B_s) conditional on x_s.
        mod_sample <- update(aMod, data=data_sample)
        hatB_sample <- coef(mod_sample)
        hatV_sample <- vcov(mod_sample)
        ##     (b) Sample \data_sample{B_s} from MV(\hat{B_sample}, \hat{Cov(B_sample)}).
        coefdf[s, ] <- MASS::mvrnorm(n = 1, mu = hatB_sample, Sigma = hatV_sample)
        if (rsq) {
            R2$rsq[s] <- summary(mod_sample)$r.squared["rsq"]
            R2$adjrsq[s] <- summary(mod_sample)$r.squared["adjrsq"]
        }
    }
    names(coefdf) <- names(coef(aMod))
    if (rsq) {
        coefdf$rsq <- R2$rsq
        coefdf$adjrsq <- R2$adjrsq
    }
    return(coefdf)
}


## For AJPS GMM model, using plmHC. 
methodComposition2 <- function (data, model, vcov=plmHC, rsq=TRUE) {
    aData <- data[[1]]
    aMod <- eval(parse(text = model))
    
    coefdf <- as.data.frame(matrix(nrow=length(data), ncol=length(coef(aMod))))
    R2 <- data.frame(rsq=rep(NA, length(data)), adjrsq=rep(NA, length(data)))
    
    for (s in seq_along(data)) {
        data_sample <- data[[s]]
        mod_sample <- update(aMod, data=data_sample)
        hatB_sample <- coef(mod_sample)
        hatV_sample <- vcov(mod_sample)
        coefdf[s, ] <- MASS::mvrnorm(n = 1, mu = hatB_sample, Sigma = hatV_sample)
        
    }
    names(coefdf) <- names(coef(aMod))
    return(coefdf)
}

## For APSR ECM and First Difference models, using plmhc. 
methodComposition3 <- function (data, model, vcov=plmHC, rsq=TRUE) {
    #df_dcpoUncertainAPSR <- data[[1]]
    aData <- data[[1]]
    aMod <- eval(parse(text = model))
    
    coefdf <- as.data.frame(matrix(nrow=length(data), ncol=length(coef(aMod))))
    R2 <- data.frame(rsq=rep(NA, length(data)), adjrsq=rep(NA, length(data)))
    
    for (s in seq_along(data)) {
        data_sample <- data[[s]]
        mod_sample <- update(aMod, data=data_sample)
        hatB_sample <- coef(mod_sample)
        hatV_sample <- vcov(mod_sample)
        coefdf[s, ] <- MASS::mvrnorm(n = 1, mu = hatB_sample, Sigma = hatV_sample)
        if (rsq) {
            R2$rsq[s] <- summary(mod_sample)$r.squared["rsq"]
            R2$adjrsq[s] <- summary(mod_sample)$r.squared["adjrsq"]
        }
    }
    names(coefdf) <- names(coef(aMod))
    if (rsq) {
        coefdf$rsq <- R2$rsq
        coefdf$adjrsq <- R2$adjrsq
    }
    return(coefdf)
}

## Moc summary

pEmp <- function (x) {
    p_pos <- mean(x < 0)
    p_neg <- mean(x > 0)
    p2side <- 2 * min(p_pos, p_neg)
    p2side
}
pNorm <- function (x) {
    z <- abs(mean(x)/sd(x))
    p2side <- 2*(1 - pnorm(z))
    p2side
}


### For AJPS model summary. 
MOCsumm <- function (moc, digits=3,
                     ff=funs(est = mean, se = sd, z = mean(.)/sd(.),
                             pnorm = pNorm(.),
                             pemp = pEmp(.))) {
    
    var_names <- names(moc) %>% 
        str_c(collapse = "|")
    
    summarise_all(moc, .funs=ff) %>%
        mutate_all(funs(round(., digits=digits))) %>%
        reshape2::melt(measure.vars=names(.)) %>%
        separate(variable, into = c("term","temp"),sep ="_(?=est|se|z|pnorm|pemp)") %>%
        pivot_wider(names_from = "temp",values_from = "value") %>%
        transmute(term = term,
                  estimate = est,
                  std.error = se) %>%
        filter(!term =="rsq"&!term =="adjrsq" )
}


### For APSR model Summaries
MOCsumm2 <- function (moc, digits=3,
                      ff=funs(est = mean, se = sd)) {
    var_names <- names(moc) %>% 
        str_c(collapse = "|")
    
    summarise_all(moc, .funs=ff) %>%
        mutate_all(funs(round(., digits=digits))) %>%
        reshape2::melt(measure.vars=names(.)) %>%
        separate(variable, into = c("term","temp"),sep ="_(?=est|se)") %>%
        pivot_wider(names_from = "temp",values_from = "value") %>%
        transmute(term = term,
                  estimate = est,
                  std.error = se) %>%
        filter(!term =="rsq"&!term =="adjrsq" )
}


# Tabulation -----------------------------------------------------------------------


na_types_dict <- list("r" = NA_real_,
                      "i" = rlang::na_int,
                      "c" = NA_character_,
                      "l" = rlang::na_lgl)

# A function that converts a string to a vector of NA types.
# e.g. "rri" -> c(NA_real_, NA_real_, rlang::na_int)
parse_na_types <- function(s) {
    
    positions <- purrr::map(
        stringr::str_split(s, pattern = ""), 
        match,
        table = names(na_types_dict)
    ) %>%
        unlist()
    
    na_types_dict[positions] %>%
        unlist() %>%
        unname()
}

# A function that, given named arguments, will make a one-row
# tibble, switching out NULLs for the appropriate NA type.
as_glance_tibble <- function(..., na_types) {
    
    cols <- list(...)
    
    if (length(cols) != stringr::str_length(na_types)) {
        stop(
            "The number of columns provided does not match the number of ",
            "column types provided."
        )
    }
    
    na_types_long <- parse_na_types(na_types)
    
    entries <- purrr::map2(cols, 
                           na_types_long, 
                           function(.x, .y) {if (length(.x) == 0) .y else .x})
    
    tibble::as_tibble_row(entries)
    
}

tidy.pgmm <- function(x,
                      conf.int = FALSE,
                      conf.level = 0.95,
                      ...) {
    result <- summary(x)$coefficients %>%
        tibble::as_tibble(rownames = "term") %>%
        dplyr::rename(
            estimate = Estimate,
            std.error = `Std. Error`,
            statistic = `z-value`,
            p.value = `Pr(>|z|)`
        )
    
    if (conf.int) {
        ci <- confint(x, level = conf.level) %>% 
            as.data.frame() %>% 
            rownames_to_column(var = "term") %>% 
            dplyr::rename(
                conf.low = `2.5 %`,
                conf.high = `97.5 %`
            )
        result <- dplyr::left_join(result, ci, by = "term")
    }
    
    result
}

glance.plm <- function(x, ...) {
    s <- summary(x)
    as_glance_tibble(
        nobs = stats::nobs(x),
        n.country = pdim(x)$nT$n,
        na_types = "ii"
    )
}


glance.pgmm <- function(x, ...) {
    s <- summary(x)
    as_glance_tibble(nobs = stats::nobs(x),
        n.country = pdim(x)$nT$n,
        n.inst = dim(x$W[[1]])[2],
        na_types = "iii"
    )
}
