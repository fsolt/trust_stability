# 0. Set ================================================
rm(list=ls())

library(pacman)

p_load(
    tidyverse,
    rio,
    here,
    plm,
    countrycode,
    readr,
    purrr,
    tidyr,
    dotwhisker,
    glue
)

source(here("data", "customFunctions.R"))

theme_set(theme_bw())

## dotwhisker::small_multiple() hacks
body(small_multiple)[[19]] <- substitute(
    p <-
        ggplot(
            df,
            aes(
                y = estimate,
                ymin = conf.low,
                ymax = conf.high,
                x = as.factor(model),
                colour = submodel
            )
        ) +
        do.call(geom_pointrange, point_args) +
        ylab("") + xlab("") +
        facet_grid(
            term ~ .,
            scales = "free",
            labeller = label_parsed,
            # enable LaTeX facet labels
            switch = "y"
        ) +             # put facet labels on left
        scale_y_continuous(position = "right") # put axis label on right
)


# 1. Calling the data

    #Discontent
    load(file = here("data", "theta_results.rda"))
    theta_results <- theta_results %>% rename(discontent = theta)
    discontent_results <- split(theta_results, theta_results$draw)
    
    set.seed(123)  # for reproducibility
    discontent_results <- discontent_results[sample(length(discontent_results), 900)] # The country control has 900 elements
    
    #Macro-Interest
    load(file = here("data", "interest_theta_results.rda"))
    theta_results <- theta_results %>% rename(interest = theta)
    interest_results <- split(theta_results, theta_results$draw)
    
    set.seed(123)  # for reproducibility
    interest_results <- interest_results[sample(length(interest_results), 900)] # The country control has 900 elements
    
    rm(theta_results)
    
    #Country control (from Tai et al (2022))
    exp_ajps_cntrl_list <- readRDS(here("data", "exp_ajps_cntrl_list.rds"))
    
    data <-map2(discontent_results, interest_results, \(data1, data2){
        data1 %>% left_join(data2, by = c("country", "year")) %>% 
            dplyr::select(-draw.x, -draw.y)
    })
    
    data <- map2(data, exp_ajps_cntrl_list, \(data1, data2) {
        data1 %>% 
            left_join(data2, by = c("country", "year")) %>% 
            mutate(
                discontent_dem_trim = case_when(
                    is.na(discontent) ~ NA,
                    Regime_VD > 1 & !is.na(discontent) ~ discontent,
                    TRUE ~ 0
                ),
                discontent_aut_trim = case_when(
                    is.na(discontent) ~ NA,
                    Regime_VD <= 1 & !is.na(discontent) ~ discontent,
                    TRUE ~ 0
                ),
                interest_dem_trim = case_when(
                    is.na(interest) ~ NA,
                    Regime_VD > 1 & !is.na(interest) ~ interest,
                    TRUE ~ 0
                ),
                interest_aut_trim = case_when(
                    is.na(interest) ~ NA,
                    Regime_VD <= 1 & !is.na(interest) ~ interest,
                    TRUE ~ 0
                ),
            ) %>%
            dplyr::select(country, year, discontent, discontent_dem_trim, discontent_aut_trim,
                          interest, interest_dem_trim, interest_aut_trim, everything())
    })
    
    rm(exp_ajps_cntrl_list, discontent_results, interest_results)
    
# 2. Preliminary analysis (using the Tai et al's (2022) code)
    
    data <- purrr::map(1:900, function(anEntry) {
        data[[anEntry]] %>%
            plm::pdata.frame(index = c("country", "year"))
    })
    
    #Set up the model
    ls_iv <- c(
      "plm::lag(discontent, 1)+plm::lag(interest, 1)",
      "plm::lag(discontent_dem_trim, 1) + plm::lag(discontent_aut_trim, 1) + plm::lag(interest_dem_trim, 1) + plm::lag(interest_aut_trim, 1)"
    ) #use trimmed data

    ls_method <- c("plm", "pgmm")

    # ls_eq1 <-
    #   glue(
    #     "vdem_electoral_dem ~ plm::lag(vdem_electoral_dem, 1:2) + {ls_iv} +
    #     plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) +
    #     plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) + plm::lag(Res_cp_WDI_di, 1)"
    #   )

    ls_eq2 <-
      glue(
        "Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + {ls_iv} +
        plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) +
        plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) + plm::lag(Res_cp_WDI_di, 1)"
      )

    # ls_mod_ajpsCLS1 <- c(
    #   glue("plm({ls_eq1}, model = 'pooling', data = aData)"),
    #   glue(
    #     "pgmm({ls_eq1} | plm::lag(vdem_electoral_dem, 3:5), data = aData, 
    #     effect='individual', model='onestep', transformation='ld', indexes=c('country', 'year'))"
    #   )
    # )

    ls_mod_ajpsCLS2 <- c(
      glue("plm({ls_eq2}, model = 'pooling', data = aData)"),
      glue(
        "pgmm({ls_eq2} | plm::lag(Libdem_VD, 3:5), data = aData, effect='individual', model='onestep', transformation='ld', indexes=c('country', 'year'))"
      )
    )

    #Electoral democracy
    # result_correct_clsUncertain1 <-
    #   map(ls_mod_ajpsCLS1[1:2], function(aMod){
    #     result <- methodComposition1(data, aMod)
    #   })
    # names(result_correct_clsUncertain1) <- c("pooled", "pooled-regime")
    # 
    # result_correct_clsUncertain_gmm1 <-
    #   map(ls_mod_ajpsCLS1[3:4], function(aMod){
    #     result <- methodComposition2(data, aMod)
    #   })
    # names(result_correct_clsUncertain_gmm1) <- c("gmm", "gmm-regime")

    #Liberal democracy
    result_correct_clsUncertain2 <-
      map(ls_mod_ajpsCLS2[1:2], function(aMod){
        result <- methodComposition1(data, aMod)
      })
    names(result_correct_clsUncertain2) <- c("pooled", "pooled-regime")

    result_correct_clsUncertain_gmm2 <-
      map(ls_mod_ajpsCLS2[3:4], function(aMod){
        result <- methodComposition2(data, aMod)
      })
    names(result_correct_clsUncertain_gmm2) <- c("gmm", "gmm-regime")

    #result1 <- append(result_correct_clsUncertain1, result_correct_clsUncertain_gmm1)
    result2 <- append(result_correct_clsUncertain2, result_correct_clsUncertain_gmm2)

    #saveRDS(result1, "results1.rds")
    saveRDS(result2, "results2.rds")
    
    result_pooled <- bind_rows(
        mutate(MOCsumm(result_correct_clsUncertain2[["pooled"]]), submodel = "Uncertainty")
    ) %>%
        mutate(type = "Pooled~OLS",
               model = "Model 1 (Pooled OLS)")
    
    result_gmm <- bind_rows(
        mutate(MOCsumm(result_correct_clsUncertain_gmm2[["gmm"]]), submodel = "Uncertainty")
    ) %>%
        mutate(type = "System~GMM",
               model = "Model 3 (System GMM)")
    
    result_regime <- bind_rows(
        mutate(MOCsumm(result_correct_clsUncertain2[["pooled-regime"]]), submodel = "Uncertainty")
    ) %>%
        mutate(type = "Pooled~OLS",
               model = "Model 2 (Pooled OLS)")
    
    result_regimeGmm <- bind_rows(
        mutate(MOCsumm(result_correct_clsUncertain_gmm2[["gmm-regime"]]), submodel = "Uncertainty")
    ) %>%
        mutate(type = "System~GMM",
               model = "Model 4 (System GMM)")
    
    result_pooledAJPS <- bind_rows(result_pooled, result_gmm) %>%
        filter(term != "(Intercept)") %>%
        mutate(
            term0 = rep(
                c(
                    "Democracy\n (t-1)",
                    "Democracy\n (t-2)",
                    "Political Discontnet(t-1)",
                    "Log GDP\n per capita\n (t-1)",
                    "GDP per\n capita growth\n (t-1)",
                    "Regional\n democracy\n (t-1)",
                    "Percent\n Muslim\n (t-1)",
                    "Resource\n dependence\n (t-1)"
                ),
                time = 2
            ),
            type = factor(type, levels = c("Pooled~OLS", "System~GMM")),
            model = factor(model, levels = paste0("Model ", c(1,3), 
                c(" (Pooled OLS)", " (System GMM)")
            ))
        )
    
    result_regimeAJPS <- bind_rows(result_regime, result_regimeGmm) %>%
        filter(term != "(Intercept)") %>%
        mutate(
            term0 = rep(
                c(
                    "Democracy\n (t-1)",
                    "Democracy\n (t-2)",
                    "Political discontent\n demo only\n (t-1)",
                    "Political discontent\n auto only\n (t-1)",
                    "Log GDP\n per capita\n (t-1)",
                    "GDP per\n capita growth\n (t-1)",
                    "Regional\n democracy\n (t-1)",
                    "Percent\n Muslim\n (t-1)",
                    "Resource\n dependence\n (t-1)"
                ),
                time = 2
            ),
            type = factor(type, levels = c("Pooled~OLS", "System~GMM")),
            model = factor(model, levels = paste0("Model ", c(2,4), rep(
                c(" (Pooled OLS)", " (System GMM)"), each = 1
            )))
        )
    
    
    index_coefName <- c(
        `Democracy\n (t-1)` = "Democracy[t-1]",
        `Democracy\n (t-2)` = "Democracy[t-2]",
        `Political Discontnet(t-1)` = "Political~Discontent[t-1]~(all~regimes)",
        `Political discontent\n demo only\n (t-1)` = "Political~Discontent[t-1]~(democracies)",
        `Political discontent\n auto only\n (t-1)` = "Political~Discontent[t-1]~(autocracies)",
        `Log GDP\n per capita\n (t-1)` = "Log~GDP~per~capita[t-1]",
        `GDP per\n capita growth\n (t-1)` = "GDP~per~capita~Growth[t-1]",
        `Regional\n democracy\n (t-1)` = "Regional~Democracy[t-1]",
        `Percent\n Muslim\n (t-1)` = "Percent~Muslim[t-1]",
        `Resource\n dependence\n (t-1)` = "Resource~Dependence[t-1]"
    )
    
    df_plot <- bind_rows(result_pooledAJPS, result_regimeAJPS) %>%
        mutate(term = index_coefName[term0],
               term = factor(term, levels = unique(index_coefName)))
    
    small_multiple(df_plot, dot_args = list(size = .9, fatten = 1)) +
        theme_bw() +
        geom_hline(yintercept = 0,
                   colour = "grey50",
                   linetype = 2) +
        scale_color_grey(
            start = 0, end = 0.4, 
            name = "Replication",
            breaks = c(
                "Point Estimates Only",
                "With Uncertainty",
                "Uncertainty & More Data"
            ),
            labels = c(
                "Point Estimates Only",
                "With Uncertainty",
                "Uncertainty & More Data"
            )
        ) + 
        theme(
            axis.text.x  = element_text(angle = 90, hjust = .5),
            strip.text.y.left = element_text(angle = 0),
            legend.position = c(-0.06, -.01),
            legend.justification = c(1, 1),
            legend.title = element_text(size = 9, face = "bold"),
            legend.title.align = 0.5,
            legend.text = element_text(size = 9),
            legend.background = element_rect(color = "gray90"),
            legend.spacing = unit(-5, "pt"),
            legend.key.size = unit(8, "mm"),
            plot.title = element_text(hjust = 1.4),
            plot.caption = element_text(
                size = 10,
                hjust = 0.2,
                margin = margin(t = 15)
            )
        ) +
        guides(color = guide_legend(override.aes = list(size = .3))) +
        ggtitle("Outcome Variable: Level of Democracy")
