#####################################################################################
# A function to perform a model selection till                                      #
# the best model candidate output is printed                                        #
#                                                                                   #
# Supported models:                                                                 #
# lm, glm (poisson), glm.nb, glmer.nb, lmer                                         #
#                                                                                   #
# Mixed models work only with one random factor, or two crossed (factor1|factor2).  #
# Mixed model data will be always scaled.                                           #
#                                                                                   #
# written by Jan Helbach                                                            #
# Februar 2021                                                                      #
#####################################################################################

library(dplyr)
library(stringr)
library(MuMIn)
library(MASS)
library(lme4)
library(lmerTest)

# debugging unit, 
# rm(list = ls())
# path = "/home/jan/Uni/research_project/data"
# setwd(path)
# x <- read.csv(sort(list.files(path = path, pattern = "all_plot"), decreasing = T)[1], stringsAsFactors = T)
# fm_term = "SR_herb_agg ~ DLI_cv * DLI * pH * pH_cv + (1|phyt_clust4)"
# fm_term = "SR_herb ~ DBHcv + (1|phyt_clust4)"
# bm_term = "SR_herb_agg ~ DLI_cv + DLI + CNratio + CNratio_cv + DLI_cv:CNratio_cv + (1|phyt_clust4)" # das ist das interssante model für H3/2
# df = x
# predNo=1
# model_type = "glmer.nb"


ms <- function(df, fm_term = "y ~ x", model_type = "lm", predNo = 1, save = FALSE, filename = "auto", save_ms = FALSE, ...) {
  
  # define necessary variables
  fm_term_elemts <- unlist(str_split(fm_term, " "))
  response <- fm_term_elemts[1]
  pred1 <- fm_term_elemts[3]
  pred2 <- fm_term_elemts[5]
  # random effect in mixed effect models
  if (str_detect(fm_term_elemts[length(fm_term_elemts)], pattern = "\\|")) {
    random_factor <- fm_term_elemts[length(fm_term_elemts)]
    nesting_variable <- unlist(str_split(random_factor, pattern = "\\|"))
    nesting_variable <- str_replace(nesting_variable, pattern = "[()]", "")
    nesting_variable <- nesting_variable[str_detect(nesting_variable, "1", negate = T)]
  }
  
  # prapare the data.frame
  fm_vars <- unique(unlist(str_split(fm_term, "[~*\\+:]"))) %>% str_trim()
  if (str_detect(fm_term_elemts[length(fm_term_elemts)], pattern = "\\|")) {fm_vars <- c(fm_vars[1:(length(fm_vars)-1)], nesting_variable)}
  df <- df %>% dplyr::select(all_of(fm_vars))
  df <- df[complete.cases(df), ]
  
  # compute the full model
  if (model_type == "lm") {fm <- lm(fm_term, data = df, na.action = na.fail)}
  else if (model_type == "glm.nb") {fm <- glm.nb(fm_term, data = df, na.action = na.fail)}
  else if (model_type == "glm_poisson") {fm <- glm(fm_term, family = "poisson", data = df, na.action = na.fail)}
  else if (model_type == "glm_logn") {fm <- glm(fm_term, family = "gaussian"(link='log'), data = df, na.action = na.fail)}
  else if (model_type == "glmer.nb") {
    df_sc <- df %>% mutate(across(where(is.numeric), scale))
    df_sc[1] <- df[1]
    fm <- glmer.nb(fm_term, data = df_sc, na.action = na.fail,
                   control = glmerControl(optimizer = "Nelder_Mead", optCtrl = list(maxfun = 10000)))}
  else if (model_type == "lmer") {
    df_sc <- df %>% mutate(across(where(is.numeric), scale))
    df_sc[1] <- df[1]
    fm <- lmer(fm_term, REML = F, data = df_sc, na.action = na.fail)}  
  else if (model_type == "glmer_logn") {
    df_sc <- df %>% mutate(across(where(is.numeric), scale))
    df_sc[1] <- df[1]
    fm <- glmer(fm_term, family = "gaussian"(link='log'), REML = F, data = df_sc, na.action = na.fail)}
  # print(summary(fm))
  
  # model selection
  ms <- dredge(fm, evaluate = T, rank = "AICc")
  
  # filter the relevant variables from the model selection. predNo = 1: 1 relevant predictor; predNo = 2: 2 relevant predictors, 0-> the very best model
  if (predNo == 1) {bm_vars <- ms %>% filter(!is.na(get(pred1))) %>% arrange(AICc) %>% head(1)}
  else if (predNo == 2) {bm_vars <- ms %>% filter(!is.na(get(pred1)) & !is.na(get(pred2))) %>% arrange(AICc) %>% head(1)}
  else if (predNo == 0) {bm_vars <- ms %>% arrange(AICc) %>% head(1)}
  bm_vars <- names(bm_vars)[which(!is.na(bm_vars))]
  bm_vars <- bm_vars[2:(length(bm_vars)-5)]
  
  # make a string of the best model formula
  bm_term <- str_c(bm_vars, collapse = " + ")
  bm_term <- paste(response, bm_term, sep = " ~ ")
  if (str_detect(fm_term_elemts[length(fm_term_elemts)], pattern = "\\|")) {bm_term <- paste(bm_term, random_factor, sep = " + ")} # considers the random factors if available (considers only one).
  
  # compute the best model candidate
  if (model_type == "lm") {bm <- lm(bm_term , data = df)}
  else if (model_type == "glm.nb") {bm <- glm.nb(bm_term , data = df)}
  else if (model_type == "glm_poison") {bm <- glm(bm_term , family = "poisson", data = df)}
  else if (model_type == "glm_logn") {bm <- glm(bm_term , family = "gaussian"(link='log'), data = df)}
  else if (model_type == "glmer.nb") {
    df_sc <- df %>% mutate(across(where(is.numeric), scale))
    df_sc[1] <- df[1]
    bm <- glmer.nb(bm_term, data = df_sc,
                   control = glmerControl(optimizer = "Nelder_Mead", optCtrl = list(maxfun = 10000)))}
  else if (model_type == "lmer") {
    df_sc <- df %>% mutate(across(where(is.numeric), scale))
    df_sc[1] <- df[1]
    bm <- lmer(bm_term, REML = F, data = df_sc)}
  else if (model_type == "glmer_logn") {
    df_sc <- df %>% mutate(across(where(is.numeric), scale))
    df_sc[1] <- df[1]
    bm <- glmer(bm_term, family = "gaussian"(link='log'), REML = F, data = df_sc, na.action = na.fail)}
  # report of the model
  print(bm_term)
  print(summary(bm))
  
  # prepare data
  data <- list()
  data$bm_term <- bm_term
  data$bm <- bm
  data$orig_data <- df
  data$orig_data$fitted_y <- fitted(bm)
  if (save_ms) {data$ms <- ms} # save the model selection table, if desired
  try(data$scaled_data <- df_sc, silent = T)
  
  
  
  # save the data if desired
  if (save) {
    if (filename == "auto") {filename <- paste(response, pred1, sep = "~")}
    saveRDS(data, file = paste0(filename, ".rds"))
  }
  return(data)
}