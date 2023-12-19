## Automatisation for multiple linear regressions

With this function you can perform linear models with different distributions including a model selection which is selecting the best model candidate according to lowest akaikes information creterion (AIC)-value. According to the best model data are fitted and attached to the model object for visualisation.

This function requires these packages:
dplyr, stringr, MuMIn, MASS, lme4, lmerTest

and

a data.frame (or tibble) with the data you want to analyse.

### Usage:

`var <- modeler(df, fm_term = "y ~ x", model_type = "lm", predNo = 1, save = FALSE, filename = "auto")`

df = dataframe with variables to analyse

fm_term = full model: structured like a linear model lm(), mixed models are possible, too.

model type = one of: lm, glm_poisson, glm_logn, glm.nb, lmer, glmer.nb (you can modify the function to add additional models)

predNo = filter the for your analysis relevant variabes in the model selection. predNo = 1: 1 relevant predictor; predNo = 2: 2 relevant predictors, 0: the very best model. your relevant predictors have to be written after the "~" in the model term.

save = TRUE/FALSE,  if true it will be saved as rds file.

filname = "auto" If auto is selected (which is default) the name will be "response~pred1". You can pass an own name here.


### You obtain a list with the elements:

var$bm_term <- best model term/formula

data$fm_term <- full model term/formula

var$bm <- best model candidate, the actual model you obtain from e.g. glm_poisson

var$orig_data <- unmodified data, but only columns selected, which where in your model formular. Additionally excluded rows with missing values.

var$orig_data$fitted_y <- fitted response values

for mixed models also:

var$scaled_data <- for mixed models data are scaled. these data can be found here.
