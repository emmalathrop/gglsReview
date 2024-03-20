
#For `run_stats.R`
library(magrittr)

run_stats(ToothGrowth, supp, len, dose, test =
           "ANOVA", interactions = F)

#For `add_letters.R`
library(ggplot2)
library(magrittr)

ggplot(ToothGrowth) +
  geom_boxes_wletters(len ~ supp, data = ToothGrowth, test =
                        "ANOVA", interactions = F)

ggplot(mpg) +
  geom_boxes_wletters(data = mpg, hwy ~ class, fsize = 18)


#For `suggest_test.R`
ds <- test_assumptions(ToothGrowth, supp, len, dose)

test <- ToothGrowth %>%
  dplyr::mutate(supp = as.factor(supp),
                len = len,
                dose = factor(dose))

is.factor(ToothGrowth$dose)

#issues: currently x is always treated as a factor, come back to this

#For `test_assumptions.R`

test_assumptions(ToothGrowth, supp, len, dose, alpha = 0.01,plots = F)

