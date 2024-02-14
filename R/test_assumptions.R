# #Common Assumptions:
# #Equal variance
# #Normality
#
# #Testing normality (changes the tests for variance):
# #If sample is large enough (n > 30), we can use the Central Limit Theorem to say that the distribution is normal enough. Let's not go that route though. I want to test for normality and warn the user if this is not true.
# #
# #If one variable, use shapiro test
#
#
#
library(magrittr)
test_assumptions <- function(data,
                             x,
                             y,
                             groups = NULL,
                             alpha = 0.05){

   data <-  data %>%
     dplyr::mutate(x = as.factor({{ x }}),
                   y = {{ y }})

   if (!is.null(groups)){ #if groups are included
     data <- data %>%
       dplyr::mutate(groups = as.factor({{ groups }}))
     }
 # if(!is.factor(data[[groups]])) data[[groups]] <- as.factor(data[[groups]])
 # if(!is.factor(data[[x]])) {data[[x]] <- as.factor(data[[x]])}
#  if(!is.factor(data[[y]])) {data[[y]] <- as.factor(data[[y]])}

   st <- data %>% #shapiro test for normality
      dplyr::group_by(x)%>%
      rstatix::shapiro_test(y)

  if (any(st$p < alpha)){
    print("Shapiro Test indicates non-normal distribution in the data")

  }
  else {
    print("Shapiro Test indicates normal distribution in the data")
  }

   #Show a plot of the distribution
   dist <- ggplot2::ggplot(data, aes(sample = y))+
     stat_qq()+
     stat_qq_line()+
     labs(title = "Normal Probability Plot")
   print(dist)

  #Testing equal variances for ungrouped data
  if (is.null(groups)){

    #for ungrouped data
    vt <- data %>%
      rstatix::levene_test(formula = y ~ x)

    if (vt$p > alpha){
      print("Variance Test indicates equal variances in the data")
    }
    else {
      print("Variance Test indicates unequal variances in the data")
    }

    data.aov <- aov(y~x, data = data)
    print(plot(data.aov, 1)) #Look at residuals


    }
  else { #grouped data
    vt <- data %>%
      dplyr::group_by(groups)%>%
      rstatix::levene_test(formula = y ~ x)

   # vt <- leveneTest(formula = )

    if (vt$p > alpha){
      print("Variance Test indicates equal variances in the data")
    }
    else {
      print("Variance Test indicates unequal variances in the data")
    }

    data.aov <- aov(y~x*groups, data = data)
    print(plot(data.aov, 1))

  }

}

# data("ToothGrowth")
# test_assumptions(ToothGrowth, supp, len, dose)




