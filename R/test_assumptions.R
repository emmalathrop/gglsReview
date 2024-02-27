#' Test Assumptions
#'
#' @param data The dataset being analyzed
#' @param x X variable
#' @param y Y (response) variable
#' @param groups Grouping variable, if applicable. Default = NULL
#' @param alpha Alpha for p-value evaluations. Default = 0.05
#' @param plots Choose whether to include diagnostic plots in the case that assumptions are not met. Default = T
#'
#' @return
#' @export
#'
#' @examples test_assumptions(ToothGrowth, supp, len, dose, alpha = 0.01,plots = F)
test_assumptions <- function(data,
                             x, #not in quotes
                             y, #not in quotes
                             groups = NULL, #not in quotes
                             alpha = 0.05,
                             plots = T #Displaying diagnostic plots
                             ){

  #library(magrittr)
  #library(ggplot2)
    out <- matrix(nrow = 2, ncol = 2)
    colnames(out) <- c("Assumption", "Decision")


    data <-  data %>%
      dplyr::mutate(x = as.factor({{ x }}),
                    y = {{ y }})

    groups = substitute(groups)

   # ds <- prep_data(data, x, y, groups)

   #Data prepping steps now in prep_data function, need to figure that out better
     if (!is.null(groups)){
     #  data <- prep_data(data, x, y, groups)
        data <- data %>%
          dplyr::mutate(groups = as.factor({{ groups }}))
       }
   #  else {
   #    data <- prep_data(data, x, y)
   #  }

   st <- data %>% #shapiro test for normality
      dplyr::group_by(x)%>%
      rstatix::shapiro_test(y)

  if (any(st$p < alpha)){

    out[1,] <- c("Shapiro Normality", "Fail")
    print("Shapiro Test indicates non-normal distribution in the data")

  }
  else {
    out[1,] <- c("Shapiro Normality", "Pass")
    print("Shapiro Test indicates normal distribution in the data")
  }

   #Show a plot of the distribution

   if (plots==T){
   dist <- ggplot2::ggplot(data, aes(sample = y))+
     stat_qq()+
     stat_qq_line()+
     labs(title = "Normal Probability Plot")
   print(dist)
   }
  #Testing equal variances for ungrouped data
   groups <- substitute(groups)

  if (is.null(groups)){

    #for ungrouped data
    vt <- data %>%
      rstatix::levene_test(formula = y ~ x)

    if (vt$p > alpha){
      out[2,] <- c("Levene Variance", "Pass")
      print("Variance Test indicates equal variances in the data")
    }
    else {
      out[2,] <- c("Levene Variance", "Fail")
      print("Variance Test indicates unequal variances in the data")
    }

    data.aov <- aov(y~x, data = data)

    if (plots == T){
    print(plot(data.aov, 1)) #Look at residuals
    } #plots

    }
  else { #grouped data
    vt <- data %>%
      dplyr::group_by(groups)%>%
      rstatix::levene_test(formula = y ~ x)

   # vt <- leveneTest(formula = )

    if (any(vt$p < alpha)){
      out[2,] <- c("Levene Variance", "Fail")
      print("Variance Test indicates unequal variances in the data")
      print(vt)
    }
    else {
      out[2,] <- c("Levene Variance", "Pass")
      print("Variance Test indicates equal variances in the data")
    }

    if (plots == T){
      data.aov <- aov(y~x*groups, data = data)
      print(plot(data.aov, 1))
    } #plots
  }
 return(out)
}

# data("ToothGrowth")
test_assumptions(ToothGrowth, supp, len, dose, alpha = 0.01,plots = F)



