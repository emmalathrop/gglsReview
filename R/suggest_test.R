suggest_test <- function(data,
                         x, #not in quotes
                         y, #not in quotes
                         groups = NULL){

#Test assumptions first
test <- test_assumptions(data, x, y, groups)

#data prep start
    data <-  data %>%
      dplyr::mutate(x = as.factor({{ x }}),
                    y = {{ y }})

    groups = substitute(groups)

   #Ideally data prep will be in prep_data function, need to figure that out better
     if (!is.null(groups)){
     #  data <- prep_data(data, x, y, groups)
        data <- data %>%
          dplyr::mutate(groups = as.factor({{ groups }}))
       }
#Data prep end


if (test[1,2] == "Pass" & test[2,2] == "Pass"){ #If pass normality and equality of variances tests

#  if (x)

} #if pass tests

}

