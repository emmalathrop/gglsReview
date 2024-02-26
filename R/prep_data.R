prep_data <- function(data, x, y, groups = NULL){
  #Prep datasets for the functions in this package, internal

   data <-  data %>%
     dplyr::mutate(x = as.factor({{ x }}),
                   y = {{ y }})

   groups = substitute(groups)

  if (!is.null(groups)){ #if groups are included
  #  groups = substitute(groups)
     data <- data %>%
       dplyr::mutate(groups = as.factor({{ groups }}))

  }

}

ds <- prep_data(ToothGrowth, supp, len, dose)
head(ds)
ds2 <- prep_data(ToothGrowth, supp, len)
head(ds2)
#something is wrong with the groups again
