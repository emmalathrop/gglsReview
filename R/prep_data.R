#' Title
#'
#' @param data data
#' @param x x
#' @param y y
#' @param groups groups
#'
#'
#' @return data
#' @import magrittr
#' @import dplyr
#' @export
#'
#' @examples prep_data(ToothGrowth, supp, len)
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

   return(data)
   invisible()

}

# ds <- prep_data(ToothGrowth, supp, len, dose)
# head(ds)
# ds2 <- prep_data(ToothGrowth, supp, len, groups = dose)
# head(ds2)

