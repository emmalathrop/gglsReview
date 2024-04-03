#' Comparison Maker
#'
#' @param data The dataset you are inputting
#' @param x The column name that contains the groups you are wanting to compare
#' @param comparisons Default is "all". If you do not want to compare all variables to each other within your grouping column, then you can specify which comparisons you want instead.
#'
#' @return comparisons
#' @export
#'
#' @examples comparison_maker(ToothGrowth, supp)
comparison_maker <- function(data,
                             x,
                             comparisons = "all"){
   # x <- enquo(x) come back to this

  data <-  data %>%
    dplyr::mutate(x = as.factor({{ x }}))

    if (comparisons == "all"){

 #   comps <- offset <- as.character(unique(dplyr::select(data, x)))

    comps <- offset <- as.character(unique(data$x))
    comparisons <- list() #Empty comparisons list

    for (i in 1:length(comps)){

      offset <- c(NA, offset)[1:length(offset)]

      for (j in 1:length(offset)){

        if (!is.na(offset[j])){
          this_comp <- list(c(comps[j], offset[j]))
          comparisons <- c(comparisons, this_comp)
        } #close the if statement

      }#Close the offset for loop

    } #close the comparisons for loop

    print(comparisons)

    }#close the if comparisons == all loop

    else { #if user does not choose "all" for comparisons and instead provides comparisons

    print(comparisons)

     }


  }
