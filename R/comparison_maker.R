#' Comparison Maker
#'
#' @param data The dataset you are inputting
#' @param groups The column name that contains the groups you are wanting to compare
#' @param comparisons Default is "all". If you do not want to compare all variables to each other within your grouping column, then you can specify which comparisons you want instead.
#'
#' @return comparisons
#' @export
#'
#' @examples something
comparison_maker <- function(data,
                             x,
                             comparisons = "all"){
   # x <- enquo(x) come back to this

  data <- droplevels(data$x)

    if (comparisons == "all"){

    comps <- offset <- unname(unlist(unique(dplyr::select(data, x))))
    comparisons <- list() #Empty comparisons list


    for (i in 1:length(comps)){

      offset <- c(NA, offset)[1:length(offset)]

      for (j in 1:length(offset)){

        if (!is.na(offset[j])){
          this_comp <- list(c(comps[[j]], offset[[j]]))
          comparisons <- c(comparisons, this_comp)
        } #close the if statement

      }#Close the offset for loop

    } #close the comparisons for loop

    }#close the if comparisons == all loop

    else { #if user does not choose "all" for comparisons and instead provides comparisons

    comparisons = comparisons

  }

  return(comparisons)

  }

#Below are the dependencies, need to figure out how to do this

#library(tidyverse)
# library(rstatix)
# library(ggpubr) #Testing this, may remove later
#
