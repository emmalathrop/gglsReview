#' Comparison Maker
#'
#' @param data The dataset you are inputting
#' @param groups The column name that contains the groups you are wanting to compare
#' @param comparisons Default is "all". If you do not want to compare all variables to each other within your grouping column, then you can specify which comparisons you want instead.
#'
#' @return A list of comparisons to be made
#' @export
#'
#' @examples
#' input <- comparison_maker(ds, "Sample")
#' plot(ds, aes(x=Sample, y=Values))+
#'  geom_boxplot()+
#'  stat_compare_means(comparisons = input)
comparison_maker <- function(data,
                             x,
                             comparisons = "all"){
    x <- enquo(x)

    if (comparisons == "all"){

    comps <- offset <- unname(unlist(unique(select(data, groups))))
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
