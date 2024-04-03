#' Title
#'
#' @param data dataset
#' @param x x
#' @param y y
#' @param groups groups
#' @param test test
#' @param interactions ints
#' @param parametric parametric
#'
#' @import magrittr
#' @import dplyr
#' @import emmeans
#' @import rstatix
#' @import stats
#' @import multcomp
#'
#' @return out
#' @export
#'
#' @examples run_stats(ToothGrowth, supp, len)
run_stats <- function(data,
         x, #not in quotes
         y, #not in quotes
         groups = NULL, #not in quotes
         test = "ANOVA", #Change default back to suggested once that works.  If "suggested", this function will choose a test based on the data types. Other options: "linear", "logistic", "paired t", "independent t", "ANOVA", "MANOVA", "Pearson", "Spearman", "Chi square", "Kruskal-Wallis", "ANOSIM", "Wilcoxon Rank-Sum", "Wilcoxon Signed-Rank"
         interactions = T, #Choose if you want to test interactions between x and grouping variable
         parametric = T

){

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

  if (test == "linear" & is.null(groups)){

    res <- summary(stats::lm(y~x, data))
    print(res)

  } #if test = linear and no groups

  if (test == "linear" & !is.null(groups)){

    if (interactions ==T){

      res <- summary(stats::lm(y~x*groups, data))
      print(res)

    } #if test = linear and yes groups and yes interactions

    if (interactions ==F){

      res <- summary(stats::lm(y~x+groups, data)) #what about if multiple groups?
      p <- unlist(res$coefficients)[2,4]
      print(res)

    } #if test = linear and yes groups and yes interactions

  }#If linear and groups != NULL

  if (test == "ANOVA" & is.null(groups)){

    res <- summary(stats::lm(y~x, data))
    #print(res)
    out <- unname(unlist(stats::TukeyHSD(aov(y~factor(x), data))))[4]

  } #ANOVA, no groups

  if (test == "ANOVA" & !is.null(groups)){

    if (interactions ==T){

      m3 <- stats::lm(y ~ x * groups, data=data)
      out <- emmeans::emmeans(m3, spec= ~x)  %>%
        multcomp::cld(Letters=letters)

    } #Interactions true

    if (interactions ==F){

      m3 <- lm(y ~ x + groups, data=data)
      out <- emmeans::emmeans(m3, spec= ~x)  %>%
        multcomp::cld(Letters=letters)

    } #Interactions false

  } #ANOVA, yes groups

  return(out)

} #Close function bracket
