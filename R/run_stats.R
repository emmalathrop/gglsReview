run_stats <- function(data,
         x, #not in quotes
         y, #not in quotes
         groups = NULL, #not in quotes
         test = "suggested", #If "suggested", this function will choose a test based on the data types. Other options: "linear", "logistic", "paired t", "independent t", "ANOVA", "MANOVA", "Pearson", "Spearman", "Chi square", "Kruskal-Wallis", "ANOSIM", "Wilcoxon Rank-Sum", "Wilcoxon Signed-Rank"
         interactions = T, #Choose if you want to test interactions between x and grouping variable
         parametric = T

){

  if (test == "linear" & is.null(groups)){

    summary(lm(y~x, data))

  } #if test = linear and no groups

  if (test == "linear" & !is.null(groups)){

    if (interactions ==T){

      summary(lm(y~x*groups, data))

    } #if test = linear and yes groups and yes interactions

    if (interactions ==F){

      summary(lm(y~x+groups, data))

    } #if test = linear and yes groups and yes interactions

  }#If linear and groups != NULL

} #Close function brackets
