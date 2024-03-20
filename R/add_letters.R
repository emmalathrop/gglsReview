geom_boxes_wletters <- function(formula = NULL, data = NULL,
                           stat = "boxplot", position = "dodge",coef=1.5,
                           font = "sans", fsize = 18, width=0.6,
                           fun.data = NULL, fun.y = NULL, fun.ymax = NULL,
                           fun.ymin = NULL, fun.args = list(),
                           outlier.colour = NULL, outlier.color = NULL,
                           outlier.shape = 19, outlier.size = 1.5,outlier.stroke = 0.5,
                           notch = FALSE,  notchwidth = 0.5,varwidth = FALSE,
                           na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE,...) {
  vars <- all.vars(formula)
  response <- vars[1]
  factor <- vars[2]
  #group <- vars[3]
  mymap <- aes_string(x=factor,y=response)

  letters <- run_stats(data = data, x = factor, y = response, test = "ANOVA", interactions = F)

  position <- position_dodge(width)
  l1 <- layer(data = data, mapping = mymap, stat = stat, geom = GeomBoxplot,
              position = position, show.legend = show.legend, inherit.aes = inherit.aes,
              params = list(outlier.colour = outlier.colour, outlier.shape = outlier.shape,
                            outlier.size = outlier.size, outlier.stroke = outlier.stroke,
                            notch = notch, notchwidth = notchwidth, varwidth = varwidth,
                            na.rm = na.rm, ...))
  l2 <- layer(data = data, mapping = mymap, stat = StatSummary,
              geom = "label", position = position, show.legend = show.legend,
              inherit.aes = inherit.aes, params = list(fun.data = letters,
                                                       fun.y = fun.y, fun.ymax = fun.ymax, fun.ymin = fun.ymin,
                                                       fun.args = fun.args, na.rm=na.rm,family=font,size=fsize/3,vjust=-0.1,...))
  return(list(l1,l2))
}


