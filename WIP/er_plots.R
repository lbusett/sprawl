er_box <- function(ts_full,
                   facets = TRUE,
                   lab_n = FALSE,
                   plot = FALSE,
                   save = FALSE,
                   type = "box") {

  p <- ggplot(ts_full, aes(x = date, y = value, group = date)) + theme_bw()
  # p <- p + geom_boxplot(outlier.colour = "transparent")
  if (type == "box")    p <- p + geom_boxplot(outlier.colour = "transparent", na.rm = TRUE)
  if (type == "violin") p <- p + geom_violin(draw_quantiles = c(0.5))
  # if(facets) {p <- p + facet_wrap(~aes_q(facets = as.name(names(ts_full[1]))))}
  if(facets) {p <- p + facet_wrap(eval(names(ts_full[1])))}
  if(lab_n) {
    globmin <- min(ts_full$value, na.rm = T)
    give.n <- function(x){
      return(c(y = globmin, label = length(which(!is.na(x)))))
    }
    p <- p + stat_summary(fun.data = give.n, geom = "text", size = 3)
    }
  if(plot) print(p)
  if(save) {
    filename = file.choose()
    ggsave(p, filename = filename, device = "png")
  } else  {
    return(p)
  }
}

