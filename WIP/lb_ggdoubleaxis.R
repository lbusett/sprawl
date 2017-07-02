
lb_ggpdoubleaxis = function(p1,p2){


  grid.newpage()

  # two plots
  # p1 <- ggplot(mtcars, aes(mpg, disp)) + geom_line(colour = "blue") +lb_theme_bw()
  p2 <- p2 +  lb_theme_bw(x_ang = 45)+ theme(axis.text.x = element_text( hjust  = 1 , vjust = 1)) %+replace%  theme(panel.background = element_rect(fill = NA))
  p2 = p2 + theme(panel.grid.major = element_line(color = 'transparent'))   + theme(panel.grid.minor = element_line(color = 'transparent'))
  #
  # + theme_bw() %+replace%
  #   theme(panel.background = element_rect(fill = NA))
  # extract gtable
  g1 <- ggplot_gtable(ggplot_build(p1))
  g2 <- ggplot_gtable(ggplot_build(p2))


  panels = g1$layout$name[ grepl("panel", g1$layout$name)]
  axis = g1$layout$name[ grepl("axis_l", g1$layout$name)]
  # overlap the panel of 2nd plot on that of 1st plot
  pp <- c(subset(g1$layout, name %in% panels, se = t:r))
  ia <- which(g2$layout$name %in% axis)
  indpan = 1
  g = g1
  a= g2$widths[g2$layout[ia, ]$l]
  a [] = 0.13

  g <- gtable_add_cols(g,  a, pos = -1 )
  for (pan in which(g2$layout$name %in% panels)) {

    g <- gtable_add_grob(g, g2$grobs[[pan]], pp$t[[indpan]], pp$l[[indpan]], pp$b[[indpan]], pp$l[[indpan]])

    for (ias in ia)
    {
      ga <- g2$grobs[[ias]]
      ax <- ga$children[[2]]
      ax$widths <- rev(ax$widths)
      ax$grobs <- rev(ax$grobs)
      ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.0, "cm")
      # ax$grobs[[2]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.0, "cm")
      g <- gtable_add_grob(g, ax, pp$t[[indpan]], length(g$widths) - 1, pp$b[[indpan]])
      # g <- gtable_add_grob(g, ax, pp$t[[indpan]], length(g$widths) - 1, pp$b[[indpan]])
    }
    indpan= indpan+1

  }
  a= g2$widths[g2$layout[ia, ]$l]
  a [] = 0.35
  # g <- gtable_add_cols(g,  a, pos = -1 )
  # g <- gtable_add_grob(g, ax, pp$t[[indpan]], length(g$widths) - 1, pp$b[[indpan]])

  # ia <- which(g$layout$name == "ylab")
  # ylab <- g$grobs[[ia]]
  #
  text.grob1 = textGrob("Avg. T [°C] / Cum. Rainfall [mm/10]", rot = 90, y = unit(.5, "npc"), gp = gpar(fontsize = 9), vjust = 0.2)
  g <- gtable_add_cols(g,  a, pos = -1 )
  g <-  gtable_add_grob(g, text.grob1, pp$t[[2]]-5, length(g$widths) - 1, pp$b[[2]]+5)
  grid.draw(g)
}
