###################################################################################################
# Import Dependencies
###################################################################################################

library(grid)

# purpose: renders multiple ggplot2 plots in a single graphics device
# input: ggplot2 plots
# output: arguments displayed in a single graphics device
# note: source/owner: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  plots <- c(list(...), plotlist)
  numPlots = length(plots)

  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                     ncol=cols, nrow=ceiling(numPlots / cols))
  }

  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout=grid.layout(nrow(layout), ncol(layout))))

    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row=matchidx$row, layout.pos.col=matchidx$col))
    }
  }
}
