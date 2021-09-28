#' Transform data frame to a ggplot object table, which can be inserted to a ggplot graph
#'
#' @param table a data frame table
#'
#' @return a ggplot2 object
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' g <- ggadonis.table(table = adonis.pairwise(iris[,1:4],iris$Species))
#' ggplot(data = data.frame(x = 1:10, y = 1:10), aes(x,y)) + geom_point() +
#' theme_bw() +
#'   annotation_custom(grob = g,
#'                     xmin = 3,xmax = 8,
#'                     ymin = 8,ymax = 8.5)
ggadonis.table <- function(table){
  # step 1 check if the results of pairwiseadonis analysis result
  if (inherits(table, "pwadonis")) {
    colnames(table)[1] <- expression("bold(pairgroup)")
    colnames(table)[2] <- expression("bold(adonis) (bold(italic(R))^bold(`2`))")
    colnames(table)[3] <- expression("bold(p.value)")
    colnames(table)[4] <- expression("bold(p.adj)")
    table[,2] <- round(table[,2], 3)
    p.sig <- which(table[, ncol(table)]< 0.05) + 1
  }
  # step 2 turn into a ggplot2 object
  g <- gridExtra::tableGrob(table, theme = tt, rows = NULL)
  # step 3 change the values based on the p.value
  if (exists("p.sig") & length(p.sig > 0)) {
    for (i in p.sig) {
      ind <- find_cell(g, i, 4, "core-fg")
      g$grobs[ind][[1]][["gp"]] <- grid::gpar(col = "red", fontface = "bold.italic")
      ind2 <- find_cell(g, i, 1, "core-fg")
      g$grobs[ind2][[1]][["gp"]] <- grid::gpar(col = "black", fontface = "bold")
    }
  }
  # step 4 draw the table or return the table object
  grid::grid.draw(g)
  return(g)
}

tt <- gridExtra::ttheme_default(colhead = list(fg_params = list(parse = TRUE)),
                     core = list(fg_params = list(fontface = c(rep("plain", 3)))))

find_cell <- function(table, row, col, name="core-fg"){
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}

