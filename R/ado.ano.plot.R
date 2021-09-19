
#' plot the results of pairwise.adonis and pairwise.anosim.
#'
#' @param data a data.frame from pairwise.adonis or pairwise.anosim
#' @param limits range of heatmap
#' @param midpoint midpoint of heatmap
#' @param show return the plot of heatmap or the p.value pointmap. 1 for heatmap
#' 2 for p.value pointmap, the default value is 1.
#'
#' @return as describe in show
#' @export ado.ano.plot
#' @author ZH Gai
#'
#' @importFrom ggplot2 aes annotate element_blank element_line element_rect element_text
#' geom_hline geom_point geom_text geom_tile geom_vline ggplot theme_void xlab xlim ylab
#' scale_fill_gradient2 scale_x_discrete scale_y_discrete theme
#'
#' @examples
#' ado.ano.plot(data = adonis.pairwise(iris[,1:4],iris$Species),
#' limit = c(0, 1), midpoint = 0.5, show = 1)

ado.ano.plot <- function(data, limits, midpoint, show = 1){
  if (inherits(data, "pwadonis")) {
    f <- data$adonisR2
  }else if (inherits(data, "pwanosim")) {
    f <- data$anosimR
  }else{
    stop("data must be of class 'pwadonis' or 'pwanosim'")
  }
  p.adj <- data$p.adj
  col <- ifelse(p.adj < 0.05, "red", "black")
  # heatmap----
  p1 <- ggplot(data = data, aes(x = rep(NA, nrow(data)), y = data[, "pairs"])) +
    geom_tile(aes(fill = f), colour = "white") +
    scale_fill_gradient2(limits = c(0,1), low = "#3c9eff",
                         mid="white", high = "#ff445d", midpoint = 0.2) +
    theme_void() +
    theme(axis.text.y = element_text(size = 14, face = "bold"),
          axis.title.x = element_text(size = 12, face = "bold"),
          legend.position = 0) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0)) +
    ylab(NULL) +
    geom_text(aes(label = round(f, 2)), col = "black", size = 5, fontface = "bold")
  if (inherits(data, "pwadonis")) p1 <- p1 + xlab("adonis (R2)")
  if (inherits(data, "pwanosim")) p1 <- p1 + xlab("anosim (R)")
  # p.value point----
  p2 <- ggplot(data, aes(x = -log10(p.adj), y = data[, "pairs"])) +
    ylab(NULL)+
    geom_point() +
    geom_vline(xintercept = -log10(0.05), size = 0.2, color = "red", lty = 2) +
    geom_hline(yintercept = 0.4, size = 0.2, color = "black", lty = 1) +
    geom_hline(yintercept = c(1:nrow(data)), size = 0.2, color = "gray90", lty = 2) +
    geom_point(size = 3, color = col) + xlim(c(0, 2.6)) +
    theme(axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_line(color = 'gray99', size = 0.1),
          panel.background = element_rect(color = 'white', fill = 'transparent'),
          axis.text = element_text(size = 12, face = "bold"),
          axis.title.x = element_text(size = 12, face = "bold")) +
    annotate(geom = "text", x = -log10(0.05), y = nrow(data)-0.5, label = "p = 0.05",
             fontface = "bold.italic", size = 5) + xlab("-log(p value)")
  # plot
  if (show == 1) return(p1)
  if (show == 2) return(p2)
}




