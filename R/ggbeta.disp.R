#' Multivariate dispersion analysis based on ggplot2 and betadisper function of vagan package
#'
#' @param data a data frame with the first collounm as the grouping information
#' @param type the type of result to present, one of "boxplot", "ordiplot" or "mod", default value is ordiplot.
#' @param pc which axis to present, one of 12, 13, or 23, the default value is 12.
#' @param show.pvalue a boolean value, the default value is TRUE.
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' genus.Syn <- read.csv(file = "genus.Syn.csv", row.names = 1)
#' ggbeta.disp(genus.Syn, pc = 12)
#' library(vegan)
#' plot(mod, ellipse = T, hull = F, conf = 0.95, axes = c(1,2))
#' vegan:::plot.betadisper(mod)
#' vegan:::boxplot.betadisper(mod)
#' ggbeta.disp(genus.Syn, pc = 12, type = "boxplot")
#' ggbeta.disp(genus.Syn, pc = 12, type = "mod")
ggbeta.disp <- function(data, type = "ordiplot", pc = 12,
                        show.pvalue = TRUE){
  # step1 Judge whether the first column is grouping information
  if ("group" != colnames(data)[1]) {
    stop("The first collumn mast be the grouping information!")
  }
  # step2 Judge whether the remaining variables are numeric
  if (!all(sapply(data[, -1], is.numeric))) {
    stop("The variable must be the numeric values!")
  }
  # step3 calculate the distance and prepare the data for plot
  dis <- vegan::vegdist(x = data[, -1], method = "bray")
  mod <<- vegan::betadisper(d = dis, group = data$group, type = "centroid")
  perm <- vegan::permutest(mod, pairwise = TRUE, permutations = 999)
  df <- data.frame(group = mod$group, mod$vectors[, c(1, 2, 3)])
  df.ct <- mod$centroids[, c(1, 2, 3)]
  colnames(df.ct) <- paste0(colnames(df.ct), ".ct")
  df.ct <- data.frame(group = rownames(df.ct), df.ct)
  grp <- as.character(unique(df$group))
  df.all <- merge(df, df.ct, by = "group")
  # p.value
  p.value <- perm$tab$`Pr(>F)`[1]
  label <- data.frame(min = apply(df.all[, 2:4], 2, min),
                      max = apply(df.all[, 2:4], 2, max))
  label$mean <- (label$min + label$max)/2
  eig <- round(mod$eig/sum(mod$eig)*100, 2)
  # step4 Judge which axis to present
  if (pc == 12) {
    x <- "PCoA1"
    y <- "PCoA2"
    x.ct <- "PCoA1.ct"
    y.ct <- "PCoA2.ct"
    x.posi <- label[1, 3]
    y.posi <- label[2, 2]
    x.lab <- paste0(x, ": ",eig[1], "%")
    y.lab <- paste0(y, ": ",eig[2], "%")
  }else if (pc == 13) {
    x <- "PCoA1"
    y <- "PCoA3"
    x.ct <- "PCoA1.ct"
    y.ct <- "PCoA3.ct"
    x.posi <- label[1, 3]
    y.posi <- label[3, 2]
    x.lab <- paste0(x, ": ",eig[1], "%")
    y.lab <- paste0(y, ": ",eig[3], "%")
  }else if (pc == 23) {
    x <- "PCoA2"
    y <- "PCoA3"
    x.ct <- "PCoA2.ct"
    y.ct <- "PCoA3.ct"
    x.posi <- label[2, 3]
    y.posi <- label[3, 2]
    x.lab <- paste0(x, ": ",eig[2], "%")
    y.lab <- paste0(y, ": ",eig[3], "%")
  }
  # step5 Judge which result to present
  if (type == "boxplot") {
    p <- ggroup::ggboxplot.LetSig1(data = data.frame(group = as.factor(genus.Syn$group),
                                                Distance_to_centroid = mod$distances)) +
      theme(legend.position = 0)
  }else if (type == "ordiplot") {
    # step6 draw ordiplot
    p <- ggplot(data = df.all,
                aes_string(x = x, y = y, shape = "group", colour = "group")) +
      geom_point(show.legend = T) +
      stat_ellipse(level = 0.68,  linetype = 1, geom = "polygon", alpha = 0.02,
                   aes(fill = group)) +
      geom_point(aes_string(x = x.ct, y = y.ct, colour = "group"),
                 show.legend = F, size = 3, shape = 16)
    # step7 add segment from centroid to sample by grouping information
    for (i in grp) {
      print(i)
      df1 <- df.all[df.all$group == i, ]
      p <- p + geom_segment(aes_string(x = x.ct, y = y.ct,
                                       xend = x, yend = y), data = df1,
                            show.legend = F)
    }
    p <- p +
      theme(panel.grid = element_line(color = 'gray90', size = 0.1),
            panel.background = element_rect(color = 'gray60',
                                                   fill = 'transparent', size = 1),
            axis.text = element_text(size = 12, face = "bold", color = "black"),
            axis.text.x = element_text(colour = "black", size = 12, face = "bold"),
            axis.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 10, face = "bold"),
            legend.title = element_blank(),
            legend.position = "right",
            panel.border = element_rect(colour = "black", fill = "transparent"))
    # step7.2 add pvalue
    if (show.pvalue) {
     p <- p + annotate(geom = "text", x = x.posi, y = y.posi*1.1,
                       label = paste0("p.value =  ", p.value),
                       size = 5, fontface = "bold.italic",
                       colour = ifelse(p.value < 0.05, "red1", "black"))
     p <- p + xlab(x.lab) + ylab(y.lab)
    }
  }else{return(mod)}
  return(p)
}
