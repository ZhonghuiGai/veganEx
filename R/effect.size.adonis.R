#' The adonis function in vegan package is used to analyze the effect of each variable of one data frame on another data matrix.
#' It can be metadata such as age and gender, the effect on the gut microbiota.
#' It can also be each OTU in the gut microbiota, the effect on the distance matrix of inflammatory factors.
#'
#' @param data1 a matrix, dependent variable, equivalent to y of y ~x
#' @param data2 a data frame, independent variable, equivalent to x of y ~x
#' @param signif select the significant variables
#' @param barplot show the barplot
#'
#' @return a ggplot2 function
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' effect.size.adonis(iris[, 1:2], iris[, 3:5, drop = F])
effect.size.adonis <- function(data1, data2, signif = TRUE, barplot = TRUE){
  stopifnot(all(rownames(data1) == rownames(data2)))
  stopifnot(all(sapply(data1, is.numeric)))
  name <- colnames(data2)
  dis <- vegdist(data1, method = "bray")
  effect.adonis <- NULL
  for (i in name) {
    print(i)
    result <- matrix(NA, nrow = 1, ncol = 3)
    colnames(result) <- c("Variable", "R2", "pvalue")
    result <- as.data.frame(result)
    formula <- as.formula(paste0("dis", "~", i))
    adonis <- vegan::adonis(formula = formula, data2, permutations = 999, distance = "bray")
    tmp <- as.data.frame(adonis$aov.tab, check.names = F)[1, ]
    result[1, 1] <- rownames(tmp)
    result[1, 2:3] <- tmp[1, 5:6]
    effect.adonis <- rbind(effect.adonis, result)
  }
  write.table(effect.adonis, "effect.size.adonis.txt",
              row.names = FALSE, sep = '\t', quote = FALSE, na = "")
  if(signif){
    effect.adonis <- effect.adonis[effect.adonis$pvalue < 0.05, ]
    effect.adonis <- effect.adonis[order(effect.adonis$pvalue), ]
    effect.adonis <- effect.adonis[order(effect.adonis$R2, decreasing = TRUE), ]
  }
  if(barplot){
    effect.adonis$Variable = factor(effect.adonis$Variable,
                                    levels=effect.adonis$Variable)
    p <- ggplot(data=effect.adonis, aes(x=Variable, y=100*R2)) +
      geom_bar(stat="identity", fill = "#247ba0")+
      theme(panel.grid = element_line(color = NA, size = 0.1),
            panel.background = element_rect(color = NA,fill = 'transparent'),
            plot.title = element_text(size = 14, face = "bold"),
            axis.text.x = element_text(size = 14, face = "bold", angle = 45, hjust = 1, vjust = 1,
                                       color = "black"),
            axis.text.y = element_text(size = 14, face = "bold", color = "black" ),
            axis.title.y = element_text(size = 14, face = "bold"),
            legend.text = element_text(size = 10, face = "bold"),
            legend.title = element_text(size = 12, face = "bold", color = "gray30"),
            axis.title.x = element_text(size = 14, face = "bold", angle = 0, hjust = 0.5),
            legend.position = c(0.8,0.905))+
      ylab(expression("% of Explained variance ( "~R^2~")"))+xlab(NULL)+
      geom_text(aes(label=round(R2, 3)*100),
                position = position_dodge(width = 0.7),
                size=4.5, hjust=0.5, vjust = 1.2, fontface="bold",
                color = "white")
  }
  return(p)
}
