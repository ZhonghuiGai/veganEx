#' Test the correlation of two matrix using the mantel function in the vegan package
#'
#' @param genusdata the OTU table
#' @param metadata the metadata
#' @param method method to be used for correlation analysis, one of pearson and spearman, the default value is pearson
#' @param perm the permutation value, default value is 999
#'
#' @return a ggplot object
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' mantel.genus2meta(iris[,1:2], iris[,3:4])
mantel.genus2meta <- function(genusdata, metadata,
                          method = "pearson", perm = 999){
  genus.dist <- vegan::vegdist(genusdata, method = "bray")
  metadata <- base::scale(metadata, center = TRUE, scale = TRUE)
  meta.dist <- vegan::vegdist(metadata, method = "euclid")
  mantel.result <- vegan::mantel(genus.dist, meta.dist, permutations = perm, method = method)
  # construct the data frame for ggplot2
  data <- data.frame(x = as.vector(genus.dist), y = as.vector(meta.dist))
  # make the plot
  p <- ggplot2::ggplot(data = data, aes(x = x, y = y)) +
    geom_point(size = 1, alpha =1 , color = "gray10")+
    geom_smooth(method = lm, formula = y ~ x, colour = "red", alpha = 0.2, se = TRUE)+
    labs(x = "Bray-Curtis dissimilarity", y = "metadata Euclidean dissimilarity") +
    theme(axis.text.x = element_text(face = "bold",colour = "black", size = 12),
          axis.text.y = element_text(face = "bold", size = 11, colour = "black"),
          axis.title= element_text(face = "bold", size = 14, colour = "black"),
          panel.background = element_rect(fill = NA, colour = "black"),
          panel.border = element_rect(fill = NA, colour = "black"),
          legend.position = "top",
          legend.text = element_text(size = 10, face = "bold"),
          legend.title = element_text(size = 11, face = "bold"))
  # extract the statistics
  r <- mantel.result$statistic
  p.val <- mantel.result$signif
  # add the text
  p <- p + ggplot2::annotate(geom = "text", x = mean(data$x), y = max(data$y)*1.1,
                             label = paste0("Mantel statistic r: ", round(r, 3)),
                             size = 5, fontface = "bold")
  if(p.val < 0.05){
    p <- p + ggplot2::annotate(geom = "text", x = mean(data$x), y = max(data$y)*1.01,
                               label = paste0("p value: ", round(p.val, 3)),
                               size = 5, fontface = "bold.italic", color = "red")
  }else{
    p <- p + ggplot2::annotate(geom = "text", x = mean(data$x), y = max(data$y)*1.01,
                               label = paste0("p value: ", round(p.val, 3)),
                               size = 5, fontface = "bold")
  }
  print(mantel.result)
  return(p)
}
