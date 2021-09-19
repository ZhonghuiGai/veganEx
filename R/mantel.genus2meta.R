#' Title
#'
#' @param genusdata
#' @param metadata
#' @param method
#'
#' @return
#' @export
#'
#' @examples
mantel.genus2meta <- function(genusdata, metadata,
                          method = "pearson"){
  genus.dist <- vegan::vegdist(genusdata, method = "bray")#根据物种丰度数据，计算样方的bray距离
  #根据样方临床指标，计算欧几里德距离，为了消除量纲影响，先进行标准化
  metadata <- base::scale(metadata, center = TRUE, scale = TRUE)#标准化
  meta.dist <- vegan::vegdist(metadata, method = "euclid")#计算欧几里德距离
  mantel.result <- vegan::mantel(genus.dist, meta.dist, permutations=9999, method = method)
  # 构建画图用的数据框
  data <- data.frame(x = as.vector(genus.dist), y = as.vector(meta.dist))
  # 作图
  p <- ggplot2::ggplot(data = data, aes(x = x, y = y))+
    geom_point(size=1, alpha=1, color="gray10")+
    geom_smooth(method = lm, colour = "red", alpha = 0.2, se = TRUE)+
    labs(x = "Bray-Curtis dissimilarity", y = "metadata Euclidean dissimilarity") +
    theme( axis.text.x = element_text(face = "bold",colour = "black", size = 12),
           axis.text.y = element_text(face = "bold", size = 11, colour = "black"),
           axis.title= element_text(face = "bold", size = 14, colour = "black"),
           panel.background = element_blank(),
           panel.border = element_rect(fill = NA, colour = "black"),
           legend.position = "top",
           legend.text = element_text(size = 10, face = "bold"),
           legend.title = element_text(size = 11, face = "bold"))
  # 提取统计量
  r <- mantel.result$statistic
  p.val <- mantel.result$signif
  # 添加文本
  p <- p + ggplot2::annotate(geom = "text", x = mean(data$x), y = max(data$y)*1.05,
                             label = paste0("Mantel statistic r: ", round(r, 3)),
                             size = 5, fontface = "bold")
  if(p.val < 0.05){
    p <- p + ggplot2::annotate(geom = "text", x = mean(data$x), y = max(data$y)*1.01,
                               label = paste0("p value: ", round(p.val, 3)),
                               size = 5, fontface = "bold", color = "red")
  }else{
    p <- p + ggplot2::annotate(geom = "text", x = mean(data$x), y = max(data$y)*1.01,
                               label = paste0("p value: ", round(p.val, 3)),
                               size = 5, fontface = "bold")
  }
  print(mantel.result)
  return(p)
}
