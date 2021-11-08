#' Calculate the alpha diversity of OUT table or ASVs table using estimateR and diversity function of vegan package.
#'
#' @param data a data frame containing the grouping information
#'
#' @return a data frame containg the alpha index
#' @export
#'
#' @author ZHonghui Gai
#' @examples
#' library(vegan)
#' data(BCI)
#' BCI <- data.frame(group = c(rep("ss", 25), rep("dd", 25)), BCI)
#' BCI$group <- as.factor(BCI$group)
#' alpha(BCI)  |> ggroup::ggboxplot.LetSig()
alpha <- function(data){
  message("This function use grouped OTU table to calculate alpha diversity indices!\nThe first col should be the grouping information!")
  if (colnames(data)[1] != "group") {
    stop("The first collomn should be the grouping information! Pls check it!")
  }
  genus <- data[, -1]
  group <- data$group
  sample <- rownames(data)
  Chao1 <- vegan::estimateR(genus)[2, ]
  stopifnot(all(names(Chao1) == sample))
  ACE <- vegan::estimateR(genus)[4, ]
  stopifnot(all(names(ACE) == sample))
  Shannon <- vegan::diversity(genus, index = "shannon")
  stopifnot(all(names(Shannon) == sample))
  Simpson <- 1 - vegan::diversity(genus, index = "simpson")
  stopifnot(all(names(Simpson) == sample))
  res <- data.frame(group = group, Chao1 = Chao1, ACE = ACE, Shannon = Shannon, Simpson = Simpson)
  return(res)
}
