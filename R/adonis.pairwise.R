#' Pairwise multilevel comparison using adonis
#'
#' @param x Data frame (the community table), or "dist" object (user-supplied distance matrix).
#' @param factors Vector (a column or vector with the levels to be compared pairwise).
#' @param sim.function Function used to calculate the similarity matrix,
#' one of 'daisy' or 'vegdist' default is 'vegdist'. Ignored if x is a distance matrix.
#' @param sim.method Similarity method from daisy or vegdist, default is 'bray'. Ignored if x is a distance matrix.
#' @param p.adjust.m The p.value correction method, one of the methods supported by p.adjust(),
#' default is 'bonferroni'.
#' @param perm perm
#' @return Table with the pairwise factors, R^2, p.value and adjusted p.value.
#' @export
#' @author ZH Gai
#' @importFrom stats p.adjust
#' @importFrom utils combn
#' @importFrom vegan adonis vegdist
#' @importFrom cluster daisy
#' @examples
#' adonis.pairwise(iris[,1:4],iris$Species)
adonis.pairwise <- function(x, factors,
                            sim.function = "vegdist",
                            sim.method = "bray",
                            p.adjust.m = "bonferroni",
                            perm = 999){
  co <- combn(unique(as.character(factors)), 2) # pairwise grouping
  n <- ncol(co)
  pairs <- vector(mode = "numeric", length = n)
  adonisR2 <- vector(mode = "numeric", length = n)
  p.value <- vector(mode = "numeric", length = n)

  for(i in 1:n){
    if(inherits(x, "dist")){
      m <- as.matrix(x)[factors %in% co[, i], factors %in% co[, i]]
    }else if (sim.function == "daisy") {
      m <- cluster::daisy(x[factors %in% co[, i], ], metric = sim.method)
    }else{
      m <- vegan::vegdist(x[factors %in% co[, i], ], method = sim.method)
    }

    ad <- vegan::adonis(m ~ factors[factors %in% co[, i]], permutations = perm)
    pairs[i] <- paste0(co[1,i], ".vs.", co[2,i])
    adonisR2[i] <- ad$aov.tab[1,5] |> round(3)
    p.value[i] <- ad$aov.tab[1,6]
  }
  p.adj <- p.adjust(p.value, method = p.adjust.m)
  pairw.res <- data.frame(pairs, adonisR2, p.value, p.adj)
  class(pairw.res) <- c("pwadonis", "data.frame")
  return(pairw.res)
}
