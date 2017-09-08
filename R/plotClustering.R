#' Plot PAM cluster results in 2D space using t-SNE
#'
#' This function allows user to plot cluster results from a PAM object using
#' t-distributed stochastic neighborhood embedding.
#' @param df Data frame from which dissimilarities were calculated
#' @param gowerDiss gower dissimilarities to use
#' @param id ids from original data
#' @param pam_fit PAM object from building a PAM clustering model
#' @keywords t-SNE, plot clustering, dimensionality reduction
#' @import magrittr
#' @import tidyverse
#' @import stats
#' @import graphics
#' @export
plotClustering <- function(df, gowerDiss, id, pam_fit) {
  if (nrow(df) - 1 < 3 * 30) {
    perplex = (nrow(df) - 1) / 3
    tsne_obj <- Rtsne::Rtsne(gowerDiss, is_distance = TRUE, perplexity = perplex)
  } else {
    tsne_obj <- Rtsne::Rtsne(gowerDiss, is_distance = TRUE)
  }
  tsne_data <-
    tsne_obj$Y %>%
    data.frame %>%
    setNames(c("X", "Y")) %>%
    dplyr::mutate(cluster = factor(pam_fit$clustering),
           name = df[, id])

  plot <- ggplot2::ggplot(tsne_data, ggplot2::aes(x = X, y = Y)) +
                   ggplot2::geom_point(ggplot2::aes(color = cluster))
  return(list(tsneObj = tsne_obj, tsneDat = tsne_data, plot = plot))
}
