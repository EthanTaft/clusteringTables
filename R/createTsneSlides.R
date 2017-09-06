#' Create slides of tsne process for use in animations
#'
#' This function allows user to plot cluster results from a PAM object using
#' t-distributed stochastic neighborhood embedding. Several slides are created
#' based on the number of iterations specified. E.g. If you specify 1000
#' iterations, the slides created will be tsne results from 1, 10, 20, 30, 40,
#' and so on up to 1000 iterations.
#' @param df Data frame from which dissimilarities were calculated
#' @param gowerDiss gower dissimilarities to use
#' @param id ids from original data
#' @param pam_fit PAM object from building a PAM clustering model
#' @param iterations Number of iterations for tsne
#' @keywords t-SNE, plot clustering, dimensionality reduction
#' @import magrittr
#' @import stats
#' @import graphics
#' @import ggplot2
#' @export
createTsneSlides <- function(df, gowerDiss, id, pam_fit, iterations) {
  set.seed(1000)
  for (i in seq(1, iterations, 10)) {
    tsne_obj <- Rtsne::Rtsne(gowerDiss, is_distance = TRUE, max_iter = i)

    tsne_data <- tsne_obj$Y %>%
      data.frame() %>%
      setNames(c("X", "Y")) %>%
      dplyr::mutate(cluster = factor(pam_fit$clustering),
                    name = df[, id])

    g <-
      ggplot2::ggplot(tsne_data, ggplot2::aes(x = X, y = Y)) +
      ggplot2::geom_point(ggplot2::aes(color = cluster))
    ylims <- ggplot2::layer_scales(g)$y$range$range
    xlims <- ggplot2::layer_scales(g)$x$range$range
    ylims <- c(-1, 1) * max(abs(ylims))
    xlims <- c(-1, 1) * max(abs(xlims))
    g +
      ggplot2::ylim(ylims) +
      ggplot2::xlim(xlims)

    file = paste0(sprintf("%05d", i), ".jpg")
    ggplot2::ggsave(file)
  }
}
