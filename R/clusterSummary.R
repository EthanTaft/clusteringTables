#' Compute Cluster summary from PAM
#'
#' This function allows user to compute cluster summaries for a PAM model
#' @param df Data frame from which dissimilarities were calculated
#' @param id If there is an id column, what is it?
#' @param pam_model model built previously using pamModel()
#' @keywords Cluster Summary, Results
#' @import tidyverse magrittr
#' @export
clusterSummary <- function(df, id = NULL, pam_model) {
  if (!is.null(id)) {
    df[[id]] <- NULL
  }
  pamResults <-
    df %>%
    dplyr::mutate(cluster = pam_model$clustering) %>%
    dplyr::group_by(cluster) %>%
    dplyr::do(the_summary = summary(.))

  return(pamResults$the_summary)
}
