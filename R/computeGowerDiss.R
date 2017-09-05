#' Compute Gower Dissimilarity
#'
#' This function allows user to compute gower dissimilarity for a dataframe
#' @param df Data frame to use in dissimilarity computation
#' @param id If there is an id column, what is it?
#' @keywords Dissimilarity
#' @export
computeGowerDiss <- function(df, id = NULL) {
  if (!is.null(id)) {
    df[[id]] <- NULL
  }
  gower_diss <- cluster::daisy(df, metric = "gower")
  return(list(gower_diss = gower_diss, gower_df = df))
}
