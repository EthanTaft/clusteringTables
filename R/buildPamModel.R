#' Build a PAM model
#'
#' This function allows user to build a PAM model
#' @param dissimilarities gower dissimilarity to use
#' @param silWidths Allows for use of a vector of silWidths. Inherited from
#' silWidth$widths. You must specify a k or vector of widths.
#' @param k Number of clusters to find.  If silWidths is present, k will become
#' which.max(silWidths). You must specify a k or vector of widths.
#' @keywords PAM, Gower, Dissimilarity
#' @export
buildPamModel <- function(dissimilarities, silWidths = NULL, k = NULL) {
  if (is.null(silWidths) && is.null(k)) {
    stop("You must specify a vector of silWidths or k")
  }
  if (!is.null(silWidths)) {
    k = which.max(silWidths)
  }
  pam_fit <- cluster::pam(x = dissimilarities, diss = TRUE, k = k)
  return(pam_fit)
}
