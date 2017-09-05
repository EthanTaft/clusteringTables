#' Compute Most Similar Observations
#'
#' This function allows user to find most similar observations from the
#' dissimilarity matrix
#' @param diss dissimilarity object
#' @param df dataframe from which the dissimilarity object was calculated
#' @keywords Dissimilarity, Find Most
#' @export
findMostSimilar <- function(diss, df) {
  if (!is.matrix(diss)) {
    diss <- as.matrix(diss)
  }
  df[which(diss == min(diss[diss != min(diss)]), arr.ind = TRUE)[1, ], ]
}
#' Compute Least Similar Observations
#'
#' This function allows user to find the least similar observations from the
#' dissimilarity matrix
#' @param diss dissimilarity object
#' @param df dataframe from which the dissimilarity object was calculated
#' @keywords Dissimilarity, Find Least
#' @export
findLeastSimilar <- function(diss, df) {
  if (!is.matrix(diss)) {
    diss <- as.matrix(diss)
  }
  df[which(diss == max(diss[diss != max(diss)]), arr.ind = TRUE)[1, ], ]
}
