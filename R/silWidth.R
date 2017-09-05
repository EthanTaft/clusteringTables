#' Find Silhouette widths and plot for many k in R
#'
#' This function allows user to calculate silhouette widths and plot them
#' to find the appropriate cluster size for later use.
#' @param df original dataframe
#' @param gowerDiss gower distance from using computeGowerDiss()
#' @param tableSize If you want a specific table size, what is it?
#' @keywords silhouette width, silhouette plot
#' @export
silWidth <- function(df, gowerDiss, tableSize = NULL) {
  if (nrow(df) <= 10) {
    kay <- 1:(nrow(df) - 1)
  } else {
    kay <- 1:10
  }
  if (!is.null(tableSize)) {
    if (tableSize >= nrow(df)) {
      stop("Specified tableSize must be less than nrows(df)")
    }
    if (tableSize > nrow(df)/2) {
      warning("tableSize > nrow(df)/2 will lead to subpar results.")
    }
    kay <- 1:ceiling(nrow(df)/tableSize)
  }

  widths <- lapply(kay, function(kay) {
    pamfit <- cluster::pam(gowerDiss, diss = TRUE, k = kay)
    return(pamfit$silinfo$avg.width)
  })

  widths <- suppressWarnings(as.numeric(as.character(widths)))

  plotSilWidth <- function() {
    plot(1:length(widths), widths,
         xlab = "Number of Clusters",
         ylab = "Avgerage Silhouette Width")
    lines(1:length(widths), widths)
  }

  return(list(widths = widths, plotWidths = plotSilWidth))

}
