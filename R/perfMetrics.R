#' Performance Metrics of the Tabling
#'
#' Gives performance metrics of tabling and compiles a finished dataset with
#' the performance metrics given at the grain level.
#' @param merged_data data set with cluster, table, and tsne data
#' from assembleToFinalDf
#' @keywords Performance metrics
#' @export
perfMetrics <- function(merged_data) {
  merged_data$Table <- as.factor(merged_data$Table)
  props <- list()
  for (i in 1:length(unique(merged_data$Table))) {
    n <- merged_data[merged_data$Table == i, ]
    n$Table <- as.numeric(n$Table)
    c <- prop.table(table(n$cluster, n$Table), 2)
    props[[i]] <- c
  }

  # Use to get percent of people coming from the most common cluster at each
  # table
  most_common <- lapply(props, function(x) max(x))
  merged_data$mostCommon <- sapply(merged_data$Table,
                                   function(i) {
                                     return(most_common[[i]])
                                   })

  mostCommon <- mean(merged_data$mostCommon)

  #number of clusters per table
  num_clusters <- lapply(props, function(x) length(x))
  merged_data$numClusters <- sapply(merged_data$Table,
                                    function(i) {
                                      return(num_clusters[[i]])
                                    })

  numClusters <- mean(merged_data$numClusters)

  return(list(mostCommon = mostCommon, numClusters = numClusters, finalDf =
                merged_data))
}

