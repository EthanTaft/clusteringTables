#' Plot Results of Tabling
#'
#' Plot the results of the finished product/dataframe from tableing
#' @param merged_data Final dataframe that includes table, cluster, and tsne
#' assignments
#' @keywords Arrange for nice table including clusters and table numbers
#' @export
plotTables <- function(merged_data) {
  merged_data %>%
    dplyr::arrange(as.factor(as.numeric(Table))) %>%
    ggplot2::ggplot(., aes(x = X, y = Y)) +
             geom_point(aes(color = Table)) +
             labs(title = "Table Distribution Among Clusters")
}



