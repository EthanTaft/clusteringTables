#' Gives nice dataframe
#'
#' Gives nice dataframe of personId, their cluster, their table, their tsne
#' coordinates, and their original responses to the questions.
#' @param tableList list of final tableing to use
#' @param df original dataset used
#' @param id What is the Id column in the original dataset. Should be the same
#' @param tsne_results Tsne data(X, Y, cluster) from plot_clustering object
#' id column in the final tableing list.
#' @keywords Arrange for nice table including clusters and table numbers
#' @export
assembleToDf <- function(tableList, df, id, tsne_results) {

  #make sure all tables inside the list are dataframes
  total <- lapply(tableList, data.frame)

  # Give every table it's table number and combine all tables into a dataframe
  # Re-order columns
  total2 <- purrr::map_df(total, function(x) {x}, .id = "Table")
  total2 <- total2[, c(2, 3, 1)]

  #inner join df and total2 by id
  total3 <- merge(df, total2, by = id)
  #inner join total3 and tsnedata
  total4 <- merge(total3, tsne_results[, !names(tsne_results) %in% "cluster"],
                  by.x = id, by.y = "name")

  return(list(dfAssembled = total4))
}

