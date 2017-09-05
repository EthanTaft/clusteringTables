#' Put as many people into specified table size
#'
#' This function is the first iteration of tableing
#' @param df original dataframe from which the clustering was done
#' @param id Id column from dataset
#' @param pam_model PAM object from building a pam model
#' @param tableSize How many people per table are there going to be?
#' @keywords Tabling
#' @export
#' @return A list of full tables and a list of tables that are not full,
#' referred to as leftOvers.
gatherTables <- function(df, id, pam_model, tableSize) {
  df$cluster <- pam_model$clustering
  datframe <- data.frame("personId" = df[[id]],
                   "cluster" = df$cluster)
  # Put each cluster, as a dataframe, into a list.
  clusterList <- split(datframe, datframe$cluster)

  # Split each cluster into as many groups of size 9 as possible, while having
  # remainder groups. For clusters of size 10, we don't want size 1 groups left.
  y <- list()
  for (j in 1:length(clusterList)) {
    if (nrow(clusterList[[j]]) != (tableSize + 1)) {
      spl <- split(clusterList[[j]],
                   rep(1:ceiling(nrow(clusterList[[j]])/tableSize),
                       each = tableSize))
      y[[j]] <- spl
    } else if (nrow(clusterList[[j]]) == (tableSize + 1)) {
      spl <- split(clusterList[[j]],
                   rep(1:ceiling(nrow(clusterList[[j]]) /
                                   ceiling(((tableSize + 1)/2))),
                       each = ceiling((tableSize + 1)/2)))
      y[[j]] <- spl
    }
    y2 <- unlist(y, recursive = FALSE) #simple unlist
  }

  #put the size nine dataframes into a list and put the non size nine frames
  #into another list
  allSameCluster <- list()
  leftOvers <- list()
  for (k in 1:length(y2)) {
    if (nrow(y2[[k]]) == tableSize) {
      allSameCluster <- c(list(y2[[k]]), allSameCluster)
    } else if (nrow(y2[[k]]) != tableSize) {
        leftOvers <- c(list(y2[[k]]), leftOvers)
    }
  }

  #Deal with the Leftovers
  listIndex <- 1
  y5 <- list()

  for (clusterSize in 1:tableSize - 1) {
    i = 1
    while (i < length(leftOvers)) {
      noMatch <- TRUE
      # if y[i] == 1, loop through searching for an 8
      if (nrow(leftOvers[[i]]) == clusterSize) {
        for (h in (i + 1):length(leftOvers)) {
          # found an 8
          if (nrow(leftOvers[[h]]) == tableSize - clusterSize) {
            # assigning to new dataframe
            y5[[listIndex]] <- c(list(rbind(leftOvers[[i]], leftOvers[[h]])))
            listIndex <- listIndex + 1
            # remove entries from list
            leftOvers[[h]] <- NULL
            leftOvers[[i]] <- NULL
            # exit the inner for loop
            noMatch <- FALSE
            break
          }
        }
      }
      if (noMatch) {
        i <- i + 1
      }
    }
  }

  sameSize <- c(allSameCluster, y5)
  #split leftOvers into nice size groups to add together while making sure that
  #there are no splits that produce a 1.

  leftOversA <- list()

  for (i in 1:length(leftOvers)) {
    if (tableSize == 8 || tableSize == 9) {
      keep <- c(1:3, 6:8)
      split <- c(4:5)
    } else if (tableSize == 10) {
      keep <- c(1:4, 7:9)
      split <- c(5:6)
    } else if (tableSize == 11) {
      keep <- c(1:5, 8:10)
      split <- c(6:7)
    } else if (tableSize == 12) {
      keep <- c(1:6, 9:11)
      split <- c(7:8)
    }
    if (nrow(leftOvers[[i]]) %in% keep) {
      spl <- split(leftOvers[[i]], sample(rep(1)))
      leftOversA[[i]] <- spl
    } else if (nrow(leftOvers[[i]]) %in% split) {
      spl <- split(leftOvers[[i]], sample(rep(2)))
      leftOversA[[i]] <- spl
    }
    leftOvers2 <- unlist(leftOversA, recursive = FALSE)
  }

  #Deal with the Leftovers
  listIndex2 <- 1
  y6 <- list()

  for (clusterSize in 1:tableSize - 1) {
    i = 1
    while (i < length(leftOvers2)) {
      noMatch <- TRUE
      # if y[i] == 1, loop through searching for an 8
      if (nrow(leftOvers2[[i]]) == clusterSize) {
        for (h in (i + 1):length(leftOvers2)) {
          # found an 8
          if (nrow(leftOvers2[[h]]) == tableSize - clusterSize) {
            # assigning to new dataframe
            y6[[listIndex2]] <- c(list(rbind(leftOvers2[[i]], leftOvers2[[h]])))
            listIndex2 <- listIndex2 + 1
            # remove entries from list
            leftOvers2[[h]] <- NULL
            leftOvers2[[i]] <- NULL
            # exit the inner for loop
            noMatch <- FALSE
            break
          }
        }
      }
      if (noMatch) {
        i <- i + 1
      }
    }
  }

  sameSize2 <- c(sameSize, y6)

  notFullDf <- plyr::ldply(leftOvers2)[, -1]
  clusterListB <- split(notFullDf, notFullDf$cluster)


  leftOversB <- list()
  for (i in 1:length(clusterListB)) {
    if (tableSize == 8 || tableSize == 9) {
      keep <- c(1:5)
      split <- c(6:8, 10:12)
    } else if (tableSize == 10) {
      keep <- c(1:6)
      split <- c(7:9, 11:13)
    } else if (tableSize == 11) {
      keep <- c(1:7)
      split <- c(8:10, 12:14)
    } else if (tableSize == 12) {
      keep <- c(1:8)
      split <- c(9:11, 13:15)
    }
    if (nrow(clusterListB[[i]]) %in% keep) {
      spl <- split(clusterListB[[i]], sample(rep(1)))
      leftOversB[[i]] <- spl
    } else if (nrow(clusterListB[[i]]) %in% split) {
      spl <- split(clusterListB[[i]], sample(rep(2)))
      leftOversB[[i]] <- spl
    }
    leftOvers3 <- unlist(leftOversB, recursive = FALSE)
  }

  #combine nice numbers
  listIndex2 <- 1
  y7 <- list()

  for (clusterSize in 1:tableSize - 1) {
    i = 1
    while (i < length(leftOvers3)) {
      noMatch <- TRUE
      # if y[i] == 1, loop through searching for an 8
      if (nrow(leftOvers3[[i]]) == clusterSize) {
        for (h in (i + 1):length(leftOvers3)) {
          # found an 8
          if (nrow(leftOvers3[[h]]) == tableSize - clusterSize) {
            # assigning to new dataframe
            y7[[listIndex2]] <- c(list(rbind(leftOvers3[[i]], leftOvers3[[h]])))
            listIndex2 <- listIndex2 + 1
            # remove entries from list
            leftOvers3[[h]] <- NULL
            leftOvers3[[i]] <- NULL
            # exit the inner for loop
            noMatch <- FALSE
            break
          }
        }
      }
      if (noMatch) {
        i <- i + 1
      }
    }
  }

  sameSize3 <- c(sameSize2, y7)
  sameSize3 <- lapply(sameSize3, data.frame)

  return(list(fullTables = sameSize3, notFull = leftOvers3))
}

