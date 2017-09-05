context("Testing perfMetrics is working correclty")

test_that("perfMetrics returns correct mostCommon value", {
  set.seed(1987)
  n = 350
  clust_dat <-
    data.frame(personId = 1:n,
               networkPref = sample(c("topic", "jobtitle", "orgtype"),
                                    size = n, replace = TRUE),
               Age = sample(23:65, size = n, replace = TRUE),
               familyImp = sample(c(1, 2, 3, 4, 5), size = n, replace = TRUE,
                                  prob = c(0.01, 0.02, 0.4, 0.10, 0.83)),
               howOld = sample(1:100, size = n, replace = TRUE),
               horror = sample(c("Yes", "No"), size = n, replace = TRUE,
                               prob = c(0.27, 0.73)),
               sailBoat = sample(c("Yes", "No"), size = n, replace = TRUE,
                                 prob = c(0.58, 0.42)))

  clust_dat$familyImp <- factor(clust_dat$familyImp,
                                levels = c("1", "2", "3", "4", "5"),
                                ordered = TRUE)


  b <- computeGowerDiss(clust_dat, id = "personId")
  d <- buildPamModel(dissimilarities = b$gower_diss, k = 13)
  e <- plotClustering(clust_dat, gowerDiss = b$gower_diss, id = "personId",
                      pam_fit = d)
  f <- suppressWarnings(gatherTables(df = clust_dat, id = "personId",
                                     pam_model = d, tableSize = 9))
  loTidyFrames <- list(rbind(f$notFull[[1]], f$notFull[[2]], f$notFull[[5]]))
  loTidyFrames2 <- list(rbind(f$notFull[[3]], f$notFull[[4]]))
  allTables <- c(f$fullTables, loTidyFrames, loTidyFrames2)
  g <- assembleToDf(tableList = allTables, df = clust_dat, id = "personId",
                    tsne_results = e$tsneDat)
  h <- perfMetrics(merged_data = g$dfAssembled)
  expect_equal(h$mostCommon, 0.9298413, tolerance = 0.0000001)
})

test_that("perfMetrics returns correct numClusters value", {
  set.seed(1987)
  n = 350
  clust_dat <-
    data.frame(personId = 1:n,
               networkPref = sample(c("topic", "jobtitle", "orgtype"),
                                    size = n, replace = TRUE),
               Age = sample(23:65, size = n, replace = TRUE),
               familyImp = sample(c(1, 2, 3, 4, 5), size = n, replace = TRUE,
                                  prob = c(0.01, 0.02, 0.4, 0.10, 0.83)),
               howOld = sample(1:100, size = n, replace = TRUE),
               horror = sample(c("Yes", "No"), size = n, replace = TRUE,
                               prob = c(0.27, 0.73)),
               sailBoat = sample(c("Yes", "No"), size = n, replace = TRUE,
                                 prob = c(0.58, 0.42)))

  clust_dat$familyImp <- factor(clust_dat$familyImp,
                                levels = c("1", "2", "3", "4", "5"),
                                ordered = TRUE)


  b <- computeGowerDiss(clust_dat, id = "personId")
  d <- buildPamModel(dissimilarities = b$gower_diss, k = 13)
  e <- plotClustering(clust_dat, gowerDiss = b$gower_diss, id = "personId",
                      pam_fit = d)
  f <- suppressWarnings(gatherTables(df = clust_dat, id = "personId",
                                     pam_model = d, tableSize = 9))
  loTidyFrames <- list(rbind(f$notFull[[1]], f$notFull[[2]], f$notFull[[5]]))
  loTidyFrames2 <- list(rbind(f$notFull[[3]], f$notFull[[4]]))
  allTables <- c(f$fullTables, loTidyFrames, loTidyFrames2)
  g <- assembleToDf(tableList = allTables, df = clust_dat, id = "personId",
                    tsne_results = e$tsneDat)
  h <- perfMetrics(merged_data = g$dfAssembled)
  expect_equal(h$numClusters, 1.202857, tolerance = 0.00001)
})

test_that("perfMetrics returns correct final dataframe", {
  set.seed(1987)
  n = 350
  clust_dat <-
    data.frame(personId = 1:n,
               networkPref = sample(c("topic", "jobtitle", "orgtype"),
                                    size = n, replace = TRUE),
               Age = sample(23:65, size = n, replace = TRUE),
               familyImp = sample(c(1, 2, 3, 4, 5), size = n, replace = TRUE,
                                  prob = c(0.01, 0.02, 0.4, 0.10, 0.83)),
               howOld = sample(1:100, size = n, replace = TRUE),
               horror = sample(c("Yes", "No"), size = n, replace = TRUE,
                               prob = c(0.27, 0.73)),
               sailBoat = sample(c("Yes", "No"), size = n, replace = TRUE,
                                 prob = c(0.58, 0.42)))

  clust_dat$familyImp <- factor(clust_dat$familyImp,
                                levels = c("1", "2", "3", "4", "5"),
                                ordered = TRUE)


  b <- computeGowerDiss(clust_dat, id = "personId")
  d <- buildPamModel(dissimilarities = b$gower_diss, k = 13)
  e <- plotClustering(clust_dat, gowerDiss = b$gower_diss, id = "personId",
                      pam_fit = d)
  f <- suppressWarnings(gatherTables(df = clust_dat, id = "personId",
                                     pam_model = d, tableSize = 9))
  loTidyFrames <- list(rbind(f$notFull[[1]], f$notFull[[2]], f$notFull[[5]]))
  loTidyFrames2 <- list(rbind(f$notFull[[3]], f$notFull[[4]]))
  allTables <- c(f$fullTables, loTidyFrames, loTidyFrames2)
  g <- assembleToDf(tableList = allTables, df = clust_dat, id = "personId",
                    tsne_results = e$tsneDat)
  h <- perfMetrics(merged_data = g$dfAssembled)
  expect_equal(ncol(h$finalDf), 13)
  expect_equal(nrow(h$finalDf), 350)
  expect_equal(h$finalDf$mostCommon[5], 1.0000000)
  expect_equal(h$finalDf$mostCommon[71], 0.3333333, tolerance = 0.000001)
  expect_equal(h$finalDf$numClusters[76], 3)
})
