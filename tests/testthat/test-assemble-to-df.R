context("Testing assembleToDf is working correctly")

test_that("assembleToDf returns correct df given model build", {
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

  expect_equal(nrow(g$dfAssembled), 350)
  expect_equal(ncol(g$dfAssembled), 11)
  expect_equal(as.character(g$dfAssembled[10, 7]), "Yes")
  expect_equal(g$dfAssembled$X[2], -7.23779736, tolerance = 0.0000001)
})

