context("Testing gatherTables is working correctly")

test_that("gatherTables returns correct fullTables, correct notFull tables", {
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
  f <- suppressWarnings(gatherTables(df = clust_dat, id = "personId",
                                     pam_model = d, tableSize = 9))
  expect_equal(length(f$fullTables), 37)
  expect_equal(f$fullTables[[32]][9, 1], 298)
  expect_equal(length(f$notFull), 5)
  expect_equal(f$notFull[[4]][3, 1], 280)
})
