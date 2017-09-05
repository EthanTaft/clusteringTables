context("Testing plotClustering")

test_that("For given pam_fit, plotClustering returns correct tsne coordinates",
          {
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
 d <- buildPamModel(dissimilarities = b$gower_diss, k = 9)
 e <- plotClustering(df = clust_dat, gowerDiss = b$gower_diss, id = "personId",
                     pam_fit = d)
 expect_equal(e$tsneDat[18, 1], 15.70941057, tolerance = 0.0000001)
 expect_equal(e$tsneDat[21, 2], 3.83755595, tolerance = 0.0000001)
})
