context("Testing that buildPamModel is working")

test_that("For given dissimilarity object and silWidth buildPamModel returns
          expected diameter for cluster 1", {
  df <- data.frame(id = c(1:10),
                   a = c(7:16),
                   b = c("a", "b", "a", "a", "b", "b", "a", "b", "a", "b"),
                   c = c(1, 3, 4, 2, 4, 2, 5, 3, 1, 2),
                   e = c("stuff", "morestuff", "howmuchmore", "morestuff",
                         "stuff", "morestuff", "stuff", "howmuchmore",
                         "howmuchmore", "stuff"))
  df$c <- factor(df$c, levels = c("1", "2", "3", "4", "5"), ordered = TRUE)
  gower <- computeGowerDiss(df, id = "id")
  w <- silWidth(df = df, gowerDiss = gower$gower_diss, tableSize = 2)
  mymodel <- buildPamModel(dissimilarities = gower$gower_diss,
                           silWidths = w$widths)
  expect_equal(mymodel$clusinfo[1, 4][[1]], 0.5555556, tolerance = 0.00001)
})

test_that("For given dissimilarity object and k buildPamModel returns
          expected diameter for cluster 1", {
  df <- data.frame(id = c(1:10),
                   a = c(7:16),
                   b = c("a", "b", "a", "a", "b", "b", "a", "b", "a", "b"),
                   c = c(1, 3, 4, 2, 4, 2, 5, 3, 1, 2),
                   e = c("stuff", "morestuff", "howmuchmore", "morestuff",
                         "stuff", "morestuff", "stuff", "howmuchmore",
                         "howmuchmore", "stuff"))
  df$c <- factor(df$c, levels = c("1", "2", "3", "4", "5"), ordered = TRUE)
  gower <- computeGowerDiss(df, id = "id")
  w <- silWidth(df = df, gowerDiss = gower$gower_diss, tableSize = 2)
  mymodel <- buildPamModel(dissimilarities = gower$gower_diss, k = 3)
  expect_equal(mymodel$clusinfo[1, 4][[1]], 0.4930556, tolerance = 0.00001)
})

test_that("For given dissimilarity object and k and silWidth buildPamModel uses
          which.max(silWidth) as k instead of k returns expected diameter for
          cluster 1", {
  df <- data.frame(id = c(1:10),
                   a = c(7:16),
                   b = c("a", "b", "a", "a", "b", "b", "a", "b", "a", "b"),
                   c = c(1, 3, 4, 2, 4, 2, 5, 3, 1, 2),
                   e = c("stuff", "morestuff", "howmuchmore", "morestuff",
                         "stuff", "morestuff", "stuff", "howmuchmore",
                         "howmuchmore", "stuff"))
  df$c <- factor(df$c, levels = c("1", "2", "3", "4", "5"), ordered = TRUE)
  gower <- computeGowerDiss(df, id = "id")
  w <- silWidth(df = df, gowerDiss = gower$gower_diss, tableSize = 2)
  mymodel <- buildPamModel(dissimilarities = gower$gower_diss,
                           silWidths = w$widths, k = 3)
  expect_equal(mymodel$clusinfo[1, 4][[1]], 0.5555556, tolerance = 0.00001)
})

test_that("For given dissimilarity, and missing silWidth and k, buildPamModel
          returns expected error", {
  df <- data.frame(id = c(1:10),
                   a = c(7:16),
                   b = c("a", "b", "a", "a", "b", "b", "a", "b", "a", "b"),
                   c = c(1, 3, 4, 2, 4, 2, 5, 3, 1, 2),
                   e = c("stuff", "morestuff", "howmuchmore", "morestuff",
                         "stuff", "morestuff", "stuff", "howmuchmore",
                         "howmuchmore", "stuff"))
  df$c <- factor(df$c, levels = c("1", "2", "3", "4", "5"), ordered = TRUE)
  gower <- computeGowerDiss(df, id = "id")
  w <- silWidth(df = df, gowerDiss = gower$gower_diss, tableSize = 2)
  expect_error(buildPamModel(dissimilarities = gower$gower_diss),
               "You must specify a vector")

})





