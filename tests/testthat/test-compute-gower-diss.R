context("Testing Computation of Gower Dissimilarities")

test_that("computeGowerDiss returns correct values for a given df", {
  df <- data.frame(id = c(1:10),
                   a = c(7:16),
                   b = c("a", "b", "a", "a", "b", "b", "a", "b", "a", "b"),
                   c = c(1, 3, 4, 2, 4, 2, 5, 3, 1, 2),
                   e = c("stuff", "morestuff", "howmuchmore", "morestuff",
                         "stuff", "morestuff", "stuff", "howmuchmore",
                         "howmuchmore", "stuff"))
  df$c <- factor(df$c, levels = c("1", "2", "3", "4", "5"), ordered = TRUE)
  gower <- computeGowerDiss(df, id = "id")
  expect_equal(gower$gower_diss[[9]], 0.5625000)
})

test_that("computeGowerDiss removes id column as specified", {
  df <- data.frame(id = c(1:9),
                   a = c(7:15),
                   b = c("a", "b", "a", "a", "b", "b", "a", "b", "a"),
                   c = c(1, 3, 4, 2, 4, 2, 5, 3, 1),
                   e = c("stuff", "morestuff", "howmuchmore", "morestuff",
                         "stuff", "morestuff", "stuff", "howmuchmore",
                         "howmuchmore"))
  df$c <- factor(df$c, levels = c("1", "2", "3", "4", "5"), ordered = TRUE)
  gower <- computeGowerDiss(df, id = "id")
  expect_equal(length(gower$gower_df), 4)
})

