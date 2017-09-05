context("Testing silWidth works correctly")

test_that("For tableSize = NULL, silWidth calculates widths correctly", {
  df <- data.frame(id = c(1:10),
                   a = c(7:16),
                   b = c("a", "b", "a", "a", "b", "b", "a", "b", "a", "b"),
                   c = c(1, 3, 4, 2, 4, 2, 5, 3, 1, 2),
                   e = c("stuff", "morestuff", "howmuchmore", "morestuff",
                         "stuff", "morestuff", "stuff", "howmuchmore",
                         "howmuchmore", "stuff"))
  df$c <- factor(df$c, levels = c("1", "2", "3", "4", "5"), ordered = TRUE)
  gower <- computeGowerDiss(df, id = "id")
  widthsssss <- silWidth(df = df, gowerDiss = gower$gower_diss)
  expect_equal(widthsssss$widths[[7]], 0.1414217, tolerance = 0.0000001)
})

test_that("For given tableSize, silWidth calculates widths correctly", {
  df <- data.frame(id = c(1:10),
                   a = c(7:16),
                   b = c("a", "b", "a", "a", "b", "b", "a", "b", "a", "b"),
                   c = c(1, 3, 4, 2, 4, 2, 5, 3, 1, 2),
                   e = c("stuff", "morestuff", "howmuchmore", "morestuff",
                         "stuff", "morestuff", "stuff", "howmuchmore",
                         "howmuchmore", "stuff"))
  df$c <- factor(df$c, levels = c("1", "2", "3", "4", "5"), ordered = TRUE)
  gower <- computeGowerDiss(df, id = "id")
  widthsssss <- silWidth(df = df, gowerDiss = gower$gower_diss, tableSize = 2)
  expect_equal(widthsssss$widths[[5]], 0.1261561, tolerance = 0.0000001)
})

test_that("For tableSize > nrow(df)/2, silWidth returns expected warning", {
  df <- data.frame(id = c(1:10),
                   a = c(7:16),
                   b = c("a", "b", "a", "a", "b", "b", "a", "b", "a", "b"),
                   c = c(1, 3, 4, 2, 4, 2, 5, 3, 1, 2),
                   e = c("stuff", "morestuff", "howmuchmore", "morestuff",
                         "stuff", "morestuff", "stuff", "howmuchmore",
                         "howmuchmore", "stuff"))
  df$c <- factor(df$c, levels = c("1", "2", "3", "4", "5"), ordered = TRUE)
  gower <- computeGowerDiss(df, id = "id")
  expect_warning(silWidth(df = df, gowerDiss = gower$gower_diss, tableSize = 6),
                 regexp = "will lead to subpar results.")
})

test_that("For tableSize = nrow(df), silWidth returns expected error", {
  df <- data.frame(id = c(1:10),
                   a = c(7:16),
                   b = c("a", "b", "a", "a", "b", "b", "a", "b", "a", "b"),
                   c = c(1, 3, 4, 2, 4, 2, 5, 3, 1, 2),
                   e = c("stuff", "morestuff", "howmuchmore", "morestuff",
                         "stuff", "morestuff", "stuff", "howmuchmore",
                         "howmuchmore", "stuff"))
  df$c <- factor(df$c, levels = c("1", "2", "3", "4", "5"), ordered = TRUE)
  gower <- computeGowerDiss(df, id = "id")
  expect_error(silWidth(df = df, gowerDiss = gower$gower_diss, tableSize = 10),
               "Specified tableSize must be less than")
})

test_that("For tableSize > nrow(df), silWidth returns expected error", {
  df <- data.frame(id = c(1:10),
                   a = c(7:16),
                   b = c("a", "b", "a", "a", "b", "b", "a", "b", "a", "b"),
                   c = c(1, 3, 4, 2, 4, 2, 5, 3, 1, 2),
                   e = c("stuff", "morestuff", "howmuchmore", "morestuff",
                         "stuff", "morestuff", "stuff", "howmuchmore",
                         "howmuchmore", "stuff"))
  df$c <- factor(df$c, levels = c("1", "2", "3", "4", "5"), ordered = TRUE)
  gower <- computeGowerDiss(df, id = "id")
  expect_error(silWidth(df = df, gowerDiss = gower$gower_diss, tableSize = 15),
               "Specified tableSize must be less than")
})


