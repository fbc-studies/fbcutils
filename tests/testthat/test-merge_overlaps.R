context("test-merge_overlaps.R")

test_that("overlapping intervals are merged", {
  df <- data.frame(x = c(1, 2), y = c(3, 4))
  out <- merge_overlaps(df, x, y)
  expect_equal(nrow(out), 1L)
})

# TODO: way more tests

test_that("meged intervals have maximal end", {
  skip("todo")
})

test_that("meged intervals have minimal start", {
  skip("todo")
})

test_that("can merge with a specified gap", {
  skip("todo")
})

test_that("initial row order does not matter", {
  skip("todo")
})

test_that("existing groups are preserved", {
  skip("todo")
})


# Identification ----------------------------------------------------------

test_that("overlapping intervals are identified", {
  df <- data.frame(x = c(1, 2), y = c(3, 4))
  out <- identify_overlaps(df, x, y)
  expect_equal(out$.seq, c(1, 1))
})