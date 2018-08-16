context("test-interleave.R")

test_that("interleaving works", {
  expect_equal(interleave(1:2, 3:4), c(1, 3, 2, 4))
  expect_equal(interleave(1:3, 4:5), c(1, 4, 2, 5, 3))
  expect_equal(interleave(4:5, 1:3), c(4, 1, 5, 2, 3))
})
