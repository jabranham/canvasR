library(canvasR)

test_that("Gets all questions", {
  expect_equal(nrow(import_quiz(quiz)), 7)
})

test_that("Gets all answers", {
  expect_equal(ncol(import_quiz(quiz)), 6)
})
