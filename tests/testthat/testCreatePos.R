context("testCreatePos.r")

library(RatioTools)

test_that("createPos returns a matrix with two columns", {
		  expect_equal(createPos(c(1,2), c(1,4)), cbind(matrix(c(1,2)), c(1,4)))
		  expect_equal(dim(createPos(c(1,2), c(1,4)))[2], 2)
})

test_that("createPos errors when given empty vectors", {
		  # Warning should be called by remove.na from a null vector
		  expect_warning(expect_error(createPos(c(),c())))
})

test_that("createPos removes NULL values from vectors", {
		  expect_equal(createPos(c(1,2,NA), c(1,NA,4)), 
			       cbind(matrix(c(1,2)), c(1,4)))
})




