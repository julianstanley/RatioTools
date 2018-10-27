library(RatioTools)

test_that("createPos returns a matrix with two columns", {
		  expect_equal(createPos(c(1,2), c(1,4)), cbind(matrix(c(1,2)), c(1,4)))
		  expect_equal(dim(createPos(c(1,2), c(1,4)))[2], 2)
})
