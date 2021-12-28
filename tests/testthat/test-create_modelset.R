p = 10
modelset  = create_modelset("all_subsets", p = 10)

test_that("Output is correct", {
  expect_equal(is.matrix(modelset), TRUE)
  expect_match(class(modelset)[1], "matrix")
  expect_equal(ncol(modelset), p)
})


test_that("Output is correct", {
  expect_error(create_modelset("general", p = 5))
  expect_error(create_modelset("part_subset", p = 5, common = c(1:6)))
})

