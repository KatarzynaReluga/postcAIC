modelset_matrix  = create_modelset("all_subsets", p = 5)
upsilon_example = compute_upsilon(modelset_matrix)

test_that("Output is correct", {
  expect_length(upsilon_example, 3)
  expect_match(class(upsilon_example), "list")

  expect_match(class(upsilon_example$upsilon)[1], "matrix")
  expect_match(class(upsilon_example$cov_terms_index0), "list")
  expect_length(upsilon_example$index_full_model, 1)
})
