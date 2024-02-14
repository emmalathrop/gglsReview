test_that("comparison_maker works", {
  expect_equal(comparison_maker(ds, "Sample"),
               list(c("sample2", "sample1"),
                    c("sample3", "sample2"),
                    c("sample3", "sample1")))
})
