# this is a junk test to just create a placeholder. It doesn't really test anything:
test_that("test data can be created", {
  testthat::skip_on_cran()
  library(LakeEnsemblR)
  
  testthat::expect_error(run_ensemble(model = c('GLM','GLM')), 
                         'model input argument cannot contain duplicates')
})
