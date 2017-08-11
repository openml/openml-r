context("listOMLFlowParameters")

skip_on_cran()

test_that("listOMLFlowParameters", {
  df = .listOMLFlowParameters(limit = 10) # works
  df = .listOMLFlowParameters(limit = 100) # works not
  df = .listOMLFlowParameters(flow.id = 21) # works not
})
