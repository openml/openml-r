context("listOMLFlowParameters")

skip_on_cran()

test_that("listOMLFlowParameters", {
  df = .listOMLFlowParameters(limit = 1)
  df = .listOMLFlowParameters(limit = 10)
  df = .listOMLFlowParameters(limit = 100)
  with_main_server({
    df = .listOMLFlowParameters(flow.id = 5685, limit = 1000) # works 
    df = .listOMLFlowParameters(flow.id = 1821, limit = 1000) # works 
  })
})
