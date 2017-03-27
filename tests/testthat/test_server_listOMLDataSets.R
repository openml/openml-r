context("listOMLDataSets")

test_that("listOMLDataSets", {
  exp.names = c("data.id", "status", "format", "name", "majority.class.size",
    "max.nominal.att.distinct.values", "minority.class.size", #"num.binary.atts",
    "number.of.classes", "number.of.features", "number.of.instances",
    "number.of.instances.with.missing.values", "number.of.missing.values",
    "number.of.numeric.features", "number.of.symbolic.features")
  
  with_main_server({
    for (dsl in list(.listOMLDataSets(limit = 10L), .listOMLDataSets(tag = "study_1", limit = 10L))) {
      expect_data_frame(dsl, col.names = "unique", min.rows = 1)
      #expect_set_equal(names(dsl), exp.names)
      expect_integer(dsl$data.id, any.missing = FALSE, unique = TRUE)
      expect_character(dsl$status, any.missing = FALSE)
      expect_character(dsl$name, any.missing = FALSE)
      expect_character(dsl$tags, any.missing = FALSE)
    }
    ds = .listOMLDataSets(number.of.instances = c(40, 100), number.of.features = c(5, 10),
      number.of.classes = 2, number.of.missing.values = 0)
    expect_true(min(ds$number.of.instances) >= 40)
    expect_true(max(ds$number.of.instances) <= 100)
    expect_true(min(ds$number.of.features) >= 5)
    expect_true(max(ds$number.of.features) <= 10)
    expect_true(unique(ds$number.of.classes) == 2)
    expect_true(unique(ds$number.of.missing.values) == 0)
    
    # check if status works
    ds = .listOMLDataSets(status = "in_preparation", limit = 10)
    expect_string(unique(ds$status))
    expect_true(unique(ds$status) == "in_preparation")
    
    ds = .listOMLDataSets(status = "deactivated", limit = 10)
    expect_data_frame(ds, nrows = 10L, col.names = "unique")
    expect_string(unique(ds$status))
    expect_true(unique(ds$status) == "deactivated")
  })
})

test_that("listOMLDataSets by data.name", {
  with_main_server({
    exp.name = "iris"
    data.sets = .listOMLDataSets(data.name = exp.name)
    # there needs to be at least on iris version
    expect_data_frame(data.sets, col.names = "unique", min.rows = 1)
    expect_true(all(data.sets$name == exp.name))
  })
})
