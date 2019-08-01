context("OMLSeedParList")

test_that("OMLSeedParList", {
  prefixes = c("openml", "bla")
  seeds = 1:2

  for (seed in seeds) {
    for (prefix in prefixes) {
      seed.par = makeOMLSeedParList(seed, prefix = prefix)
      expect_output(print(seed.par), "OMLSeedParList")
      for (i in seq_along(seed.par)) expect_is(seed.par[[i]], "OMLRunParameter")
      expect_is(seed.par, "OMLSeedParList")
      #expect_equal(paste0(prefix, ".", c("seed", "kind", "normal.kind")), unname(extractSubList(seed.par, "name")))
      expect_equal(c(seed, RNGkind()), unname(extractSubList(seed.par, "value")))

      # check isSeedPar
      expect_true(all(isSeedPar(seed.par)))

      # check extractSeed
      expect_equal(extractSeed(seed.par), seed)

      # check seed setting
      setOMLSeedParList(seed.par)
      n = rnorm(3)
      set.seed(seed = seed, kind = RNGkind()[1], normal.kind = RNGkind()[2])
      expect_equal(n, rnorm(3))
    }
  }

  with_test_cache({
    # get run without seed parameter
    run = getOMLRun(1L)
    par = getOMLSeedParList(run)
    expect_is(par, "OMLSeedParList")
    expect_equal(unclass(unname(par)), list())
  })
})
