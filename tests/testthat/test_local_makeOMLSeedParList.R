context("makeOMLSeedParList")

test_that("makeOMLSeedParList", {
  prefixes = c("openml", "bla")
  seeds = 1:2
  
  for (seed in seeds) {
    for (prefix in prefixes) {
      seed.par = makeOMLSeedParList(seed, prefix = prefix)
      for(i in seq_along(seed.par)) expect_is(seed.par[[i]], "OMLRunParameter")
      expect_is(seed.par, "OMLSeedParList")
      expect_equal(paste0(prefix, ".", c("seed", "kind", "normal.kind")), unname(extractSubList(seed.par, "name")))
      expect_equal(c(seed, RNGkind()), unname(extractSubList(seed.par, "value")))
      
      # check isSeedPar
      expect_true(all(isSeedPar(seed.par)))
      
      # check extractSeed
      expect_equal(extractSeed(seed.par), seed)
    }
  }
})
