configureOpenML(show.info = FALSE)

# remove this after testthat upload
if (packageVersion("testthat") < "0.9.1.9000") {
  skip_on_travis <- function() {
    if (!identical(Sys.getenv("TRAVIS"), "true")) return()

    skip("On Travis")
  }
}
