.onLoad <- function(libname, pkgname) {
  S7::methods_register()

  # The S7 class names contain "::", which cannot appear in an S3 method *name*,
  # so `@exportS3Method` cannot express these registrations -- do them by hand on
  # the shared `generics::tidy` generic (see R/27_tidy.R).
  registerS3method("tidy", "diseasenowcasting::nowcast_prediction",
                   tidy_nowcast_prediction, envir = asNamespace("generics"))
  registerS3method("tidy", "diseasenowcasting::nowcast",
                   tidy_nowcast, envir = asNamespace("generics"))

  invisible()
}
