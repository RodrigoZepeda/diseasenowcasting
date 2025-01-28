.onAttach <- function(libname, pkgname) {

  #Package start-up message
  cli::cli_h1("diseasenowcasting", class = "packageStartupMessage")
  cli::cli_li("To see what you can do go to {.url https://rodrigozepeda.github.io/diseasenowcasting/}.", class = "packageStartupMessage")
  cli::cli_li("To get help, report an issue, or suggest an enhancement go to: {.url https://github.com/RodrigoZepeda/diseasenowcasting/issues}.", class = "packageStartupMessage")

}

