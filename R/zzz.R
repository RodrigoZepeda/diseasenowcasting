.onAttach <- function(libname, pkgname) {

  #Package start-up message
  msg <- cli::format_message(c(
    "{.emph diseasenowcasting}\n",
    "+ To see what you can do go to {.url https://rodrigozepeda.github.io/diseasenowcasting/}.",
    "+ To get help, report an issue, or suggest an enhancement go to: {.url https://github.com/RodrigoZepeda/diseasenowcasting/issues}."
  ))

  packageStartupMessage(msg)

}

