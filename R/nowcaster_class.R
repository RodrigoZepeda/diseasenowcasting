#' Disease Nowcaster
#'
#' The Disease Nowcaster Class called `dnowcaster`. Provides a wrapper for all the nowcasters
#' availables to homogenize summary functions, print functions, fitting and updating functions
#'
#'
#' @section Usage:
#' \preformatted{py = PythonEnv$new(port, path)
#'
#' py$start()
#'
#' py$running
#'
#' py$exec(..., file = NULL)
#' py$stop(force = FALSE)
#'
#' }
#'
#' @section Arguments:
#' \code{port} The port to use for communication with Python.
#'
#' \code{path} The path to the Python executable.
#'
#' \code{...} Commands to run or named variables to set in the Python process.
#'
#' \code{file} File containing Python code to execute.
#'
#' \code{force} If \code{TRUE}, force the Python process to terminate
#'   using a sytem call.
#'
#' @section Methods:
#' \code{$new()} Initialize a Python interface. The Python process is not
#'   started automatically.
#'
#' \code{$start()} Start the Python process. The Python process runs
#'   asynchronously.
#'
#' \code{$running} Check if the Python process is running.
#'
#' \code{$exec()} Execute the specified Python
#'   commands and invisibly return printed Python output (if any).
#'   Alternatively, the \code{file} argument can be used to specify
#'   a file containing Python code. Note that there will be no return
#'   value unless an explicit Python \code{print} statement is executed.
#'
#' \code{$stop()} Stop the Python process by sending a request to the
#'   Python process. If \code{force = TRUE}, the process will be
#'   terminated using a system call instead.
#'
#' @name dnowcaster
#' @examples
#' message("No Python distribution found!")
NULL

#' @export
dnowcaster = R6::R6Class("dnowcaster", cloneable = FALSE)

