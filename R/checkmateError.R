#' Construct a checkmate error condition
#'
#' Construct a condition object analogous to \code{base::simpleError} for
#' checkmate-style assertion failures. The returned object is an S3 condition
#' with class hierarchy \code{c("checkmateError", "simpleError", "error",
#' "condition")}.
#'
#' Because the condition inherits from \code{"error"}, it can be caught with
#' generic error handlers (e.g., \code{tryCatch(error = ...)}). You can also catch it
#' more specifically with handlers for the \code{checkmateError}.
#'
#' @param message Character string giving a human-readable description of the
#'   assertion failure.
#' @param call Optional call object where the condition occurred (default
#'   \code{NULL}). This can be used by handlers/printing methods to show context.
#'
#' @return An object of class \code{c("checkmateError", "simpleError", "error",
#' "condition")}.
#'
#' @seealso base::simpleError, base::signalCondition, base::stop,
#'   base::conditionMessage, base::conditionCall, base::withCallingHandlers,
#'   base::tryCatch
#'
#' @examples
#'
#' # Catch by class using tryCatch (specific handler)
#' tryCatch(
#'   stop(checkmateError("bad")),
#'   checkmateError = function(a) {
#'     message("caught checkmate assertion failure: ", conditionMessage(a))
#'     NULL
#'   }
#' )
#'
#' # Or catch generically as an error
#' tryCatch(
#'   stop(checkmateError("bad")),
#'   error = function(e) {
#'     message("caught as generic error: ", conditionMessage(e))
#'     NULL
#'   }
#' )
#'
#' # You can handle assertion errors separately from regular errors
#' tryCatch(
#'   stop(checkmateError("oops")),
#'   checkmateError = function(a) {
#'     message("an assertion failed: ", conditionMessage(a))
#'   },
#'   # This handler won't run because the one above will catch the assertion
#'   # failure
#'   error = function(e) {
#'     message("a runtime error occurred: ", conditionMessage(e))
#'   }
#' )
#'
#' @export
#' @rdname checkmateError
checkmateError <- function(message, call = NULL) {
  structure(
    list(
      message = as.character(message),
      call = call
    ),
    class = c("checkmateError", "simpleError", "error", "condition")
  )
}


#' @export
#' @rdname checkmateError
checkmate_error <- checkmateError
