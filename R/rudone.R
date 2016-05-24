#' Send a notification after executing some code
#'
#' There are two versions of this function: \code{rudone} for non-standard evaluation (interactive use), and \code{rudone_} for standard evaluation (programmatic use). Both wrap \code{rudone_exec}.
#' @param code Statements to execute. If there is more than one statement, they should be wrapped in curly brackets.
#' @param notification Notification object to send. See \code{\link{notification}} for details.
#' @param results Logical. Should the results be send along with the notification?
#' @seealso notification
#' @export
rudone <- function(code, notification, results = FALSE){
  expr <- substitute(code)
  message <- paste(trim_whitespace(deparse(expr)), collapse = "")
  # The starting and ending curly brackets can be ignored.
  message <- gsub("^\\{+|\\}+$", "", message)
  notification$message <- message

  rudone_exec(expr, notification, results)
}

#' @rdname rudone
#' @export
rudone_ <- function(fun, args, notification, results = FALSE){
  funExpr <- deparse(substitute(fun))
  argExpr <- deparse(substitute(args))
  expr <- expression(do.call(fun, args = args))

  notification$message <- make_call(as.character(funExpr), as.character(argExpr))
  rudone_exec(expr, notification, results)
}

#' Execute code and send notification
#'
#' Executes the expression, and calls \code{\link{notify}} with the notification
#' @param expr Expression.
#' @param notification Notification to send after the expression is executed
#' @param results Logical. If \code{results == TRUE}, the results of the \emph{final} statement are send along with the notification, if supported by the notification method.
rudone_exec <- function(expr, notification, results){
  start <- Sys.time()
  result <- eval(expr)
  if(results){
   t <- tempfile()
   notification$result <- TRUE
   notification$result_file <- t
   sink(file = t)

  }
  notification$start <- start
  notify(notification)
  result
}

