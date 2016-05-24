#' Send a notification
#'
#' Uses the elements of the \code{notification} object to determine the message to send, and sends it. See \code{\link{notification}} for details of the notification object.
#' @param notification A list with at least a message and a start element. Extra elements depend on the type of notification.
#' @export
#' @examples
#' notification <- list("message" = "Done", start = Sys.time())
#' notify(notification)
#'
#' \dontrun{
#'  notification <- telegram("user", token = getToken())
#'  notification$start = Sys.time()
#'  notification$message = "Done"
#'  notify(notification)
#' }
notify <- function(notification, ...) UseMethod("notify")

#' Send a Telegram notification
#'
#' @describeIn notify Send a Telegram notification
#' @export
#' @inheritParams notify
notify.telegram <- function(notification, ...){
  url <- "https://api.telegram.org/bot"
  message <- notification$message
  elapsed <- round(as.numeric(Sys.time() - notification$start), digits = 3)
  message <- sprintf("`%s`", message)
  message <- paste(message, "\n*Elapsed:*", elapsed, "seconds")
  body <- list(chat_id = notification$chat_id, text = message, parse_mode = "Markdown")
  httr::POST(paste0(url, notification$token, "/sendMessage"), body = body)
}

#' @describeIn notify Send an email notification
#' @export
#' @inheritParams notify
notify.email <- function(notification, ...){

}

#' @describeIn notify Play a sound notification
notify.sound <- function(notification, ...){
  platform <- Sys.info()["sysname"]
  if(platform == "Linux"){
    tryCatch({
      res <- suppressWarnings(system2("aplay", args = c(notification$file), stdout = TRUE, stderr = TRUE))
    }
    )
  }
}

#' @inheritParams notify
#' @describeIn notify Print a notification
#' @export
notify.default <- function(notification, ...){
  cat(notification$message)
  cat("\n")
  elapsed <- round(as.numeric(Sys.time() - notification$start), digits = 3)
  cat(paste("Elapsed:", elapsed, "seconds\n"))
}
