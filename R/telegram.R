#' Notifications
#'
#' Create a notification. This can be either through sending a Telegram message, an email, by printing a sound, or in its most basic form: printing a notification. A notification is sent through the matching \code{\link{notify}} function.
#' @details Below are details of each of the notification types.
#' @name notification
#' @param user Addressee of the notification.
NULL

#' @section Telegram:
#' There are two requirements before a notification can be send to a Telegram bot. First, the bot needs to be authorized, which should result in a token. Next, the target user needs to initiate a chat with the bot. If either requirement is not met, the function throws an error.
#' @param token Character. The bot-specific secret used to access the Telegram API.
#' @seealso The Telegram bot API at \url{https://core.telegram.org/bots/api#authorizing-your-bot}
#' @importFrom httr POST
#' @importFrom jsonlite fromJSON
#' @export
#' @rdname notification
telegram <- function(user, token = getToken()){
  notification <- list()
  url <- "https://api.telegram.org/bot"
  r <- POST(paste0(url, token, "/getUpdates"))

  if(r$status_code == 200){
    # Everything is okay
    # The payload is raw, so must be converted to character and then JSON for parsing
    content <- fromJSON(rawToChar(r$content))
    senders <- content$result$message$chat
    senders <- senders[which(senders$username == user),]
    # Check if there is a user with the specified username. Otherwise, we can't send a notification
    if(length(senders) > 0 && nrow(senders) > 0){
      # Everything is okay
      notification$chat_id <- senders$id[1]
      notification$token <- token
      class(notification) <- "telegram"
    }
    else {
      # Can't send anything without a chat_id
      stop("Cannot retrieve chat_id for this user. Has the user initiated a conversation with the bot?")

    }
  }
  else{
    if(r$status_code == 401 || r$status_code==411){
      stop("Bot is not authorized. Did you get a token?")
    }
    else {
      stop(paste("Status", r$status_code))
    }
  }
  notification
}


#' Retrieve or store a Telegram token
#'
#' Looks in \code{extdata/telegram.token} for a Telegram bot token
#' @name token
getToken <- function(){
  scan("extdata/telegram.token", what = character(), quiet = TRUE)
}

#' Save a Telegram token
#'
#' Saves the token as \code{extdata/telegram.token}
#' @rdname token
saveToken <- function(token){
  cat(token, file = "extdata/telegram.token")
}

