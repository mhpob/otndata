#' Check to see if the user has provided a server
#'
#' @inheritParams .otn_api
#' @keywords internal
check_server <- function(server) {
  if (is.null(server)) {
    cli::cli_abort("Please provide a server, like `otn_login('otn')`.")
  }
}
