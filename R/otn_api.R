#' Call OTN API endpoints.
#'
#' @param endpoint Character. API endpoint to call.
#' @inheritParams .otn_server_url
#'
#' @keywords internal
.otn_api <- function(endpoint, server = otn_global$server) {
  server |>
    httr2::request() |>
    httr2::req_url_path_append("++api++") |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_user_agent("otndata R Package")
}
