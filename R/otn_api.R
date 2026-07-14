#' Call OTN API endpoints.
#'
#' @param endpoint Character. API endpoint to call.
#' @param server URL of the network's Plone CMS
#'
#' @keywords internal
.otn_api <- function(endpoint, server = otn_global$server_url) {
  server |>
    httr2::request() |>
    httr2::req_url_path_append("++api++") |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_user_agent("otndata R Package")
}
