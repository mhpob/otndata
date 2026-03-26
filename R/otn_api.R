#' Call OTN API endpoints
#'
#' @param server Character. URL of the OTN-style Plone server. Defaults to the
#'   main OTN server at <https://members.oceantrack.org>.
#' @param endpoint Character. API endpoint to call.
.otn_api <- function(server = "otn", endpoint) {
  otn_set_server(server) |>
    httr2::request() |>
    httr2::req_url_path_append("++api++") |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_user_agent("otndata R Package")
}
