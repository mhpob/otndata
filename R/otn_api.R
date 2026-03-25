#' Call OTN API endpoints
#'
#' @param server Character. URL of the OTN-style Plone server. Defaults to the
#'   main OTN server at <https://members.oceantrack.org>.
#' @param endpoint Character. API endpoint to call.
.otn_api <- function(server = NULL, endpoint) {
  if (is.null(server)) {
    server <- "https://members.oceantrack.org"
  }

  server |>
    httr2::request() |>
    httr2::req_url_path_append("++api++/++api++") |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_user_agent("otndata R Package")
}
