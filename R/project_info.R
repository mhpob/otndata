otn_project_info <- function(code = NULL, server = NULL) {
  if (is.null(server)) {
    server <- "https://members.oceantrack.org"
  }
  server |>
    httr2::request() |>
    httr2::req_url_path_append("project") |>
    httr2::req_url_query(ccode = code) |>
    httr2::req_perform()
}
