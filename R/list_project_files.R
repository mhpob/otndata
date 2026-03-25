#' List OTN project files
#'
#' This function lists the file names, types, upload date, and URLs of OTN
#' project files -- basically everything you see in the *Data and Metadata*
#' section of your project page. You will be prompted to log in if it is not a
#' public project.
#'
#' @param project The project code.
list_project_files <- function(
  project
) {
  json_response <- paste0(
    "https://members.oceantrack.org/api/++api++/data/repository/",
    project,
    "/data-and-metadata"
  ) |>
    request() |>
    req_perform() |>
    resp_body_json()

  # check for files, drop templates
  # check for pages, drop into them

  links <- json_response$items[
    sapply(json_response$items, function(x) x$`@type`) == "Document"
  ] |>
    sapply(function(x) x$`@id`)
}
