#' Internal function to list small OTN data bases
#'
#' Provides access to the data that is on the main OTN members page. Login is
#'   not required.
#'
#' @param type Character. Internal data base to retrieve.
#' @inheritParams .otn_api
.otn_list <- function(server = NULL, type) {
  endpoint <- switch(
    type,
    contacts = "all_contacts.json",
    countries = "active_countries.json",
    species = "all_species.json",
    institutions = "all_institutions.json",
    projects = "projects.json",
    stats = "header_stats.json"
  )

  server |>
    .otn_api(endpoint) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
}

#' Retrieve OTN contact list
#'
#' @inheritParams .otn_list
#' @export
otn_list_contacts <- function(server = "otn") {
  .otn_list(server, "contacts")
}

#' Retrieve OTN country list
#'
#' @inheritParams .otn_list
#' @export
otn_list_countries <- function(server = "otn") {
  .otn_list(server, "countries")
}

#' Retrieve OTN species list
#'
#' @inheritParams .otn_list
#' @export
otn_list_species <- function(server = "otn") {
  .otn_list(server, "species")
}

#' Retrieve OTN institution list
#'
#' @inheritParams .otn_list
#' @export
otn_list_institutions <- function(server = "otn") {
  .otn_list(server, "institutions")
}

#' Retrieve OTN project list
#'
#' @inheritParams .otn_list
#' @export
otn_list_projects <- function(server = "otn") {
  .otn_list(server, "projects")
}

#' Retrieve OTN server statistics
#'
#' @inheritParams .otn_list
#' @export
otn_list_stats <- function(server = "otn") {
  .otn_list(server, "stats")
}
