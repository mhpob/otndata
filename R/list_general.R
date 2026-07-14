#' Internal function to list small OTN databases.
#'
#' Provides access to the data that is on the main OTN members page. Login is
#'   not required.
#'
#' @param type Character. Internal data base to retrieve.
#' @inheritParams .otn_server_url network
#'
#' @keywords internal
.otn_list <- function(network, type) {
  endpoint <- switch(
    type,
    contacts = "all_contacts.json",
    countries = "active_countries.json",
    species = "all_species.json",
    institutions = "all_institutions.json",
    projects = "projects.json",
    stats = "header_stats.json"
  )

  endpoint |>
    .otn_api(server = .otn_server_url(network, set = FALSE)) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
}

#' Retrieve OTN contact list.
#'
#' @inheritParams .otn_list
#' @export
otn_list_contacts <- function(network = "otn") {
  .otn_list(network, "contacts")
}

#' Retrieve OTN country list.
#'
#' @inheritParams .otn_list
#' @export
otn_list_countries <- function(network = "otn") {
  .otn_list(network, "countries")
}

#' Retrieve OTN species list.
#'
#' @inheritParams .otn_list
#' @export
otn_list_species <- function(network = "otn") {
  .otn_list(network, "species")
}

#' Retrieve OTN institution list.
#'
#' @inheritParams .otn_list
#' @export
otn_list_institutions <- function(network = "otn") {
  .otn_list(network, "institutions")
}

#' Retrieve OTN project list.
#'
#' @inheritParams .otn_list
#' @export
otn_list_projects <- function(network = "otn") {
  .otn_list(network, "projects")
}

#' Retrieve OTN network statistics.
#'
#' @inheritParams .otn_list
#' @export
otn_list_stats <- function(network = "otn") {
  .otn_list(network, "stats")
}
