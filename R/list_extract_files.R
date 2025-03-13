#' List OTN data extraction files
#'
#' @param project The project code.
#' @param detection_type one of, or a vector of, "all" (default), "matched",
#'    "external", "qualified", "sentinel_tag", or "unqualified". Partial
#'    matching is allowed, and will repair to the correct argument if spaces
#'    or the words detection(s)" are included.
#'    More information on data types can be found on
#'    \href{https://members.oceantrack.org/data/otn-detection-extract-documentation-matched-to-animals}{OTN's website}.
#' @param since Currently unused. Future documentation: Only list files
#'    uploaded after this date. Optional, but must be in YYYY-MM-DD format.
#'
#' @export
list_extract_files <- function(
  project,
  detection_type = c(
    "all",
    "matched",
    "external",
    "qualified",
    "sentinel",
    "unqualified"
  ),
  since = NULL
) {
  # Check and coerce input args
  detection_type <- gsub(" |detection[s]", "", detection_type)
  detection_type <- match.arg(detection_type, several.ok = TRUE)

  if ("external" %in% detection_type) {
    detection_type[detection_type == "external"] <- "matched_external_partners"
  }
  if ("sentinel" %in% detection_type) {
    detection_type[detection_type == "sentinel"] <- "sentinel_tag"
  }

  json_response <- paste0(
    "https://members.oceantrack.org/api/++api++/data/repository/",
    project_code,
    "/detection-extracts/"
  ) |>
    httr2::request() |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  links <- json_response$items[
    sapply(json_response$items, function(x) x$`@type`) == "File"
  ] |>
    sapply(function(x) x$`@id`)

  if (all(detection_type != "all")) {
    links <- grep(detection_type, links, value = TRUE)
  }

  links
}
