#' List OTN data extraction files
#'
#' @param project The project code.
#' @param detection_type one of, or a vector of, "all" (default), "matched",
#'    "external", "qualified", "sentinel_tag", or "unqualified". Partial
#'    matching is allowed, and will repair to the correct argument if spaces
#'    or the words detection(s)" are included.
#'    More information on data types can be found on
#'    \href{https://members.oceantrack.org/data/otn-detection-extract-documentation-matched-to-animals}{OTN's website}.
#' @param since Only list files uploaded after this date. Optional, but must be
#'    in YYYY-MM-DD format.
#'
#' @return A data frame with columns of "project", "detection_type",
#'  'detection_year', 'file_name', and "url".
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
    project,
    "/detection-extracts/?b_size=100"
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

  file_metadata <- lapply(links, function(x) {
    httr2::request(x) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_throttle(capacity = 100, fill_time_s = 30)
  }) |>
    httr2::req_perform_parallel() |>
    lapply(httr2::resp_body_json)

  files <- data.frame(
    project = project,
    file_type = sapply(file_metadata, function(x) x$parent$title),
    detection_type = gsub(
      paste0(".*", project, "_(.*)_detections.*"),
      "\\1",
      links
    ),
    detection_year = as.numeric(gsub(".*detections_(.*)\\..*", "\\1", links)),
    upload_date = sapply(file_metadata, function(x) x$modified) |>
      as.POSIXct(format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    file_name = sapply(file_metadata, function(x) x$file$filename),
    url = sapply(file_metadata, function(x) x$file$download)
  )

  if (!is.null(since)) {
    # Check that date is in YYYY-MM-DD format
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", since)) {
      warning(
        paste0(
          "The \"since\" date was not provided in YYYY-MM-DD format.",
          "\nAll files have been returned."
        )
      )
    } else {
      files <- files[files$upload_date >= since, ]
    }
  }

  files
}
