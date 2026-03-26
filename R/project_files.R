#' Return files associated with an OTN project
#'
#' @param project Character. The project code.
#' @param type Character. Portion of the URL representing the data type you wish
#'   to return.
#' @param batch_size Numeric. The number of results to return. Defaults to 25.
#' @inheritParams .otn_api
.otn_files <- function(project, type, batch_size = NULL, server = NULL) {
  session_token <- Sys.getenv("OTN_SESSION_TOKEN")
  project_endpoint <- paste(
    "data/repository",
    project,
    type,
    "@search",
    sep = "/"
  )

  the_files <- server |>
    .otn_api(project_endpoint) |>
    httr2::req_url_query(
      portal_type = "File",
      b_size = batch_size,
      metadata_fields = c(
        "Creator",
        "created",
        "modified",
        "getURL",
        "id",
        "title"
      ),
      .multi = "explode"
    ) |>
    httr2::req_auth_bearer_token(session_token) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    _$items |>
    lapply(
      \(.) {
        t(.) |> as.data.frame()
      }
    ) |>
    do.call(rbind, args = _)

  # Clean up names
  # This was an opinionated selection by M. O'Brien. Query
  #   `metadata_fields = "_all"` above to see all that are available.
  the_files <- the_files[,
    c(
      "title",
      "description",
      "getURL",
      "created",
      "modified",
      "Creator",
      "getObjSize",
      "type_title"
    )
  ]

  names(the_files) <- c(
    "name",
    "description",
    "url",
    "created",
    "modified",
    "creator",
    "size",
    "type"
  )

  # Up until now, the DF has had list columns.
  #   Likely a way to fix L33 to avoid this.
  the_files <- the_files |>
    lapply(unlist) |>
    data.frame()

  # Clean date-times
  convert_timestamps <- function(timestamp) {
    timestamp |>
      gsub("+00:00", "", x = _) |>
      as.POSIXct(tz = "UTC", format = "%FT%T")
  }

  the_files$created <- convert_timestamps(the_files$created)
  the_files$modified <- convert_timestamps(the_files$modified)

  return(the_files)
}

#' List OTN project files
#'
#' This function lists the file names, types, upload date, and URLs of OTN
#' project files -- basically everything you see in the *Data and Metadata*
#' section of your project page. You will be prompted to log in if it is not a
#' public project.
#'
#' @inheritParams .otn_files
#' @export
otn_project_files <- function(
  project,
  batch_size = NULL,
  server = NULL
) {
  .otn_files(project, "data-and-metadata", batch_size, server)
}

#' List OTN extract files
#'
#' This function recursively lists files in the *Detection Extracts* section of
#' your project.
#'
#' @inheritParams .otn_files
#' @export
otn_extract_files <- function(
  project,
  batch_size = NULL,
  server = NULL
) {
  .otn_files(project, "detection-extracts", batch_size, server)
}
