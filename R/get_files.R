#' Download file(s) from an OTN-style Plone server
#'
#' Either `files` or `url` must be provided. If `files` is provided, files in
#' the data.frame are downloaded in parallel.
#'
#' @param files a data.frame returned by by `otn_project_files` or
#'   `otn_extract_files`. Can have been filtered or edited, but must have columns
#'   of "url" and "name".
#' @param url The URL of the file as returned by `otn_project_files` or
#'   `otn_extract_files`
#' @param outdir The output directory (where you want the file to be saved).
#'   Defaults to the current working directory.
#' @export
otn_get_file <- function(files = NULL, url = NULL, outdir = '.') {
  if (all(is.null(files), is.null(url))) {
    cli::cli_abort("Please provide one of `files` or `url`.")
  }
  session_token <- Sys.getenv("OTN_SESSION_TOKEN")

  build_request <- function(file_url) {
    file_url |>
      httr2::request() |>
      httr2::req_url_path_append("@@download/file") |>
      httr2::req_cookies_set(auth_token = session_token)
  }

  if (!dir.exists(outdir)) {
    dir.create(outdir)

    cli::cli_alert_info("Directory created: {outdir}.")
  }

  if (!is.null(files)) {
    responses <- lapply(
      files$url,
      function(x) {
        build_request(x) |>
          httr2::req_throttle(capacity = 2, fill_time_s = 1)
      }
    ) |>
      httr2::req_perform_parallel(
        paths = file.path(outdir, files$name),
        on_error = "continue"
      )

    cli::cli_alert_info("Files saved to {file.path(outdir, files$name)}.")
  } else {
    file_in_memory <- url |>
      build_request() |>
      httr2::req_perform()

    cd_header <- file_in_memory |>
      httr2::resp_header("Content-Disposition")

    file_name <- gsub("attachment; filename\\*=UTF-8''", '', cd_header)

    out_name <- file.path(outdir, file_name)

    file_in_memory |>
      httr2::resp_body_raw() |>
      writeBin(con = file.path(outdir, file_name))

    cli::cli_alert_info("File saved to {out_name}.")
  }
}
