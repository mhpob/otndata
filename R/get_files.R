#' @export
otn_get_file <- function(file_url, outdir = '.') {
  session_token <- Sys.getenv("OTN_SESSION_TOKEN")

  # # Create a temporary directory to store intermediate files
  # td <- file.path(tempdir(), "otndata_files")

  # # remove previous files. Needed if things errored out.
  # if (file.exists(td)) {
  #   unlink(td, recursive = T)
  # }

  # dir.create(td)

  file_in_memory <- file_url |>
    httr2::request() |>
    httr2::req_url_path_append("@@download/file") |>
    httr2::req_cookies_set(auth_token = session_token) |>
    httr2::req_perform()

  cd_header <- file_in_memory |>
    httr2::resp_header("Content-Disposition")

  file_name <- gsub("attachment; filename\\*=UTF-8''", '', cd_header)

  file_in_memory |>
    httr2::resp_body_raw() |>
    writeBin(con = file.path(outdir, file_name))
}
