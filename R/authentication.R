#' Authenticate to an Ocean Tracking Network server
#'
#' This function prompts you for the username and password associated with
#' your OTN account. This is necessary so that you may interface with any
#' project-specific files.
#'
#' A pop up will appear asking for your username and password. If everything works
#' out, your credentials will be kept in the sessions' cookies. Your username/password
#' will not be saved -- this was done intentionally so that you don't accidentally
#' save credentials in a public script.
#'
#' @inheritParams .otn_api
#' @param set_credentials Logical. Provide credentials for the current
#'   session only?
#' @export

otn_login <- function(
  server = NULL,
  set_credentials = FALSE
) {
  if (isTRUE(set_credentials)) {
    otn_set_credentials(temporary = TRUE)
  }
  creds <- c(login = Sys.getenv("OTN_USER"), pass = Sys.getenv("OTN_PASS"))

  if (any(creds == "")) {
    cli::cli_abort("Credentials missing.")
  }

  login_request <- server |>
    .otn_api("@login") |>
    httr2::req_body_json(
      list(
        login = creds["login"],
        password = creds["pass"]
      )
    )

  withCallingHandlers(
    login_response <- login_request |>
      httr2::req_perform() |>
      httr2::resp_body_json(),
    httr2_http_401 = function(cnd) {
      rlang::abort("Login failed. Check the supplied credentials", parent = cnd)
    }
  )

  cli::cli_alert_success("Login successful!")
  invisible(login_response)
}

#' Install your OTN username and password in your \code{.Renviron} File for repeated use
#'
#' @description This code was adapted from \href{https://github.com/walkerke/tidycensus/blob/ddb33b5f72734a4ff14332bd55cbac4850688600/R/helpers.R}{\code{tidycensus::census_api_key}}. Note that this saves your credentials in your .Renviron, meaning that anyone who is using your computer can theoretically access what your MATOS username and password are. So... use this carefully!
#'
#' @param temporary Logical. Scrub credentials after the current session?
#' @param overwrite Logical. Overwrite previously-stored MATOS credentials?
#'
#' @export
#' @examples
#' \dontrun{
#' otn_set_credentials()
#' }
#' # Yup, that's it!
otn_set_credentials <- function(temporary = TRUE, overwrite = FALSE) {
  if (isTRUE(temporary)) {
    username <- getPass::getPass("Username:", noblank = T)
    password <- getPass::getPass("Password:", noblank = T)

    Sys.setenv(paste0("OTN_USER='", username, "'"))
    Sys.setenv(paste0("OTN_PASS='", password, "'"))
  } else {
    home <- Sys.getenv("HOME")
    renv_path <- file.path(home, ".Renviron")

    if (!file.exists(renv_path)) {
      file.create(renv_path)
    }

    renv <- readLines(renv_path)

    if (any(grepl("OTN", renv), grepl("OTN", Sys.getenv())) && overwrite == F) {
      cli::cli_abort(
        "Some OTN credentials already exist. You can overwrite them with the argument overwrite=TRUE."
      )
    }

    username <- getPass::getPass("Username:", noblank = T)
    password <- getPass::getPass("Password:", noblank = T)

    username <- paste0("OTN_USER='", username, "'")
    password <- paste0("OTN_PASS='", password, "'")

    # Append API key to .Renviron file
    write(username, renv_path, sep = "\n", append = TRUE)
    write(password, renv_path, sep = "\n", append = TRUE)

    cli::cli_alert_info(
      'Your OTN credentials have been stored in your .Renviron and can be accessed by Sys.getenv("MATOS_USER") or Sys.getenv("MATOS_PASS"). \nTo use now, restart R or run `readRenviron("~/.Renviron")`'
    )
  }
}
