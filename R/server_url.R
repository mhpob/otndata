#' Set the OTN Plone instance with which you wish to interact.
#'
#' @param network Character. Lowercase code of the desired telemetry network. One of
#'   "otn", "act", "atap", "itag", "migramar", "nep", "npact", "pirat", or
#'   "otn_devel" (the OTN development server). Note that "etn", "fact", "glatos",
#'   "path" and "raft" are accepted, but only to produce an error and redirect you.
#' @param set Set the network in the current session?
.otn_server_url <- function(
  network = c(
    "otn",
    "act",
    "atap",
    "itag",
    "migramar",
    "nep",
    "npact",
    "pirat",
    "otn_devel",
    "etn",
    "fact",
    "glatos",
    "path",
    "raft"
  ),
  set = TRUE
) {
  network <- rlang::arg_match(network)
  server_url <- switch(
    network,
    # Legitimate servers:
    atap = , # evaluates to OTN
    migramar = , # evaluates to OTN
    nep = , # evaluates to OTN
    otn = "https://members.oceantrack.org",
    otn_devel = "https://members.devel.oceantrack.org",
    act = "https://data.theactnetwork.com",
    itag = "https://data.itagscience.com",
    npact = "https://plone.npact.aoos.org",
    pirat = "https://piratnetwork.org",

    # Servers that don't exist and should error:
    etn = cli::cli_abort(
      c(
        "ETN does not use an OTN-style Plone CMS.",
        "Check out the {.href [ETN R package](https://www.europeantrackingnetwork.org/en/etn-package)} instead."
      ),
    ),
    fact = cli::cli_abort("FACT does not use an OTN-style Plone CMS."),
    glatos = cli::cli_abort(
      c(
        "GLATOS does not use an OTN-style Plone CMS.",
        "Check out the {.href [glatos R package](https://ocean-tracking-network.r-universe.dev/glatos)} instead."
      )
    ),
    path = cli::cli_abort(
      c(
        "PATH does not have the Plone REST API activated.",
        "Work on this front is ongoing, so try again in a few months!"
      )
    ), # path = "https://fishdb.wfcb.ucdavis.edu",
    raft = cli::cli_abort("RAFT does not use an OTN-style Plone CMS.")
  )

  if (isTRUE(set)) {
    otn_global$server_url <- server_url
    otn_global$network <- ifelse(network == "atap", "saf", network)
  }
  invisible(server_url)
}
