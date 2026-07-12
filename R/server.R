#' Set the OTN Plone instance with which you wish to interact.
#'
#' @param server Character. Lowercase network code of the desired server. One of
#'   "otn", "act", "npact", or "devel" (the OTN development server). Note that
#'   "etn", "fact", and "glatos" are accepted, but only to produce an error and redirect you.
otn_set_server <- function(
  server = c(
    "otn",
    "act",
    "npact",
    "pirat",
    "path",
    "devel",
    "etn",
    "fact",
    "glatos"
  )
) {
  server <- rlang::arg_match(server)
  switch(
    server,
    # Legitimate servers:
    act = "https://data.theactnetwork.com",
    devel = "https://members.devel.oceantrack.org",
    npact = "https://plone.npact.aoos.org",
    otn = "https://members.oceantrack.org",
    # path = "https://fishdb.wfcb.ucdavis.edu",
    pirat = "https://piratnetwork.org",

    # Servers that don't exist and should error:
    etn = cli::cli_abort(
      c(
        "ETN does not use an OTN-style Plone CMS.",
        "Check out the {.href [ETN R package](https://www.europeantrackingnetwork.org/en/etn-package)} instead."
      ),
    ),
    fact = cli::cli_abort("FACT Plone server not yet implemented"),
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
    )
  )
}
