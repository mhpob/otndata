#' Set the OTN Plone instance with which you wish to interact.
#'
#' @param server Character. Lowercase network code of the desired server. One of
#'   "otn", "npact", "devel" (the OTN development server). Note that "atn", "fact",
#'   and "etn" are accepted, but only to produce an error and redirect you.
otn_set_server <- function(
  server = c("otn", "npact", "act", "fact", "etn", "devel")
) {
  server <- rlang::arg_match(server)
  switch(
    server,
    # Legitimate servers:
    otn = "https://members.oceantrack.org",
    devel = "https://members.devel.oceantrack.org",
    npact = "https://plone.npact.aoos.org",

    # Servers that don't exist and should error:
    act = cli::cli_abort("ACT Plone server not yet implemented"),
    etn = cli::cli_abort(
      c(
        "ETN does not use an OTN-style Plone CMS.",
        "Check out the {.href [ETN R package](https://www.europeantrackingnetwork.org/en/etn-package)} instead."
      ),
    ),
    fact = cli::cli_abort("FACT Plone server not yet implemented")
  )
}
