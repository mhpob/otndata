#' Set the OTN Plone instance with which you wish to interact.
#'
#' @param server Character. Lowercase network code of the desired server. One of
#'   "otn", "npact", "act", "fact", or "devel" (the OTN development server).
otn_set_server <- function(server = c("otn", "npact", "act", "fact", "devel")) {
  switch(
    server,
    otn = "https://members.oceantrack.org",
    devel = "https://members.devel.oceantrack.org",
    npact = "https://plone.npact.aoos.org",
    act = stop("ACT Plone server not yet implemented"),
    fact = stop("FACT Plone server not yet implemented")
  )
}
