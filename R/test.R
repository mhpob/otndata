# library(httr2)

# json_response <- 'https://members.oceantrack.org/api/++api++/data/repository/pbsm/data-and-metadata' |>
#   request() |>
#   req_perform() |>
#   resp_body_json()

# links <- json_response$items[
#   sapply(json_response$items, function(x) x$`@type`) == "Document"
# ] |>
#   sapply(function(x) x$`@id`)

# links_by_year <- lapply(links, function(x) {
#   httr2::request(x) |>
#     httr2::req_headers(Accept = "application/json") |>
#     httr2::req_throttle(capacity = 100, fill_time_s = 30)
# }) |>
#   httr2::req_perform_parallel() |>
#   lapply(httr2::resp_body_json)

# j <- data.frame(
#   type = sapply(links_by_year, function(x) {
#     sapply(x$items, function(y) y$title)
#   }) |>
#     unlist(),
#   id = sapply(links_by_year, function(x) {
#     sapply(x$items, function(y) y$`@id`)
#   }) |>
#     unlist()
# )

# json_response <- paste0(
#   "https://members.oceantrack.org/api/++api++/data/repository/",
#   project,
#   "/detection-extracts/@querystring-search"
# ) |>
#   request() |>
#   req_body_json(
#     list(
#       "query" = list(
#         list(
#           "i" = "modified",
#           "o" = "plone.app.querystring.operation.date.largerThan",
#           "v" = "2025-01-01"
#         ),
#         list(
#           "i" = "portal_type",
#           "o" = "plone.app.querystring.operation.selection.any",
#           "v" = "File"
#         )
#       )
#     )
#   ) |>
#   req_perform()
