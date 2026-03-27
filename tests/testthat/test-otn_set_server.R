test_that("Returns server addresses", {
  otn_set_server() |>
    expect_equal("https://members.oceantrack.org")
  otn_set_server("otn") |>
    expect_equal("https://members.oceantrack.org")
  otn_set_server("devel") |>
    expect_equal("https://members.devel.oceantrack.org")
  otn_set_server("npact") |>
    expect_equal("https://plone.npact.aoos.org")
})

test_that("Errors with not-yet-developed servers", {
  otn_set_server("act") |>
    expect_error("ACT Plone server not yet implemented")
  otn_set_server("etn") |>
    expect_error("ETN does not use an OTN-style Plone CMS.")
  otn_set_server("fact") |>
    expect_error("FACT Plone server not yet implemented")
})

test_that("Errors with gibberish", {
  otn_set_server("gibberish") |>
    expect_error("`server` must be one of.*gibberish")
})
