# Set the OTN Plone instance with which you wish to interact.

Set the OTN Plone instance with which you wish to interact.

## Usage

``` r
otn_set_server(
  server = c("otn", "act", "npact", "pirat", "devel", "etn", "fact", "glatos")
)
```

## Arguments

- server:

  Character. Lowercase network code of the desired server. One of "otn",
  "act", "npact", or "devel" (the OTN development server). Note that
  "etn", "fact", and "glatos" are accepted, but only to produce an error
  and redirect you.
