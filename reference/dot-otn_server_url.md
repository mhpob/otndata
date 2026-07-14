# Set the OTN Plone instance with which you wish to interact.

Set the OTN Plone instance with which you wish to interact.

## Usage

``` r
.otn_server_url(
  network = c("otn", "act", "atap", "itag", "migramar", "nep", "npact", "pirat",
    "otn_devel", "etn", "fact", "glatos", "path", "raft"),
  set = TRUE
)
```

## Arguments

- network:

  Character. Lowercase code of the desired telemetry network. One of
  "otn", "act", "atap", "itag", "migramar", "nep", "npact", "pirat", or
  "otn_devel" (the OTN development server). Note that "etn", "fact",
  "glatos", "path" and "raft" are accepted, but only to produce an error
  and redirect you.

- set:

  Set the network in the current session?
