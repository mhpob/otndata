# Internal function for searching OTN projects.

Internal function for searching OTN projects.

## Usage

``` r
.otn_search(network, type, search_term)
```

## Arguments

- network:

  Character. Lowercase code of the desired telemetry network. One of
  "otn", "act", "atap", "itag", "migramar", "nep", "npact", "pirat", or
  "otn_devel" (the OTN development server). Note that "etn", "fact",
  "glatos", "path" and "raft" are accepted, but only to produce an error
  and redirect you.

- type:

  Character. Internal data base to search.

- search_term:

  Character. Search term to use.
