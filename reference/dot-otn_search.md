# Internal function for searching OTN projects.

Internal function for searching OTN projects.

## Usage

``` r
.otn_search(server, type, search_term)
```

## Arguments

- server:

  Character. Lowercase network code of the desired server. One of "otn",
  "act", "npact", or "devel" (the OTN development server). Note that
  "etn", "fact", and "glatos" are accepted, but only to produce an error
  and redirect you.

- type:

  Character. Internal data base to search.

- search_term:

  Character. Search term to use.
