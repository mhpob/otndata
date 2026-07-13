# Search OTN database by institution.

Search OTN database by institution.

## Usage

``` r
otn_search_institution(institution, server = "otn")
```

## Arguments

- institution:

  Character. Institution for which to search. DOES NOT support partial
  matching.

- server:

  Character. Lowercase network code of the desired server. One of "otn",
  "act", "npact", or "devel" (the OTN development server). Note that
  "etn", "fact", and "glatos" are accepted, but only to produce an error
  and redirect you.
