# Search OTN database by project code.

Search OTN database by project code.

## Usage

``` r
otn_search_code(code, server = "otn")
```

## Arguments

- code:

  Character. Project code for which to search. Supports partial
  matching.

- server:

  Character. Lowercase network code of the desired server. One of "otn",
  "act", "npact", or "devel" (the OTN development server). Note that
  "etn", "fact", and "glatos" are accepted, but only to produce an error
  and redirect you.
