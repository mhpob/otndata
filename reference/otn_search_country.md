# Search OTN database by project country.

Search OTN database by project country.

## Usage

``` r
otn_search_country(country, server = "otn")
```

## Arguments

- country:

  Character. Project country for which to search. DOES NOT support
  partial matching.

- server:

  Character. Lowercase network code of the desired server. One of "otn",
  "act", "npact", or "devel" (the OTN development server). Note that
  "etn", "fact", and "glatos" are accepted, but only to produce an error
  and redirect you.
