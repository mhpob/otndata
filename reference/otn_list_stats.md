# Retrieve OTN server statistics.

Retrieve OTN server statistics.

## Usage

``` r
otn_list_stats(server = "otn")
```

## Arguments

- server:

  Character. Lowercase network code of the desired server. One of "otn",
  "act", "npact", or "devel" (the OTN development server). Note that
  "etn", "fact", and "glatos" are accepted, but only to produce an error
  and redirect you.
