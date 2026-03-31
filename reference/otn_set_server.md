# Set the OTN Plone instance with which you wish to interact.

Set the OTN Plone instance with which you wish to interact.

## Usage

``` r
otn_set_server(server = c("otn", "npact", "act", "fact", "etn", "devel"))
```

## Arguments

- server:

  Character. Lowercase network code of the desired server. One of "otn",
  "npact", "devel" (the OTN development server). Note that "atn",
  "fact", and "etn" are accepted, but only to produce an error and
  redirect you.
