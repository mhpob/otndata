# Search OTN database by local node.

Search OTN database by local node.

## Usage

``` r
otn_search_node(node, server = "otn")
```

## Arguments

- node:

  Character. OTN node for which to search. DOES NOT support partial
  matching.

- server:

  Character. Lowercase network code of the desired server. One of "otn",
  "act", "npact", or "devel" (the OTN development server). Note that
  "etn", "fact", and "glatos" are accepted, but only to produce an error
  and redirect you.
