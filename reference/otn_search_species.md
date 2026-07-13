# Search OTN database by project species

Search OTN database by project species

## Usage

``` r
otn_search_species(species, server = "otn")
```

## Arguments

- species:

  Character. Project species for which to search. DOES NOT support
  partial matching and only recognizes scientific names of the form
  "Genus species" (note capitalization).

- server:

  Character. Lowercase network code of the desired server. One of "otn",
  "act", "npact", or "devel" (the OTN development server). Note that
  "etn", "fact", and "glatos" are accepted, but only to produce an error
  and redirect you.
