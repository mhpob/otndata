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

  Character. URL of the OTN-style Plone server. Defaults to the main OTN
  server at <https://members.oceantrack.org>.
