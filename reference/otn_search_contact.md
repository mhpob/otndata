# Search OTN database by project contact.

Search OTN database by project contact.

## Usage

``` r
otn_search_contact(contact, server = "otn")
```

## Arguments

- contact:

  Character. Project contact for which to search. DOES NOT support
  partial matching.

- server:

  Character. Lowercase network code of the desired server. One of "otn",
  "act", "npact", or "devel" (the OTN development server). Note that
  "etn", "fact", and "glatos" are accepted, but only to produce an error
  and redirect you.
