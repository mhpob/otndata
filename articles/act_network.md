# Access for ACT Network members

You’ve just received notifications that your ACT Network tag matches are
in. Hooray!! Let’s walk through how to interact with all of your data.

``` r

library(otndata)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(arrow)
#> 
#> Attaching package: 'arrow'
#> The following object is masked from 'package:utils':
#> 
#>     timestamp
```

First, we need to log in. If you have never entered your username and
password before, you’ll need to do so. Run the following:

``` r

otn_login("act")
#> ✔ Login successful!
```

This will just set your credentials for the current R session. If you
wish to use the same credentials after closing/reopening R, use
`otn_set_credentials`:

``` r

otn_set_credentials("act")
```

What you might first want to do is check out all of the data extract
files on offer.

``` r

otn_extract_files(project = "tailwinds")
#>                                            name description
#> 1   tailwinds_qualified_detections_2023.parquet            
#> 2       tailwinds_qualified_detections_2023.zip            
#> 3   tailwinds_qualified_detections_2024.parquet            
#> 4       tailwinds_qualified_detections_2024.zip            
#> 5 tailwinds_unqualified_detections_2023.parquet            
#> 6     tailwinds_unqualified_detections_2023.zip            
#> 7 tailwinds_unqualified_detections_2024.parquet            
#> 8     tailwinds_unqualified_detections_2024.zip            
#>                                                                                                                         url
#> 1   https://data.theactnetwork.com/data/repository/tailwinds/detection-extracts/tailwinds_qualified_detections_2023-parquet
#> 2       https://data.theactnetwork.com/data/repository/tailwinds/detection-extracts/tailwinds_qualified_detections_2023.zip
#> 3   https://data.theactnetwork.com/data/repository/tailwinds/detection-extracts/tailwinds_qualified_detections_2024-parquet
#> 4       https://data.theactnetwork.com/data/repository/tailwinds/detection-extracts/tailwinds_qualified_detections_2024.zip
#> 5 https://data.theactnetwork.com/data/repository/tailwinds/detection-extracts/tailwinds_unqualified_detections_2023-parquet
#> 6     https://data.theactnetwork.com/data/repository/tailwinds/detection-extracts/tailwinds_unqualified_detections_2023.zip
#> 7 https://data.theactnetwork.com/data/repository/tailwinds/detection-extracts/tailwinds_unqualified_detections_2024-parquet
#> 8     https://data.theactnetwork.com/data/repository/tailwinds/detection-extracts/tailwinds_unqualified_detections_2024.zip
#>               created            modified creator     size type
#> 1 2026-06-12 13:25:31 2026-06-12 13:25:31 krichie 133.0 KB File
#> 2 2026-06-12 13:25:49 2026-06-12 13:25:50 krichie  95.6 KB File
#> 3 2026-06-12 13:26:03 2026-06-12 13:26:03 krichie 174.2 KB File
#> 4 2026-06-12 13:26:19 2026-06-12 13:26:19 krichie 129.7 KB File
#> 5 2026-06-12 13:26:33 2026-06-12 13:26:33 krichie 602.3 KB File
#> 6 2026-06-12 13:26:45 2026-06-12 13:26:45 krichie 410.6 KB File
#> 7 2026-06-12 13:26:59 2026-06-12 13:26:59 krichie  62.7 KB File
#> 8 2026-06-12 13:27:14 2026-06-12 13:27:14 krichie  35.9 KB File
```

This can be a lot! We can just grab the files that have been modified
after a certain date – usually the date just before the most-recent data
push.

``` r

otn_extract_files(project = "tailwinds", since = "2026-06-12")
#>                                            name description
#> 1   tailwinds_qualified_detections_2023.parquet            
#> 2       tailwinds_qualified_detections_2023.zip            
#> 3   tailwinds_qualified_detections_2024.parquet            
#> 4       tailwinds_qualified_detections_2024.zip            
#> 5 tailwinds_unqualified_detections_2023.parquet            
#> 6     tailwinds_unqualified_detections_2023.zip            
#> 7 tailwinds_unqualified_detections_2024.parquet            
#> 8     tailwinds_unqualified_detections_2024.zip            
#>                                                                                                                         url
#> 1   https://data.theactnetwork.com/data/repository/tailwinds/detection-extracts/tailwinds_qualified_detections_2023-parquet
#> 2       https://data.theactnetwork.com/data/repository/tailwinds/detection-extracts/tailwinds_qualified_detections_2023.zip
#> 3   https://data.theactnetwork.com/data/repository/tailwinds/detection-extracts/tailwinds_qualified_detections_2024-parquet
#> 4       https://data.theactnetwork.com/data/repository/tailwinds/detection-extracts/tailwinds_qualified_detections_2024.zip
#> 5 https://data.theactnetwork.com/data/repository/tailwinds/detection-extracts/tailwinds_unqualified_detections_2023-parquet
#> 6     https://data.theactnetwork.com/data/repository/tailwinds/detection-extracts/tailwinds_unqualified_detections_2023.zip
#> 7 https://data.theactnetwork.com/data/repository/tailwinds/detection-extracts/tailwinds_unqualified_detections_2024-parquet
#> 8     https://data.theactnetwork.com/data/repository/tailwinds/detection-extracts/tailwinds_unqualified_detections_2024.zip
#>               created            modified creator     size type
#> 1 2026-06-12 13:25:31 2026-06-12 13:25:31 krichie 133.0 KB File
#> 2 2026-06-12 13:25:49 2026-06-12 13:25:50 krichie  95.6 KB File
#> 3 2026-06-12 13:26:03 2026-06-12 13:26:03 krichie 174.2 KB File
#> 4 2026-06-12 13:26:19 2026-06-12 13:26:19 krichie 129.7 KB File
#> 5 2026-06-12 13:26:33 2026-06-12 13:26:33 krichie 602.3 KB File
#> 6 2026-06-12 13:26:45 2026-06-12 13:26:45 krichie 410.6 KB File
#> 7 2026-06-12 13:26:59 2026-06-12 13:26:59 krichie  62.7 KB File
#> 8 2026-06-12 13:27:14 2026-06-12 13:27:14 krichie  35.9 KB File
```

The most important thing, however, is downloading those files! Let’s
grab the parquet files via `otn_extract_files`, download them with
`otn_download`, and create a mini-database for ourselves.

``` r

otn_extract_files(
  project = "tailwinds",
  since = "2026-06-12"
) |>
  dplyr::filter(grepl("_qualified.*parquet", name)) |>
  otn_download(outdir = "./my_awesome_detections")
#> ℹ Directory created: ./my_awesome_detections.
#> ℹ Files saved to ./my_awesome_detections/tailwinds_qualified_detections_2023.parquet and ./my_awesome_detections/tailwinds_qualified_detections_2024.parquet.
```

Create a database on the fly using the [arrow
package](https://arrow.apache.org/docs/r).

``` r

my_db <- arrow::open_dataset("my_awesome_detections/")
my_db
#> FileSystemDataset with 2 Parquet files
#> 22 columns
#> basisOfRecord: string
#> institutionCode: string
#> collectionCode: string
#> dateLastModified: timestamp[ms]
#> dateCollectedUTC: timestamp[ms]
#> uncorrectedDateCollectedUTC: timestamp[ms]
#> trackerCode: string
#> tagName: string
#> catalogNumber: string
#> station: string
#> receiverSerial: string
#> decimalLongitude: double
#> decimalLatitude: double
#> geodeticDatum: string
#> rcvrCatNumber: string
#> sensorType: string
#> sensorName: string
#> sensorRaw: double
#> geometry: binary
#> contactPI: string
#> ...
#> 2 more columns
#> Use `schema()` to see entire schema
#> 
#> See $metadata for additional Schema metadata
```

Now we can do some really quick summaries.

``` r

my_db |>
  dplyr::group_by(trackerCode) |>
  dplyr::tally() |>
  dplyr::arrange(-n) |>
  dplyr::collect()
#> # A tibble: 58 × 2
#>    trackerCode        n
#>    <chr>          <int>
#>  1 FACT.SCDNRGPD   1784
#>  2 ACT.LICABLE     1290
#>  3 ACT.MUFISH      1028
#>  4 ACT.SBUSOMAS01   974
#>  5 ACT.HMSWEA       757
#>  6 ACT.NCATS2021    569
#>  7 ACT.MSRPWS       507
#>  8 ACT.SBUINGRAM    394
#>  9 ACT.MUEMPIRE     281
#> 10 ACT.MUS4F        246
#> # ℹ 48 more rows
```

Or find out who we should email and say “hi!”.

``` r

my_db |>
  dplyr::distinct(contactPI) |>
  dplyr::mutate(contactPI = sub(" \\(.*", "", contactPI)) |>
  head() |>
  dplyr::collect()
#> # A tibble: 6 × 1
#>   contactPI       
#>   <chr>           
#> 1 Kara Dodge      
#> 2 Matthew Ogburn  
#> 3 Matthew Balazik 
#> 4 Jeff Kneebone   
#> 5 Michael Frisk   
#> 6 Bradley Peterson
```
