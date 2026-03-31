
<!-- README.md is generated from README.Rmd. Please edit that file -->

# otndata

<!-- badges: start -->

<!-- badges: end -->

`otndata` is a wrapper around the Ocean Tracking Network’s Plone content
management system setup.

## Installation

You can install the development version of otndata from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("mhpob/otndata")
```

## Example

There are a few things that can be accessed without logging in. Mainly
all of the information presented via the leaflet map that can be seen on
<https://members.oceantrack.org>.

``` r
library(otndata)

otn_list_species() |>
  head()
#>           scientificname           commonname
#> 1          Abramis brama         common bream
#> 2 Acanthocybium solandri                wahoo
#> 3    Acanthurus bahianus       cirujano pardo
#> 4     Acanthurus blochii ringtail surgeonfish
#> 5   Acanthurus chirurgus           doctorfish
#> 6   Acanthurus coeruleus            blue tang

otn_list_projects() |>
  head()
#>   node   collectioncode        country longitude latitude
#> 1  ETN    Usk:2021-2024 UNITED KINGDOM    -4.035   52.405
#> 2  ETN          V2LTROT         NORWAY    10.945   63.605
#> 3  ETN OTN-Tosenfjorden         NORWAY     6.140   68.715
#> 4  ETN          ST08SWE         SWEDEN    10.960   58.465
#> 5  OTN              RNP         NORWAY    10.385   63.420
#> 6  OTN           V2LBFC MEXICO, BELIZE   -87.650   18.625
#>                                             shortname
#> 1                River Usk receiver network 2021-2024
#> 2             OTN-Loan_Salmonids in Trondheimsfjorden
#> 3                   OTN-Tosenfjorden,OTN-Tosenfjorden
#> 4 sea trout_2018-9_gullmarn,sea trout_2018-9_gullmarn
#> 5                          NTNU River Nidelva Project
#> 6                                  Big Fish Caribbean
#>                                                                                         longname
#> 1                                                           River Usk receiver network 2021-2024
#> 2                                                        OTN_Loan_Salmonids in Trondheimsfjorden
#> 3   Marine migrations and area use of brown trout in Tosenfjorden, Bindal municipality 2015-2017
#> 4                        Sea trout <i>Salmo trutta</i> connectivity in Gullmarn fjord, Skagerrak
#> 5                   Migration of anadromous brown trout between river and fjord, Nidelva, Norway
#> 6 Big Fish: Cooperative monitoring network for fish spawning aggregations in the wider Caribbean
#>         ocean                                              website             datacenter_infourl   id
#> 1 NE ATLANTIC http://www.lifewatch.be/en/imis?dasid=6491&show=json   http://www.lifewatch.be/etn/ <NA>
#> 2 NE ATLANTIC http://www.lifewatch.be/en/imis?dasid=8311&show=json   http://www.lifewatch.be/etn/ <NA>
#> 3 NE ATLANTIC http://www.lifewatch.be/en/imis?dasid=6201&show=json   http://www.lifewatch.be/etn/ <NA>
#> 4 NE ATLANTIC http://www.lifewatch.be/en/imis?dasid=8137&show=json   http://www.lifewatch.be/etn/ <NA>
#> 5 NE ATLANTIC                                                 <NA> https://members.oceantrack.org <NA>
#> 6 NW ATLANTIC                        http://geo.gcoos.org/restore/ https://members.oceantrack.org <NA>

otn_list_stats()
#> $project_count
#> [1] 1601
#> 
#> $contributor_count
#> [1] 2435
#> 
#> $inst_count
#> [1] 467
#> 
#> $species_count
#> [1] 457
#> 
#> $rcvr_count
#> [1] 2973
```

You can also query projects according to code, country of origin,
species, institution, node, or point of contact.

``` r
otn_search_node("ACT") |>
  head()
#>   node collectioncode country longitude latitude                                   shortname
#> 1  ACT        NJDEPMR     USA  -73.8185  38.0705                   NJDEP Mullica River Array
#> 2  ACT         CTSEC6     USA  -73.8150  38.0700 CT DEEP Sturgeon movements in CT, 2010-2017
#> 3  ACT          CT001     USA  -72.2500  41.6450        CT DEEP LIS array (Sec. 6 2018-2022)
#> 4  ACT         RIWFCC     USA  -73.8185  38.0705  Narragansett Bay Cable Corridor Monitoring
#> 5  ACT          CBASR     USA  -73.8150  38.0700                SERC Atlantic Stingray Study
#> 6  ACT        TNCCVOW     USA  -73.8185  38.0705          TNC / NEFSC CVOW tagging and array
#>                                                                                                                    longname
#> 1                                                                                                 NJDEP Mullica River Array
#> 2 Identification of summering and foraging habitats of Atlantic Sturgeon in Connecticut waters and within Long Island Sound
#> 3                             CT DEEP array of VEMCO receivers in Long Island Sound and lower Connecticut River, 2018-2023.
#> 4                                                                      Narragansett Bay Wind Farm Cable Corridor Monitoring
#> 5                                                                               Atlantic Stingray Habitat Use and Migration
#> 6                                                                                        TNC / NEFSC CVOW tagging and array
#>         ocean website            datacenter_infourl
#> 1 NW ATLANTIC    <NA> https://matos.asascience.com/
#> 2 NW ATLANTIC    <NA> https://matos.asascience.com/
#> 3 NW ATLANTIC    <NA> https://matos.asascience.com/
#> 4 NW ATLANTIC    <NA> https://matos.asascience.com/
#> 5 NW ATLANTIC    <NA> https://matos.asascience.com/
#> 6 NW ATLANTIC    <NA> https://matos.asascience.com/

otn_search_code("tail")
#>   node collectioncode country longitude latitude       shortname
#> 1  ACT      TAILWINDS     USA  -73.8185  38.0705 UMCES TailWinds
#>                                                                                longname       ocean
#> 1 TailWinds: Team for Assessing Impacts to Living resources from offshore WIND turbineS NW ATLANTIC
#>                        website            datacenter_infourl
#> 1 https://tailwinds.umces.edu/ https://matos.asascience.com/

otn_search_contact("Mike O'Brien")
#>   node collectioncode country longitude latitude                          shortname
#> 1  ACT         CBBBMB     USA  -73.8150  38.0700 UMCES Chesapeake Backbone, Mid-Bay
#> 2  ACT      TAILWINDS     USA  -73.8185  38.0705                    UMCES TailWinds
#> 3  ACT         MAMBON     USA  -73.8185  38.0705                  Mid-Atlantic MBON
#> 4  ACT       NAVYKENN     USA  -69.7800  43.7750   Navy Kennebec ME Telemetry Array
#>                                                                                         longname
#> 1                            Building a Mainstem Chesapeake Bay Telemetry Array: Mid-Bay Segment
#> 2          TailWinds: Team for Assessing Impacts to Living resources from offshore WIND turbineS
#> 3                Mid-Atlantic MBON: Dynamic Biodiversity and Telemetry Data for a Changing Coast
#> 4 Naval Undersea Warfare Center (NUWC) Kennebec River and Offshore Acoustic Telemetry Monitoring
#>         ocean                                          website            datacenter_infourl
#> 1 NW ATLANTIC                                             <NA> https://matos.asascience.com/
#> 2 NW ATLANTIC                     https://tailwinds.umces.edu/ https://matos.asascience.com/
#> 3 NW ATLANTIC https://marinebon.org/us-mbon/mid-atlantic-mbon/ https://matos.asascience.com/
#> 4 NW ATLANTIC                                             <NA> https://matos.asascience.com/
```

You’ll likely need to log in to access other parts of the CMS. You can
set your username and password for your system using the
`otn_set_credentials` helper function.

``` r
otn_set_credentials()
```

After that, you can just log in using `otn_login`. This package is meant
to interface with any node’s Plone instance. You can switch between them
using the `server` argument.

``` r
otn_login(server = 'devel')
#> ✔ Login successful!
```

List your project’s files:

``` r
otn_project_files(project = 'nsbs', server = 'devel', batch_size = 5)
#>                                           name  description
#> 1        Copy of otn_tagging_metadata_2019.xls Manu tagging
#> 2 NSBS_otn_metadata_tagging_2019 Reel Deal.xls             
#> 3                  Tagging Metadata - Original             
#> 4        Tagging Metadata - Quality Controlled             
#> 5        Satellite Tagging Metadata - Original             
#>                                                                                                                             url
#> 1        https://members.devel.oceantrack.org/data/repository/nsbs/data-and-metadata/2019/copy-of-otn_tagging_metadata_2019.xls
#> 2 https://members.devel.oceantrack.org/data/repository/nsbs/data-and-metadata/2019/nsbs_otn_metadata_tagging_2019-reel-deal.xls
#> 3                    https://members.devel.oceantrack.org/data/repository/nsbs/data-and-metadata/2014/tagging-metadata-original
#> 4                          https://members.devel.oceantrack.org/data/repository/nsbs/data-and-metadata/2014/tagging-metadata-qc
#> 5          https://members.devel.oceantrack.org/data/repository/nsbs/data-and-metadata/2014/satellite-tagging-metadata-original
#>               created            modified creator     size type
#> 1 2020-01-27 16:31:22 2020-01-27 16:31:23   cbate 714.0 KB File
#> 2 2019-09-09 17:33:09 2019-09-16 18:13:28  ntress 794.5 KB File
#> 3 2016-11-07 20:16:10 2014-10-07 17:54:57 badavis 725.0 KB File
#> 4 2016-11-07 20:16:15 2014-10-09 14:18:29 badavis 721.5 KB File
#> 5 2016-11-07 20:16:20 2014-10-08 18:11:06 badavis 713.5 KB File

otn_extract_files(project = 'nsbs', server = 'devel', batch_size = 5)
#>                      name
#> 1 Matched to Animals 2018
#> 2 Matched to Animals 2019
#> 3 Matched to Animals 2015
#> 4 Matched to Animals 2013
#> 5 Matched to Animals 2014
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               description
#> 1                                                                                                                                                                                                                                                                                                                                                                                                                                                            These detections have been mapped to animals tagged by your project and detected by any receiver projects, including your own. This file includes the initial release of the tagged animals.
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                                            These detections have been mapped to animals tagged by your project and detected by any receiver projects, including your own. This file includes the initial release of the tagged animals.
#> 3 For Trackers: All detections of all tags released by the project no matter where they occurred. Detections classified as false but with a transmitter belonging to this project will be included in the future. All single detections are considered false. There may be detections of some of your tag ids which have not been matched. There can be many reasons for this. To check if any of your tags may have been missed please see the mystery tag list for your region or series. For Deployment Operators: Sets of sentinel tag detections, sets of detections mapped to animals without the animal details, sets of 'UNQUALIFIED' detections.
#> 4                                                                                                                                                                                                                                                                                                                                                                                                                                                            These detections have been mapped to animals tagged by your project and detected by any receiver projects, including your own. This file includes the initial release of the tagged animals.
#> 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
#>                                                                                                             url
#> 1 https://members.devel.oceantrack.org/data/repository/nsbs/detection-extracts/nsbs_matched_detections_2018.zip
#> 2 https://members.devel.oceantrack.org/data/repository/nsbs/detection-extracts/nsbs_matched_detections_2019.zip
#> 3 https://members.devel.oceantrack.org/data/repository/nsbs/detection-extracts/nsbs_matched_detections_2015.zip
#> 4 https://members.devel.oceantrack.org/data/repository/nsbs/detection-extracts/nsbs_matched_detections_2013.zip
#> 5 https://members.devel.oceantrack.org/data/repository/nsbs/detection-extracts/nsbs_matched_detections_2014.zip
#>               created            modified    creator     size type
#> 1 2018-10-10 18:57:31 2020-10-23 18:27:39     otnbot  42.5 KB File
#> 2 2019-11-04 18:57:48 2020-10-23 18:27:39     otnbot  34.0 KB File
#> 3 2016-11-07 20:17:57 2017-04-10 14:54:25 fwhoriskey 170.0 KB File
#> 4 2016-12-21 15:42:11 2020-10-23 18:27:38     otnbot 141.0 KB File
#> 5 2016-12-21 15:42:15 2016-12-13 17:56:30 fwhoriskey 353.5 KB File
```

Or, just grab the ones modified more recently using the `since`
argument:

``` r
otn_project_files(
  project = 'nsbs',
  since = "2020-01-01",
  server = 'devel',
  batch_size = 5
)
#>                            name description
#> title NSBS_project_metadata.txt            
#>                                                                                                         url
#> title https://members.devel.oceantrack.org/data/repository/nsbs/data-and-metadata/nsbs_project_metadata.txt
#>                   created            modified creator   size type
#> title 2020-06-04 19:00:12 2020-06-04 19:00:15  ntress 2.6 KB File

otn_extract_files(
  project = 'nsbs',
  since = "2020-01-01",
  server = 'devel',
  batch_size = 5
)
#>                      name
#> 1 Matched to Animals 2018
#> 2 Matched to Animals 2019
#> 3 Matched to Animals 2013
#> 4 Matched to Animals 2016
#> 5 Matched to Animals 2017
#>                                                                                                                                                                                    description
#> 1 These detections have been mapped to animals tagged by your project and detected by any receiver projects, including your own. This file includes the initial release of the tagged animals.
#> 2 These detections have been mapped to animals tagged by your project and detected by any receiver projects, including your own. This file includes the initial release of the tagged animals.
#> 3 These detections have been mapped to animals tagged by your project and detected by any receiver projects, including your own. This file includes the initial release of the tagged animals.
#> 4 These detections have been mapped to animals tagged by your project and detected by any receiver projects, including your own. This file includes the initial release of the tagged animals.
#> 5 These detections have been mapped to animals tagged by your project and detected by any receiver projects, including your own. This file includes the initial release of the tagged animals.
#>                                                                                                             url
#> 1 https://members.devel.oceantrack.org/data/repository/nsbs/detection-extracts/nsbs_matched_detections_2018.zip
#> 2 https://members.devel.oceantrack.org/data/repository/nsbs/detection-extracts/nsbs_matched_detections_2019.zip
#> 3 https://members.devel.oceantrack.org/data/repository/nsbs/detection-extracts/nsbs_matched_detections_2013.zip
#> 4 https://members.devel.oceantrack.org/data/repository/nsbs/detection-extracts/nsbs_matched_detections_2016.zip
#> 5 https://members.devel.oceantrack.org/data/repository/nsbs/detection-extracts/nsbs_matched_detections_2017.zip
#>               created            modified creator     size type
#> 1 2018-10-10 18:57:31 2020-10-23 18:27:39  otnbot  42.5 KB File
#> 2 2019-11-04 18:57:48 2020-10-23 18:27:39  otnbot  34.0 KB File
#> 3 2016-12-21 15:42:11 2020-10-23 18:27:38  otnbot 141.0 KB File
#> 4 2016-12-21 15:42:19 2020-08-18 17:38:32  otnbot  42.0 KB File
#> 5 2017-05-05 18:21:55 2020-10-23 18:27:39  otnbot  74.4 KB File
```

You can download a file via its URL. This is a BIG work in progress and
the API will likely change multiple times in the coming weeks.

``` r
otn_get_file(
  "https://members.devel.oceantrack.org/data/repository/nsbs/detection-extracts/nsbs_matched_detections_2017.zip"
)
```
