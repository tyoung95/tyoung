
# footyr

<!-- badges: start -->
<!-- badges: end -->

Welcome! `footyr` is a useful R package that works together with the ISport API to query and parse various types of football-related data.

### Installation

You can install the latest version of `footyr` from Github:

``` r
# My public Github
devtools::install_github("tyoung95/tyoung/footyr")
library(footyr)
```

### Required Packages

There are several required packages that need to be used as dependencies for `footyr`. These include:

* [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html)
* [httr](https://cran.r-project.org/web/packages/httr/index.html)
* [jsonlite](https://cran.r-project.org/web/packages/jsonlite/index.html)
* [magick](https://cran.r-project.org/web/packages/magick/index.html)
* [magrittr](https://cran.r-project.org/web/packages/magrittr/index.html)
* [stringr](https://cran.r-project.org/web/packages/stringr/index.html)
* [tibble](https://cran.r-project.org/web/packages/tibble/index.html)
* [tidyr](https://cran.r-project.org/web/packages/tidyr/index.html)
* utils


### ISport API

* This package is made for use with the [ISport API](https://www.isportsapi.com).
* Registration for the [ISport API key](https://www.isportsapi.com/auth/register)(both free trial and paid plans.)
* Useful [documentation](https://www.isportsapi.com/docs.html) for the ISport API.



### ISport API Key

Having registered online and received your API key, you need to then store your API key in your REnviron. This is important as you will not be able to perform calls to the API without the key, and as this key is unique to you, you would need to keep it secret, much like a password.

**Important**: You should store your API key under the name "ISPORT_KEY". This is crucial as this package relies on this.

``` r
#Use this to set your API key, replacing "YOUR_API_KEY" with your actual key.
Sys.setenv("ISPORT_KEY" = "YOUR_API_KEY")

#Check your key
Sys.getenv("ISPORT_KEY")
```


### Example

This is a basic example which shows you how to solve a common problem: In this case, displaying the league table. See `vignettes` for more examples.

``` r
library(footyr)
# Finding the league table for the English Premier League
myleague(leagueID = 1639, table = "yes")
```

