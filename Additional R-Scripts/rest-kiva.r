######################################################################
## Filename: rest-kiva.r
## Description:
## Author: Helge Liebert
## Created: Do Jan  4 19:13:53 2018
## Last-Updated: Mi Jan  9 19:15:12 2019
######################################################################

## Libraries
library(jsonlite)
library(httr)
library(rvest)

## Kiva Examples

## Get the 20 most recent loans
newloans <- fromJSON("https://api.kivaws.org/v1/loans/newest.json")

## All lenders for a particular loan id
loans <- fromJSON("http://api.kivaws.org/v1/loans/38239/lenders.json")
str(loans)
toJSON(loans, pretty = TRUE)

## Simplify structure
loans <- fromJSON("http://api.kivaws.org/v1/loans/38239/lenders.json",
                  flatten = TRUE)
str(loans)
toJSON(loans, pretty = TRUE)
loans <- as.data.frame(loans)
str(loans)
toJSON(loans, pretty = TRUE)


## List of all API methods
methods <- fromJSON("https://api.kivaws.org/v1/methods.json", flatten = TRUE)
str(methods)
methods <- as.data.frame(methods$methods)
methods[grep("search", methods$id), ]



## Request specific info from KIVA API

## Examples
## https://build.kiva.org/api
## https://build.kiva.org/docs/getting_started
## https://api.kivaws.org/v1/loans/search.html?sector=Agriculture
## https://api.kivaws.org/v1/loans/search.json?sector=Agriculture&country=VN

## Parameters
baseurl <- "https://api.kivaws.org/v1/"
method <- "loans/search.json?"
## method <- "loans/search.xml?"
## method <- "loans/search.html?"
country <- "VN,KH"
sector <- "Agriculture"
type <- "individuals"
status <- "funded"
sortby <- "newest"

## Construct URL
query <- paste0("country_code=", country, "&",
                "sector=", sector, "&",
                "borrower_type=", type, "&",
                "status=", status, "&",
                "sort_by=", sortby)

uri <- paste0(baseurl, method, query )
uri

## Send HTTP GET request, handle response content, library(httr)
response <- GET(uri)
response
if (response$status_code == 200) {
    jsontable <- content(response, as = "text")
} else {
    stop("HTTP response not OK!")
}
jsontable

## Parse json data
data <- fromJSON(jsontable, flatten = TRUE)
str(data)
names(data)
data$paging
data <- data$loans
head(data)
dim(data)

## Even more simple, pass URI directly
data <- fromJSON(uri, flatten = TRUE)
data <- data$loans
str(data)
head(data)
dim(data)

## Nested elements need to be flattened
## data$tags <- sapply(data$tags, function(x) paste(unlist(x), collapse = ", "))
## data$themes <- sapply(data$themes, function(x) paste(unlist(x), collapse = ", "))
## data$description.languages <- sapply(data$description.languages, function(x) paste(unlist(x), collapse = ", "))
## str(data)



## Get all data, multiple requests, iterate over pages

## Note: very simple proof of concept
## (should check http response for error and have better tests)
## (more efficient to large queries to file immediately)

## Parameters
baseurl <- "https://api.kivaws.org/v1/"
method  <- "loans/search.json?"
country <- "VN"
sector  <- "Agriculture"
type    <- "individuals"
status  <- "funded"
sortby  <- "oldest" # (o/w duplicates may occur when new entries are added)
pagelength <- 20 # max page length allowed is 500

## Construct URL
query <- paste0("country_code=", country, "&",
                "sector=", sector, "&",
                "borrower_type=", type, "&",
                "status=", status, "&",
                ## "per_page=", pagelength, "&"
                "sort_by=", sortby)
uri <- paste0(baseurl, method, query)

## Get maxpagenumber and other information for iteration
response <- fromJSON(uri, flatten = TRUE)
response$paging
maxpages <- response$paging$pages
records  <- response$paging$total
columns  <- ncol(response$loans)


## Open csv, write header
header <- names(response$loans)
write.table(t(header), file = "Data/kiva.csv", sep = ";",
            col.names = FALSE, row.names = FALSE)

# Or collect in data frame (don't do this for large jobs)
## data <- data.frame(matrix(nrow = 0, ncol = columns))
## names(data) <- header

## Simple helper function to flatten columns
unnest <- function(col) paste(unlist(col), collapse = ", ")


## Iterate over pages, limit to first three
for (p in seq(1, maxpages, by = 1)[1:3]) {

    ## Info
    print(paste0(p, "/", maxpages))

    ## Append page to uri
    pquery <- paste0(uri, "&page=", p)

    ## Get data, assert completeness
    loans <- fromJSON(pquery, flatten = TRUE)$loans
    stopifnot(nrow(loans) == pagelength)
    stopifnot(ncol(loans) == columns)

    ## Fix nested list columns ... or just use data.table::fwrite()
    ## str(loans)
    loans$tags <- sapply(loans$tags, unnest)
    ## loans$themes <- sapply(loans$themes, unnest) # missing for older records
    loans$description.languages <- sapply(loans$description.languages, unnest)
    ## str(loans)

    ## Collect loans in data frame
    ## data <- rbind(data, loans)

    ## Append to file
    write.table(loans, "Data/kiva.csv", sep = ";", append = TRUE,
                col.names = FALSE, row.names = FALSE)
}


## head(data)
## dim(data)



## TheyWorkForYou.com Example
apikey <- "G3WVqtBtKAbdGVqrd8BKajm8"
base <- "https://www.theyworkforyou.com/api/"
format <- "js"
func <- "getMPs?"
query <- paste0("&", "key=", apikey, "&", "output=", format)
uri <- paste0(base, func, query)
uri
## listofmps <- fromJSON(uri) # problem with encoding, maybe xml is better
response <- GET(uri)
response <- content(response, as = "raw")
listofmps <- fromJSON(rawToChar(response))
str(listofmps)
