######################################################################
## Filename: scrape-ggi.r
## Description:
## Author: Helge Liebert
## Created: Do Dez  6 13:51:53 2018
## Last-Updated: Mi Jan  9 14:04:51 2019
######################################################################

## load library
library(rvest)

## initiate session on site
site <- "http://nap.psa.gov.ph/ggi/default.asp"
session <- html_session(site)

## look at nodes
html_nodes(session, "form")
html_nodes(session, "option")

## get the drop-down options
options <- html_nodes(session, "select[name='strMunicipality2'] > option")
municipalities <- data.frame(
    value  = html_attr(options, "value"),
    option = html_text(options))
head(municipalities)
dim(municipalities)

## empty data frame to be filled
data <- data.frame(matrix(nrow = 0, ncol = 5))

## get ggi data for all municipalities in the list
for (op in municipalities$value[1:2]) {

    ## display some information
    print(op)
    print(paste0(which(op == municipalities$value), "/",
                 nrow(municipalities), " ", op))

    ## set option and submit form
    form <- html_form(html_node(session, "#form2"))
    form <- set_values(form, "strMunicipality2" = op)
    newpage <- submit_form(session, form)

    ## get table and process it
    table <- html_table(html_node(newpage, "table"), fill = TRUE)
    table$municipality <- op
    table <- table[3:nrow(table), ]

    ## assert correct dimension
    stopifnot(ncol(table) == 6)
    ## print(table)

    ## append to data
    data <- rbind(data, table)
}

## build header and add to data frame
header <- c("indicator", "value2005", "rank2005", "value2008", "rank2008",
            "municipality ")
names(data) <- header

## look at data
dim(data)
head(data)
