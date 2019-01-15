######################################################################
## Filename: scrape-ggi-csv.r
## Description:
## Author: Helge Liebert
## Created: Do Dez  6 13:51:53 2018
## Last-Updated: Mi Jan  9 19:16:53 2019
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

## build header and write to file
header <- c("indicator", "value2005", "rank2005", "value2008", "rank2008",
            "municipality")
write.table(t(header), "Data/ggi.csv", sep = ";",
            col.names = FALSE, row.names = FALSE)

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

    ## append to file
    write.table(table, "Data/ggi.csv", sep = ";", append = TRUE,
                col.names = FALSE, row.names = FALSE)
}
