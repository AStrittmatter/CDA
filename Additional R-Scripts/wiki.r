######################################################################
## Filename: wiki.r
## Author: Helge Liebert
## Created: Di Jan  8 18:23:16 2019
## Last updated:
## Description: illustrating basic web scraping using wikipedia
######################################################################


## install and load required library
## install.packages("rvest", dependencies = TRUE)
library(rvest)



###########################
## SCRAPING A WIKI TABLE ##
###########################

## 1) fetch and parse the website
page <- read_html("https://en.wikipedia.org/wiki/Infant_mortality")
## 2) extract the html node containing the table
table <- html_node(page, xpath = "//*[@id='mw-content-text']/div/table[2]")
## 3) extract the table as a data frame
mrates <- html_table(table)
mrates


################################################
## INVESTIGATING PAGE ELEMENTS AND NAVIGATION ##
################################################

## list table nodes
html_nodes(page, "table")
## using css or xpath selectors is equivalent
table <- html_node(page, css = "#mw-content-text > div > table:nth-child(121)")
table <- html_node(page, xpath = "//*[@id='mw-content-text']/div/table[2]")


## check out links in the table
html_nodes(table, "a") %>% html_attr("href")
tablelinks <- html_attr(html_nodes(table, "a"), "href")
link <- grep("Somalia", tablelinks, value = TRUE)
link

## looking at html elements and their attributes
html_nodes(page, "link")
html_nodes(page, "a")
html_nodes(page, "a") %>% html_attr("href")
html_nodes(table, "a") %>% html_attr("href")

## looking at links from link and anchor tags
html_nodes(page, "link") %>% html_attr("href")
html_nodes(page, "a") %>% html_attr("href")
html_attr(html_nodes(page, "a"), "href")


## follwing a link to another page
session <- html_session("https://en.wikipedia.org/wiki/Infant_mortality")
session <- follow_link(session, "Somalia")
page <- read_html(session)
table <- html_node(page, xpath = "//*[@id='mw-content-text']/div/table[4]")
regions <- html_table(table)
regions





######################
## REGEX FILTERING  ##
######################

## filtering links
page <- read_html("https://en.wikipedia.org/wiki/Infant_mortality")
wikilinks <- html_attr(html_nodes(page, "a"), "href")


## regex examples
links <- grep("^/wiki", wikilinks, value = TRUE)
links <- grep("^/wiki.*[0-9][0-9]$", wikilinks, value = TRUE)
links <- grep("^/wiki.*File:.*", wikilinks, value = TRUE)
links <- grep("^(?!.*:)/wiki/.*Mortality", wikilinks, value = TRUE, perl = TRUE)
links <- grep("^(?!.*:)(/wiki/.*Mortality)|(/wiki/.*Somalia)", wikilinks, value = TRUE, perl = TRUE)
links

# sometimes easier to do it in multiple steps for readability
links <- grep("^/wiki/", wikilinks, value = TRUE)
links <- grep("Mortality|Somalia", links, value = TRUE)
links <- grep(":", links, value = TRUE, invert = TRUE)
links

# select only internal links matching with mortality or somalia, no files or category pages
links <- grep("^(?!.*:)(/wiki/.*Mortality)|(/wiki/.*Somalia)", wikilinks,
              ignore.case = TRUE, value = TRUE, perl = TRUE)
links <- unique(links)
links

# navigate to linked page
session <- jump_to(session, links[1])
page <- read_html(session)
html_nodes(page, "title")

# navigate to linked page
session <- jump_to(session, links[5])
page <- read_html(session)
html_nodes(page, "title")
