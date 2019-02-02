######################################################################
## Filename: prep-kiva.r
## Description:
## Author: Helge Liebert
## Created: Fr Dez  7 15:01:01 2018
## Last-Updated: Sa Dez  8 00:26:19 2018
######################################################################

## install.packages("cld2")
## install.packages("cld3")

## library(dplyr)
## library(tidyverse)
## library(tidytext)
## library(tm)
## library(jsonlite)

library(data.table)
## library(cld2)
## library(cld3)
library(ggplot2)


## loans <- fromJSON("../Data/loans.json")
loans <- fread("../../Data/loans.csv")
## loans <- fread("../../Data/loans.csv", nrows = 100000)
names(loans) <- tolower(names(loans))
names(loans) <- gsub("_", "", names(loans))
names(loans)
dim(loans)
str(loans)
## head(loans)

# format conversions
loans[, postedtime := as.POSIXct(postedtime,
                                 format = "%Y-%m-%d %H:%M:%OS %z", tz = "UTC")]
loans[, raisedtime := as.POSIXct(postedtime,
                                 format = "%Y-%m-%d %H:%M:%OS %z", tz = "UTC")]
loans[, disbursetime := as.POSIXct(disbursetime,
                                   format = "%Y-%m-%d %H:%M:%OS %z", tz = "UTC")]
loans[, plannedexpirationtime := as.POSIXct(plannedexpirationtime,
                                            format = "%Y-%m-%d %H:%M:%OS %z", tz = "UTC")]
## loans[, raiseduration := difftime(raisedtime, postedtime, units = "days")]
## qplot(loans$raiseduration, binwidth = 10)

## building a uniform english description
## loans <- loans[originallanguage == "English"]
loans <- loans[(originallanguage ==  "English") | (descriptiontranslated != ""), ]
loans[, desc := description]
loans[originallanguage != "English", desc := descriptiontranslated]
loans <- loans[desc != "", ]

## removing html tags and newline characters
## quick and dirty
## loans[sample(nrow(loans), 10), desc]
## head(loans[grep("<.*?>", desc), desc ], 10)
loans[, desc := gsub("(\\\\r\\\\n)+", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("(\\\\n)+", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("(\\\\t)+", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("(<br>)+|(</br>)|(<br/>)+|(<br />)+|(<bre>)+|(<br  />)+", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<p>|<p.*=.*>|</p>|<p/>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<i>|</i>|<i/>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<a>|<a.*=.*>|<a.*href.*>|</a>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<strong>|</strong>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<b>|</b>|<b/>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<li>|</li>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<em>|</em>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<font .*>|</font>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<span.*>|</span>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<size.*>|</size>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<div.*>|</div>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<h[0-5]>|</h[0-5]>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<u>|</u>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<ol>|</ol>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<o>|</o>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<ul>|</ul>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<cite>|</cite>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("</p<>|<p|p>|>p<|>b>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<>|</>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<blockquote>|</blockquote>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<be>|<\\}>|<\\{>|<\\[>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<http.*>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<s>|</s>|<d>|</d>", " ", desc, ignore.case = TRUE)]
loans[, desc := gsub("<.*i>", "", desc, ignore.case = TRUE)]
loans[, desc := gsub("<ital>|</ital>|<small>|</P:>", "", desc, ignore.case = TRUE)]
loans <- loans[!grep("</E", desc), ]
loans <- loans[!grep("</R", desc), ]

loans[, desc := gsub("^[*]+Note:.*[*]+", "", desc)]
loans[, desc := gsub("[*]+Note:.*$", "", desc, ignore.case = TRUE)]
loans[, desc := gsub(".Translated by.*$|.Translated from.*$", "", desc, ignore.case = TRUE)]
loans[, desc := gsub("Translated by.*$|Translated from.*$", "", desc, ignore.case = TRUE)]
loans[, desc := gsub("[*]{2} [*]{2} .*This text has been translated.*$", "", desc, ignore.case = TRUE)]
loans[, desc := gsub("[*]{2} This text has been translated.*$", "", desc, ignore.case = TRUE)]
loans[, desc := gsub("[*] This text has been translated.*$", "", desc, ignore.case = TRUE)]
loans[, desc := gsub("This text has been translated.*$", "", desc, ignore.case = TRUE)]
loans[, desc := gsub("Translated  from.*$", "", desc)]
loans[, desc := gsub("Translated and elaborated.*$", "", desc)]
loans[, desc := gsub("Translated.*Kiva fellow.*$", "", desc)]
loans[, desc := gsub("Translated.*Kiva volunteer.*$", "", desc)]
loans[, desc := gsub("<Espa.ol>.*$", "", desc, ignore.case = TRUE)]
loans <- loans[desc != "", ]

## whitespace
## loans[, desc := gsub("^\\s+|\\s+$", "", desc)]
loans[, desc := trimws(desc)]
loans[, desc := gsub("\\s+", " ", desc)]


## some more language detection
## based on chrome language detection library
## beware, cld3 takes way longer than cld2
## loans[, desclang := cld3::detect_language(desc)]
## loans[, table(desclang)]
## loans <- loans[desclang == "en", ]
## loans[, desclang := NULL]

loans[, desclang := cld2::detect_language(desc)]
loans[, table(desclang)]
loans <- loans[desclang == "en", ]
loans[, desclang := NULL]

## a few more restrictions
loans <- loans[loanamount <= 10000, ]
loans <- loans[distributionmodel == "field_partner", ]
loans <- loans[status != "fundRaising", ]
## loans <- loans[status == "funded", ]
## loans <- loans[originallanguage == "English"]
## loans[fundedamount != loanamount,  .N, ]

## have a look at the variables, then get rid of most of them
## loans[, description := NULL]
loans[, description := desc] # rename again for consistency with other script
loans[, desc := NULL]
loans[, descriptiontranslated := NULL]
loans[, imageid := NULL]
loans[, videoid := NULL]
loans[, loanname := NULL]
loans[, townname := NULL]
loans[, currency := NULL]
loans[, currencypolicy := NULL]
loans[, currencyexchangecoveragerate := NULL]
loans[, partnerid := NULL]
loans[, lenderterm := NULL]
loans[, numlenderstotal := NULL]
loans[, numjournalentries := NULL]
loans[, numbulkentries := NULL]
loans[, tags := NULL]
loans[, borrowernames := NULL]
loans[, borrowerpictured := NULL]
loans[, distributionmodel := NULL]
loans[, borrowergenders := NULL]
loans[, countrycode := NULL]
loans[, status := NULL, ]
loans[, postedtime := NULL, ]
loans[, raisedtime := NULL, ]
loans[, plannedexpirationtime := NULL, ]
loans[, disbursetime := NULL, ]
loans[, originallanguage := NULL, ]

## better manageable dimension
names(loans)
dim(loans)

## write to file
fwrite(loans, "Data/kiva.csv", sep = ";")

## smaller files for use in lecture
set.seed(100)
fwrite(loans[sample(nrow(loans), 10000), ], "Data/kiva-tiny.csv", sep = ";")
fwrite(loans[sample(nrow(loans), 100000), ], "Data/kiva-small.csv", sep = ";")
