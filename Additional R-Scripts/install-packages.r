# This will install all required dependencies for the course

# Packages to be installed from CRAN
install.packages(c("fBasics",
                   "corrplot",
                   "psych",
                   "glmnet",
                   "glmnetUtils",
                   "grf",
                   "rpart",
                   "rpart.plot",
                   "treeClust",
                   "randomForest",
                   "rlang",
                   "readr",
                   "devtools",
                   "tidyverse",
                   "reshape2",
                   "caret",
                   "neuralnet",
                   "plotmo",
                   "doParallel",
                   "RandomFieldsUtils",
                   "doSNOW",
                   "rms",
                   "Rtools",
                   "data.table",
                   "rvest",
                   "httr",
                   "jsonlite",
                   "xml2",
                   "tm",
                   "tidytext",
                   "topicmodels",
                   "wordcloud",
                   "SentimentAnalysis",
                   "naivebayes",
                   "slam",
                   "lexicon",
                   "gmodels",
                   "qdap",
                   "cld2",
                   "cld3",
                   "RColorBrewer",
                   "IRkernel",
                   "IRdisplay",
                   "repr"),
                   dependencies = TRUE)

# Sets up the R kernel for Jupyter Notebooks
IRkernel::installspec()

# Packages to be installed from Github
devtools::install_github("MCKnaus/dmlmt")
