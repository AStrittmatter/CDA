######################################################################
## Filename: text-analysis.r
## Description:
## Author: Helge Liebert
## Created: Fr Dez  7 15:01:01 2018
## Last-Updated: Sa Dez  8 00:26:19 2018
######################################################################

## Libraries
library(tm)
library(data.table)
library(ggplot2)
library(tidytext)
library(dplyr)
library(topicmodels)
library(wordcloud)
library(SentimentAnalysis)
library(naivebayes)
library(slam)
library(glmnet)
library(lexicon)


## Simple helper function to view first copora elements, only for lecture
chead <- function(c) lapply(c[1:2], as.character)




## Read data
loans <- fread("Data/kiva-tiny.csv", encoding = "UTF-8")
names(loans)


## Set up corpus
setnames(loans, "loanid", "doc_id")
setnames(loans, "description", "text")
corp <- Corpus(DataframeSource(loans))

## Inspect it
corp
str(corp[[1]])
lapply(corp[1:2], as.character)


## Main corpus transformations, passed via tm_map()
## Other transformations have to be wrapped in content_transformer()
getTransformations()

## All chars to lower case
corp <- tm_map(corp, content_transformer(tolower))
## chead(corp)

## Remove punctuation
corp <- tm_map(corp, removePunctuation)
## chead(corp)
corp <- tm_map(corp, removePunctuation, ucp = TRUE)
## chead(corp)

## Remove numbers
corp <- tm_map(corp, removeNumbers)
## chead(corp)


## For specific transformations, you could also pass a lambda function to remove
## patterns based on a regex

## Example:
## toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))
## corp <- tm_map(corp, toSpace, "patternhere")


## Look at the most frequent words in our text and see whether we should get rid of some
frequent_terms <- qdap::freq_terms(corp, 30)
plot(frequent_terms)

## More invasive changes: remove generic and custom stopwords
corp <- tm_map(corp, removeWords, stopwords('english'))
## chead(corp)

## And a few more words we filter for lack of being informative
corp <- tm_map(corp, removeWords, "loan")
corp <- tm_map(corp, removeWords, "kiva")

## There are a lot of names in the data, these are not really informative
## We apply a dictionary to get rid of some of them
corp <- tm_map(corp, removeWords, common_names[1:floor(length(common_names)/2)])
corp <- tm_map(corp, removeWords, common_names[floor(length(common_names)/2):length(common_names)])
corp <- tm_map(corp, removeWords, freq_first_names[1:floor(nrow(freq_first_names)/2), Name])
corp <- tm_map(corp, removeWords, freq_first_names[floor(nrow(freq_first_names)/2):nrow(freq_first_names), Name])
## corp <- tm_map(corp, removeWords, freq_last_names) # needs to be truncated as well, even longer
## chead(corp)



## Stem document
## corp <- tm_map(corp, stemDocument, language = 'english')
## chead(corp)

## Strip extra whitespace
corp <- tm_map(corp, stripWhitespace)
## chead(corp)

## Build a document-term or term-document matrix
## Default is term-frequency weighting (document length normalized count)
## dtm <- TermDocumentMatrix(corp)
dtm <- DocumentTermMatrix(corp)

## Inspect the document-term matrix
inspect(dtm)


## Alternatively, do it directly
## dtm <- DocumentTermMatrix(Corpus(DataframeSource(loans)),
##                           control = list(weighting = weightTfIdf,
##                                          language = "english",
##                                          tolower = TRUE,
##                                          removePunctuation = TRUE,
##                                          removeNumbers = TRUE,
##                                          stopwords = TRUE,
##                                          stemming = FALSE,
##                                          wordLengths = c(3, Inf)))
## inspect(dtm)


## Inspect most popular words
findFreqTerms(dtm, lowfreq=1000)

## Restrictions by terms also possible like this
## freqterms <- findFreqTerms(dtm, 5)
## dtm <- DocumentTermMatrix(corp, control=list(dictionary = freqterms))

## Inspect associations
## findAssocs(dtm, 'hard', 0.1)


## Remove sparse terms
## Improves tractability and saves time, my laptop cannot handle large matrices
## Tweak the sparse parameter to influence # of words
inspect(dtms)
dtms <- removeSparseTerms(dtm, sparse=0.90)
dim(dtms)
dtms <- dtms[row_sums(dtms) > 0, ]
dim(dtms)


## Alternatively, filter words by mean tf-idf
## Calculate average term-specific tf-idf weights as
## mean(word count/document length) * log(ndocs/ndocs containing word)
termtfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *
             log2(nDocs(dtm)/col_sums(dtm > 0))
summary(termtfidf)

## Only include terms with above mean tf-idf score
dtmw <- dtm[, (termtfidf >= 0.15)]
dim(dtmw)
## And documents within which these terms occur - this may induce selection
dtmw <- dtmw[row_sums(dtmw) > 0, ]
dim(dtmw)



## Simple visualization
wordcloud(corp, max.words = 100, random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))

## Counts from dtms
counts <- sort(colSums(as.matrix(dtms)), decreasing = TRUE)
counts <- data.frame(word = names(counts), freq = counts)
wordcloud(words = counts$word, freq = counts$freq,
          max.words = 100, random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))

## Counts from dtmw
counts <- sort(colSums(as.matrix(dtmw)), decreasing = TRUE)
counts <- data.frame(word = names(counts), freq = counts)
wordcloud(words = counts$word, freq = counts$freq,
          max.words = 100, random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))



## Dictionary method: Sentiment analysis using dictionaries
sentiment <- analyzeSentiment(dtms, language = "english")
sentiment <- convertToDirection(sentiment$SentimentGI)
## Potentially add back to original data for further analysis
## loans$sentiment <- sentiment
table(sentiment)




## Unsupervised method: Topic model
## lda <- LDA(dtms, k = 2, control = list(seed = 100))
## lda <- LDA(dtms, k = 10, control = list(seed = 100))
lda <- LDA(dtmw, k = 2, control = list(seed = 100))
## lda <- LDA(dtmw, k = 10, control = list(seed = 100))
## str(lda)

## Most likely topic for each document, could merge this to original data
## topic <- topics(lda, 1)
## Five most frequent terms for each topic
terms(lda, 5)

## Plot most frequent terms and associated probabilities by topic
tpm <- tidy(lda, matrix = "beta")
topterms <-
    tpm %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
topterms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()


## not working well: use loanuse text for classifier. filter by tfidf
loanuse <- loans[, .(doc_id, loanuse)]
setnames(loanuse, "loanuse", "text")
dtmuse <- DocumentTermMatrix(Corpus(DataframeSource(loanuse)),
                             control = list(weighting = weightTf,
                                            language = "english",
                                            tolower = TRUE,
                                            removePunctuation = TRUE,
                                            removeNumbers = TRUE,
                                            stopwords = TRUE,
                                            stemming = FALSE,
                                            wordLengths = c(3, Inf)))
inspect(dtmuse)

termtfidf <- tapply(dtmuse$v/row_sums(dtmuse)[dtmuse$i], dtmuse$j, mean) *
    log2(nDocs(dtmuse)/col_sums(dtmuse > 0))
summary(termtfidf)

## Filter by tf-idf
## dim(dtmuse)
dtmuse <- dtmuse[, (termtfidf >= 1.70)]
dtmuse <- dtmuse[row_sums(dtmuse) > 0, ]
## dim(dtmuse)

## Unsupervised method: Topic model
lda <- LDA(dtmuse, k = 6, control = list(seed = 100))
## str(lda)

## Most likely topic for each document, could merge this to original data
topic <- topics(lda, 1)
## Five most frequent terms for each topic
terms(lda, 10)

## Plot most frequent terms and associated probabilities by topic
tpm <- tidy(lda, matrix = "beta")
topterms <-
    tpm %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
topterms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()






## Supervised methods: Prep data
## Convert the sparse term-document matrix to a standard data frame
bag <- as.data.frame(as.matrix(dtms))

## Convert token counts to simple binary indicators
bag <- as.data.frame(sapply(bag, function(x) as.numeric(x > 0)))
bag$doc_id <- rownames(as.matrix(dtms))

## Add outcomes from the original data: Predict agricultural sector
loans$agsector <- as.numeric(loans$sectorname=="Agriculture")
bag <- merge(bag, loans[, .(agsector, loanamount, doc_id)], by = "doc_id")
table(bag$agsector)


## Partition data in test and training sample
set.seed(100)
testids <- sample(floor(nrow(bag)/3))
xtrain <- as.matrix(bag[-testids, !(names(bag) %in% c("agsector", "loanamount", "doc_id"))])
ytrain <- as.factor(bag[-testids,  "agsector"])
xtest  <- as.matrix(bag[ testids, !(names(bag) %in% c("agsector", "loanamount", "doc_id"))])
ytest  <- as.factor(bag[ testids,  "agsector"])




## Supervised generative model: Naive Bayes
nbclassifier <- naive_bayes(xtrain, ytrain)
## nbclassifier <- naive_bayes(xtrain, ytrain, laplace = 1)
nbpred <- predict(nbclassifier, xtest)

## Performance statistics: Classification rate
1-mean(as.numeric(nbpred != ytest))
## Performance statistics: Confusion matrix
## table(nbpred, ytest)a
caret::confusionMatrix(nbpred, ytest)
## gmodels::CrossTable(nbpred, ytest,
##                     prop.chisq = FALSE, chisq = FALSE, prop.t = FALSE,
##                     dnn = c("Predicted", "Actual"))




## Supervised text regression: L1 penalized logistic regression
l1classifier <- cv.glmnet(xtrain, ytrain, alpha = 1, family = "binomial")
l1pred <- as.factor(predict(l1classifier, xtest, s = "lambda.min", type = "class"))

## Performance statistics: Classification rate
1-mean(as.numeric(l1pred != ytest))
## Performance statistics: Confusion matrix
caret::confusionMatrix(l1pred, ytest)

## L1 logistic classifier using rare feature upweighting
sdweights <- apply(xtrain, 2, sd)
l1classifier <- cv.glmnet(xtrain, ytrain, alpha = 1, family = "binomial",
                          standardize = FALSE, penalty.factor  = sdweights)
l1pred <- as.factor(predict(l1classifier, xtest, s = "lambda.min", type = "class",
                            penalty.factor  = sdweights))

## Performance statistics: Classification rate
1-mean(as.numeric(l1pred != ytest))
## Performance statistics: Confusion matrix
caret::confusionMatrix(l1pred, ytest)




## Further example: Predict Loan Amount
## Supervised text regression: L1 penalized linear regression

## Rebuild outcome vectors
ytrain <- as.matrix(bag[-testids,  "loanamount"])
ytest  <- as.matrix(bag[ testids,  "loanamount"])

## Estimate and predict
l1predictor <- cv.glmnet(xtrain, ytrain, alpha = 1, family = "gaussian")
l1pred <- predict(l1predictor, xtest, s = "lambda.min", type = "response")

## RMSE
sqrt(mean((l1pred - ytest)^2))
caret::postResample(l1pred, ytest)
