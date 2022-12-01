# PRACTICE TOPIC MODELING IN R (EXAMPLE TAKEN FROM CHAPTER 6 AND 2 OF TEXT MINING WITH R)


### 1. LOAD LIBRARIES 

library(tidyverse)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(dplyr)

## if the topicmodels package is not present, install it first:
## install.packages("topicmodels")



### 2.LOAD AND PROCESS DATA 

# the AssociatedPress dataset consists of 2246 news articles and 10473 distinct words
# comes with the topicmodels package

# load data
data("AssociatedPress")
AssociatedPress

# we need a document-term matrix to run LDA, and these data are already in the right shape
# but what if we had these data in a tidy-text format instead?
# see https://www.tidytextmining.com/dtm.html

# tidy()
# data in tidy text format: one-token-per-document-per-row
# useful for basic exploratory analyses (see Chapter 1 and 2)
ap_td <- tidy(AssociatedPress)
ap_td

# cast_dtm()
# re-shape the data back in document-term matrix
ap_td %>%
  cast_dtm(document, term, count)



### 3. RUN LDA 

# the main function to use is LDA()
# the user must specify a k (number of topics): to get started, the book picks 2 topics 
# set a seed so that the output of the model is reporoducible
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda

# NB: the larger the data, and as the number of topics you ask increases: 
# the longest it takes for R to estimate the model



### 4. EXPLORE RESULTS

# the function LDA() returns an object containing the full details of the model

# first, we extract the per-topic-per-word probabilities: "beta”
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics
# for each topic-term we have a probability of being generated from topic 1 and 2

# find the 10 terms most common within each topic
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%    # can also use" top_n(10, beta) 
  ungroup() %>%
  arrange(topic, -beta)
ap_top_terms

# this is a tidy data frame, thus easy to visualize
ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
# is there evidence that these words cohere into smt that can be called a topic?
# or do we need to re-run the model asking for more topics?

# second, we extract the per-document-per-topic probabilities: "gamma” 
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents
# 25% of words in document 1 are generated from topic 1
# most of these documents are drawn from a mix of topics
# but document 6 seems to be drawn almost entirely from topic 2

# tidy the document-term-matrix and use filters 
# to closely inspect most common words in document 6 
tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))