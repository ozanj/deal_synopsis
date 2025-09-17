
library(topicmodels)
library(tm)

data('AssociatedPress')

AssociatedPress

#this is the document term matrix
inspect(AssociatedPress[1:5, 1:5])

AP_topic_model<-LDA(AssociatedPress, 
                      k=10, 
                      control = list(seed = 321) # so everyone gets the same result
                    )

AP_topic_model

library(tidytext)
library(dplyr)
library(ggplot2)

AP_topics <- tidy(AP_topic_model, matrix = "beta")

ap_top_terms <- 
  AP_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# STRUCTURAL TOPIC MODELING
library(stm)

google_doc_id <- "1LcX-JnpGB0lU1iDnXnxB6WFqBywUKpew" # google file ID
poliblogs<-read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_doc_id), stringsAsFactors = FALSE)

poliblogs %>% glimpse()

processed <- textProcessor(poliblogs$documents, metadata = poliblogs)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

out
docs
vocab %>% str()
vocab
meta

First_STM <- stm(documents = out$documents, vocab = out$vocab,
                 K = 10, prevalence =~ rating + s(day) ,
                 max.em.its = 75, data = out$meta,
                 init.type = "Spectral", verbose = FALSE)

plot(First_STM)

findThoughts(First_STM, texts = poliblogs$documents,
             n = 2, topics = 3)

