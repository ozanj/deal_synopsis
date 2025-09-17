
# Topic models packages
library(topicmodels)

# Other packages
library(tidytext)
library(dplyr)
library(ggplot2)

# Structural Topic modeling packages
library(stm)



########### Topic Modeling: Latent Dirichlet Allocation

# load data set that's within package
data("AssociatedPress")
AP_topic_model<-LDA(AssociatedPress, k=10, control = list(seed = 321))
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

########## Structural Topic Modeling

# load data
google_doc_id <- "1LcX-JnpGB0lU1iDnXnxB6WFqBywUKpew" # google file ID
poliblogs<-read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_doc_id), stringsAsFactors = FALSE)

# STEP ONE: TEXT PROCESSING
    # using stm `textProcessor`
processed <- textProcessor(poliblogs$documents, metadata = poliblogs)
    # other ways to conduct text processing

# STEP TWO: PREPARING DATA
    # using stm `prepDocuments`
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta
        # can specify what threshold of infrequent terms want to remove
plotRemoved(processed$documents, lower.thresh = seq(1,200, by = 100)) #first see what different theshhold do
out <- prepDocuments(processed$documents, processed$vocab,
                     processed$meta, lower.thresh = 15) #then indicate threshhold

# STEP THREE: ESTIMATION
    # spectral initialization (use this one)
First_STM <- stm(documents = out$documents, vocab = out$vocab,
                 K = 10, prevalence =~ rating + s(day) , #prevalence is how meta data should be used 
                 max.em.its = 75, data = out$meta,
                 init.type = "Spectral", verbose = FALSE)
Second_STM <- stm(documents = out$documents, vocab = out$vocab,
                 K = 10, prevalence =~ rating + s(day) , #prevalence is how meta data should be used 
                 max.em.its = 75, data = out$meta,
                 runs = 20, seed = 845859)

  # figure out what number of k to use

#findingk <- searchK(out$documents, out$vocab, K = c(10:30), prevalence =~ rating + s(day), data = meta, verbose=FALSE)









# doing the STM
First_STM <- stm(documents = out$documents, vocab = out$vocab,
                 K = 10, prevalence =~ rating + s(day) , #prevalence is how meta data should be used 
                 max.em.its = 75, data = out$meta,
                 init.type = "Spectral", verbose = FALSE)
plot(First_STM)

# inspect topics by look for certain number of examples of certain topics
findThoughts(First_STM, texts = poliblogs$documents,
             n = 2, topics = 3)

#instead of single value of k, specify a range of k

#plot(findingk)
