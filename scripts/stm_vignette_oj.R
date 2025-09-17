
### SETTINGS
#rm(list = ls())

library(tidyverse)
library(stm)

# documents have meta data associated with them

# definitions
  # TOPIC a topic is defined as a mixture over words where each word has a probability of belonging to a topic.
  # DOCUMENT. a document is a mixture over topics, meaning that a single document can be composed of multiple topics.
      # the sum of topic proportions across all topics for a document is one
      # the sum of word probabilities for a given toic is one
  # TOPICAL PREVALENCE
    # how much of a document is associated with a topic    
  # TOPICAL CONTENT
    # refers to words used within a topic 
  # METADATA.
    # metadata that explain topical prevalence are referred to as topical prevalence covariats
    # variables that explain topical content are referred to as topical content covariates
  
# GENERATIVE PROCESS FOR EACH DOCUMENT (d) WITH VOCABULARY SIZE V FOR A STM MODEL WITH K TOPICS
  # 1. didn't understand
  # 2. didn't understand
  # 3. for each word in document (n = 1,...N_d)
    # draw word's topic assignment based on document specific distribution over topics
    # conditional on topic, draw observed word from that topic

# 3. use of the stm package

  # broad steps to prep and estimation: ingest >> prepare >> estimate
    # ingest = textProcessor() and readCorpus
    # prpare = prepDocuments() and plotRemoved()
    # estimate = stm

  # broad steps after estimation
    # evaluate
    # understand
    # visualize

  # ingest
      # three parts (object) of a text corpus:
        # documents object is a list containing word indices and associated count
        # vocab is a character vector containing words asssociated with the word indeces
        # metadata matrix containing document covariates

      # reading in data from a spreadsheet
        # first, get data into R dataframe
        # textProcessor() function conver and process the data to get it ready for analysis in stm package

# reading in data

google_doc_id <- "1LcX-JnpGB0lU1iDnXnxB6WFqBywUKpew" # google file ID
data <-read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_doc_id), stringsAsFactors = FALSE)
data %>% glimpse()

# textProcessor() function
  # Function that takes in a vector of raw texts (in a variety of languages) and performs basic operations. This function is essentially a wrapper tm package where various user specified options can be selected.
  # does stuff like: converts to lowercase, removes "stop words", removes numbers, removes punctuation, turns all words into "stem" words
processed <- textProcessor(data$documents, metadata = data)
processed %>% str()

# prepDocuments() function: 
  # Performs several corpus manipulations including removing words and renumbering word indices (to correct for zero-indexing and/or unused words in the vocab vector).
out <- prepDocuments(processed$documents, processed$vocab, processed$meta) # process loaded data to make sure it is in right format, and remove infrequent terms
  # out <- prepDocuments(processed$documents, processed$vocab, + processed$meta, lower.thresh = 15)
  # ?prepDocuments
    # prepDocuments(
    #   documents,
    #   vocab,
    #   meta = NULL,
    #   lower.thresh = 1,
    #   upper.thresh = Inf,
    #   subsample = NULL,
    #   verbose = TRUE
    # )    
  #lower.thresh = 1
    # Words which do not appear in a number of documents greater than lower.thresh will be dropped and both the documents and vocab files will be renumbered accordingly. If this causes all words within a document to be dropped, a message will print to the screen at it will also return vector of the documents removed so you can update your meta data as well.
  # upper.thresh = Inf,
    # As with lower.thresh but this provides an upper bound. Words which appear in at least this number of documents will be dropped. Defaults to Inf which does no filtering

out %>% str()

docs <- out$documents # documents in desired matrix form
  docs %>% str()

vocab <- out$vocab
  vocab %>% str()

meta <-out$meta
  meta %>% str()
  
# plot removed terms
  plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))
  
# 3.3 Estimating the structural topic model
  
  # innovation of STEM is that it incorporates metadata into the topic modeling framework
  # in STM metadata can be entered in the topic model in two ways:
    # topical prevalence
      # topical prevalence defined:
        # how much of a document is associated with a topic    
      # metadata covariates for topical prevalence allow the observed metadata to affect the frequency with which a topic is discussed within a document
        # own interpretation: so the metadata becomes part of a document?
        # before, topical prevalence for each document was determined solely by the "document" cell; 
          # now, topical prevalence for each document is determined by document cell + some function of function of covariates
    # topical content
      # topical content defined:
        # refers to words used within a topic 
        # word rate use?? 
      # Covariates in topical content allow the observed metadata to affect the word rate use within a given topic
        # Without a content covariate:
        #   The model will treat different word choices (e.g., Dems: "insurance, access"
        #   vs Reps: "costs, regulation") as separate topics.
        #
        # With a content covariate (e.g., party):
        #   The model keeps this as one topic (e.g., "healthcare") but estimates
        #   different word distributions depending on the covariate.  
  
  # Main document text will be the "description" column.
  #
  # Question: Should the "current_financing_status" column be used as a covariate 
  #           for topic prevalence or for topical content?
  #
  # Answer:
  # - Better as a topical prevalence covariate.
  # - Reason: financing stage (Angel, VC, Accelerator, etc.) likely affects 
  #   *which topics* appear in company descriptions 
  #   (e.g., early-stage prototyping vs. scaling SaaS vs. revenue generation).
  # - Less useful as a topical content covariate, because PitchBook descriptions 
  #   are written in a standardized editorial style, so the *word choice within 
  #   a topic* won’t vary much by financing status.
  #
  
  
  # Covariates and STM:
  #
  # - "current_financing_status" (Angel, VC, Accelerator, etc.) → best as a 
  #   topical prevalence covariate. Financing stage affects *which topics* appear 
  #   more often in descriptions (e.g., early traction vs. scaling vs. revenue generation).
  #
  # - Time (e.g., year) → a good candidate for a topical content covariate. 
  #   Over time, companies still discuss the *same broad topics* (e.g., EdTech), 
  #   but the *words* within those topics change. 
  #   Example: in 2015, "MOOCs, digital textbooks"; in 2025, "AI tutors, adaptive learning."
  #
  # Summary:
  # - Status-like variables (financing stage, sector, geography) → use for prevalence.
  # - Temporal or framing-related variables (year, party, media outlet) → use for content.
  
# estimation with topical prevalence parameter (e.g., current financing status is "angel", venture capital, etc)  
  # Topical prevalence captures how much each topic contributes to a document.
  # topical prevalence is a function of "rating" (e.g., liberal or conservative) and the variable "day"
  
poliblogPrevFit <- stm(documents = out$documents, vocab = out$vocab,
                         K = 20, prevalence =~ rating + s(day),
                         max.em.its = 75, data = out$meta,
                         init.type = "Spectral")  
  
  # Summary: current_financing_status → use in `prevalence =` not `content =`.
    #load(url("https://github.com/bstewart/stm/blob/gh-pages/files/VignetteObjects.RData?raw)

# 3.4 evaluate: model selection and search

  # basically, use spectral


#poliblogSelect <- selectModel(out$documents, out$vocab, K = 20,prevalence =~ rating + s(day), max.em.its = 75, data = out$meta, runs = 20, seed = 8458159)


# Model search across numbers of topics

  # STM assumes a fixed user-specified number of topics. There is not a “right” answer to the number of topics that are appropriate for a given corpus (Grimmer and Stewart 2013), but the function searchK uses a data-driven approach to selecting the number of topics.
  # For example, one could estimate a STM model for 7 and 10 topics and compare the results along each of the criteria.
    # according to what criteria???

storage <- searchK(out$documents, out$vocab, K = c(7, 10),  prevalence =~ rating + s(day), data = meta)

plot(storage)

# 3.5 interpreting STM by plotting and inspecting results

  # understanding topics through words and example documents
    # We next describe two approaches for users to explore the topics that have been estimated. The first approach is to look at collections of words that are associated with topics. The second approach is to examine actual documents that are estimated to be highly associated with each topic.
      # To explore the words associated with each topic we can use the labelTopics function.

labelTopics(poliblogPrevFit, c(3, 7, 20))
labelTopics(poliblogPrevFit)

  # 