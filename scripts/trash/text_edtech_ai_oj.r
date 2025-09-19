################################################################################
## [ PROJ ] < Comapny description, applying topic modeling>
## [ FILE ] < text_edtech_ai_oj.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 9/17/2025
## [ DESC ] < load data on deals + companies in the edtech+AI space. process and then apply structural topic modeling>
################################################################################


# install.packages("devtools")
#devtools::install_github("oscarkjell/text")
library(text)

# stuff that needs to be done once per machine (unless you wipe the environment)
  #textrpp_install() # sets up Python env + installs numpy, torch, transformers, etc
  #textrpp_initialize() # This makes sure the conda env is set up with the right Python packages (torch, transformers, etc.). You only need to do this once per machine (unless you wipe the env).

#Switch reticulate to use that env (each new R session)

reticulate::use_condaenv("textrpp_condaenv", required = TRUE)
reticulate::py_config()

Sys.setenv(RETICULATE_PYTHON = "~/Library/r-miniconda-arm64/envs/textrpp_condaenv/bin/python")

Sys.setenv(RETICULATE_PYTHON = "/opt/homebrew/opt/python@3.12/bin/python3.12")
reticulate::py_config()

library(reticulate)
py_config()


# initialize and save the profile so R remembers next time
textrpp_initialize(save_profile = TRUE)

library(text)
library(tibble)

# tiny toy corpus
texts <- tibble(text = c(
  "I love studying topic models",
  "Python and R can both do text analysis",
  "Structural topic modeling includes covariates"
))

# run BERTopic via textTopics
model <- textTopics(
  data = texts,
  variable_name = "text",
  embedding_model = "distilroberta",
  num_top_words = 5
)

# peek at the results
head(model)
