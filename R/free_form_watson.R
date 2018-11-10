#' Here we're going to leverage IBM Watson's natural language resource libraries
#' to extract sentiment, emotion, personality and entities from free-form text
#' responses in the survey data.

# NLU docs: https://console.bluemix.net/apidocs/natural-language-understanding#related-information

library(readr)
library(dplyr)
library(httr)
library(jsonlite)

# Import free-form responses
freeforms <- read_csv("./DATA/kaggle-survey-2018/freeFormResponses.csv")
freeforms[1:10, ] %>% View()

# Find which questions are worth analysing 
sort(sapply(freeforms, function(x) length(x[!is.na(x)])), decreasing = TRUE) / nrow(freeforms)

# Natural language understanding
# Personality insights
# Tone analysis

# IBM Watson API anatomy
API_KEY     <- "PdW3hpeF7RmNOqWoJZwZsm7TtY3nOLZKB96PX3UX4Gxr"
SERVICE_URL <- "https://gateway-fra.watsonplatform.net/natural-language-understanding/api"
METHOD      <- "/v1/analyze"
VERSION     <- "2018-03-16"

# Entity analysis of Q38 to find top media sources
q38_responses <- freeforms$Q38_OTHER_TEXT[-1] %>% na.omit()

json <- fromJSON("./JSON/watson_body_entities.json")

json$text <- paste(q38_responses, collapse = ", ")

# Make API call
res <- POST(
        url = paste0(SERVICE_URL, METHOD),
        body = json, encode = "json",
        query = list(version = VERSION),
        add_headers("Content-Type" = "application/json"),
        authenticate("apikey", API_KEY),
        verbose()
        )

res
api_response <- content(res, "text")
api_response_parsed <- fromJSON(api_response, flatten = TRUE, simplifyDataFrame = TRUE)

entities <- api_response_parsed$entities
