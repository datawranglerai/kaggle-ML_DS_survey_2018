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

watson_calculate_nlu_units <- function(text, features) {
        #'
        #' Calculates the number of NLU units that will be used up in an API
        #' request.
        #' 
        #' @param characters. Character. Text to be analysed.
        #' @param features. Named list. Features being requested.
        #'
        #' @return Numeric. Number of NLU units to be used.
        #'
        #' @note 
        #' NOTE: A NLU item is based on the number of data units enriched and
        #' the number of enrichment features applied. A data unit is 10,000
        #' characters or less. For example: extracting Entities and Sentiment
        #' from 15,000 characters of text is (2 Data Units * 2
        #' Enrichment Features) = 4 NLU Items. A custom model refers to an
        #' annotation model developed with Watson Knowledge Studio.
        #' 
        #' @references 
        #' NLU API: https://www.ibm.com/watson/services/natural-language-understanding/
        #'
        
        data_units <- ceiling(nchar(text) / 10000)
        enrichment_features <- length(parameters[["features"]])
        
        data_units * enrichment_features
}

watson_NLU_query <- function(text, parameters, api_key) {
        #'
        #' Passes text through IBM Watson's Natural Language Understanding API.
        #'
        #' @param text Character. Text to be analysed by the
        #' @param parameters Named list. Parameters to be passed to the body of
        #' the API request.
        #'
        #' @return Dataframe. Entities identified in the text.
        #'
        #' @references 
        #' API documentation: https://console.bluemix.net/apidocs/natural-language-understanding#related-information
        #'
        
        library(httr)
        library(jsonlite)
        
        # Service anatomy
        SERVICE_URL <- "https://gateway-fra.watsonplatform.net/natural-language-understanding/api"
        METHOD      <- "/v1/analyze"
        VERSION     <- "2018-03-16"
        
        parameters[["text"]] <- text
        
        cat("This request will use", watson_calculate_nlu_units(text, parameters), "NLU units\n")
        
        # Make API call
        res <- POST(
                url = paste0(SERVICE_URL, METHOD),
                body = parameters,
                encode = "json",
                query = list(version = VERSION),
                add_headers("Content-Type" = "application/json"),
                authenticate("apikey", api_key)
        )
        
        api_response <- content(res, "text")
        api_response_parsed <- fromJSON(api_response, flatten = TRUE, simplifyDataFrame = TRUE)
        
        api_response_parsed$entities
        
}

API_KEY <- "PdW3hpeF7RmNOqWoJZwZsm7TtY3nOLZKB96PX3UX4Gxr"
json <- fromJSON("./JSON/watson_body_entities.json") # parameters for entity request

# QUESTION 38 ----

# Entity analysis of Q38 to find top media sources
q38_responses <- freeforms$Q38_OTHER_TEXT[-1] %>% na.omit()
text <- paste(q38_responses, collapse = ", ")



# Find entities
entities_q38 <- watson_NLU_query(text, parameters = json, api_key = API_KEY)


# QUESTION 12 ----

q12_responses <- freeforms$Q12_OTHER_TEXT[-1] %>% na.omit()
text <- paste(q12_responses, collapse = ", ")

entities_q12 <- watson_NLU_query(text, json, API_KEY)
