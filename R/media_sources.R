library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(httr)
library(jsonlite)

# Join MCQ data to freeform
freeforms <- read_csv("./DATA/kaggle-survey-2018/freeFormResponses.csv") %>%
        filter(row_number() > 1)

freeforms <- multiple_choice_responses %>%
        filter(row_number() > 1) %>%
        bind_cols(freeforms) %>%
        select(
                Q7,              # industry
                Q41_Part_1,      # value of bias
                Q43,             # time spent on bias
                Q38_OTHER_TEXT1, # media sources
                Q33_OTHER_TEXT1  # public datasets
        ) %>%
        rename(
                INDUSTRY            = Q7,
                VALUE_OF_BIAS       = Q41_Part_1,
                TIME_SPENT_ON_BIAS  = Q43,
                MEDIA_SOURCES       = Q38_OTHER_TEXT1,
                PUBLIC_DATA_SOURCES = Q33_OTHER_TEXT1
        )

# Industries that spend a lot of time on bias
industry_bias <- freeforms %>%
        group_by(INDUSTRY, TIME_SPENT_ON_BIAS) %>%
        summarize(count = n()) %>%
        ungroup() %>%
        na.omit() %>%
        mutate(TIME_MAX = as.numeric(str_extract(TIME_SPENT_ON_BIAS, "\\d+$"))) %>%
        group_by(INDUSTRY) %>%
        summarize(MEAN_TIME_MAX = weighted.mean(TIME_MAX, count)) %>%
        arrange(desc(MEAN_TIME_MAX))

head(industry_bias, 5)
tail(industry_bias, 5)

# Industry media sources
industry_media <- freeforms %>%
        na.omit() %>%
        group_by(INDUSTRY) %>%
        summarize(paste(MEDIA_SOURCES, collapse = ", "))

# Public data sets
industry_data <- freeforms %>%
        na.omit() %>%
        filter() %>%
        group_by(INDUSTRY) %>%
        summarize(SOURCES = paste(PUBLIC_DATA_SOURCES, collapse = ", "))

API_KEY <- Sys.getenv("NLU_KEY")
json <- fromJSON("./JSON/watson_body_entities.json") # parameters for entity request

industry_data_sources <- lapply(industry_data$SOURCES, function(x) {
        watson_NLU_query(x, json, API_KEY)
})

names(industry_data_sources) <- industry_data$INDUSTRY
