library(readr)
library(dplyr)
library(stringr)

format_survey_schema <- function(x) {
        #'
        #' Formats the survey schema file for the 2018 Kaggle ML & DS Survey
        #' Challenge into a tidy, analysis-friendly format.
        #'
        #' @param x Character. Path to Kaggle survey schema file.
        #'
        #' @return Dataframe. Number of respondents per question as well as
        #' question text, sorted by question number.
        #'
        #' @references 
        #' Kaggle competition overview: https://www.kaggle.com/kaggle/kaggle-survey-2018/home
        #' Schema details: https://www.kaggle.com/kaggle/kaggle-survey-2018#SurveySchema.csv
        #'

        survey_schema_raw <- read_csv(x) %>%
                select(starts_with("Q"))       # select question columns only
        
        survey_schema <- data_frame(
                question_label  = colnames(survey_schema_raw),
                question_text   = as.character(survey_schema_raw[1,]),
                respondents     = as.numeric(survey_schema_raw[2,]),
                question_number = as.numeric(sub("Q", "", question_label))
        ) %>%
                mutate(
                        question_text = str_match(question_text, "(^.*\\?).*$")[,2]
                ) %>%
                arrange(question_number)
        
        survey_schema
        
}

format_mcq_schema <- function(x) {
        #'
        #' Formats the question structure of all multiple choice responses into
        #' an analysis-friendly structure.
        #'
        #' @param x Character. Path to Kaggle multiple choice response file.
        #'
        #' @return Dataframe. Question text per question and question part.
        #'
        #' @references 
        #' Kaggle competition overview: https://www.kaggle.com/kaggle/kaggle-survey-2018/home
        #' MCQ details: https://www.kaggle.com/kaggle/kaggle-survey-2018#multipleChoiceResponses.csv
        #'
        
        survey_schema <- format_survey_schema("./DATA/kaggle-survey-2018/SurveySchema.csv")
        
        mcq_schema_raw <- read_csv(x, n_max = 1) %>%
                select(starts_with("Q")) 
        
        mcq_schema <- data_frame(
                question_label_detailed  = colnames(mcq_schema_raw),
                choice                   = as.character(mcq_schema_raw[1,]),
                question_label           = str_match(question_label_detailed, "(^Q\\d+).*")[,2]
        ) %>%
                filter(!grepl("TEXT", question_label_detailed)) %>%
                left_join(survey_schema, by = "question_label") %>%
                mutate(
                        choice = str_match(choice, ".*(?:Selected Choice)? - (.*)$")[,2],
                        choice = gsub("Selected Choice", NA, choice),
                        question_text = str_match(question_text, "(^.*[\\?:]).*$")[,2]
                        ) 
        
        mcq_schema
        
}