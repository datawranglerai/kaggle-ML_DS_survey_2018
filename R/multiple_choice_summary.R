# Import schema
source("./R/import_schema.R")
survey_schema <- format_survey_schema("./DATA/kaggle-survey-2018/SurveySchema.csv")
mcq_schema    <- format_mcq_schema("./DATA/kaggle-survey-2018/multipleChoiceResponses.csv")

multiple_choice_responses <- read_csv("./DATA/kaggle-survey-2018/multipleChoiceResponses.csv")

# Remove any columns with user-entered text
cols <- colSums(mapply(grepl, ".* - Text$", multiple_choice_responses))
responses_mcq <- multiple_choice_responses[, which(cols == 0)]

# Tally up results for each question
mcq_data <- lapply(survey_schema$question_label, function(x) {
        question  <- x
        col_regex <- paste0("^", question, "(?:\\D.*)?$")
        
        cat("Processing", question, "\n")
        
        # Summarize MCQ results for each question
        responses_mcq %>%
                filter(row_number() > 1) %>%  # remove question text row   
                select(matches(col_regex)) %>%
                gather(question, response,
                       matches(question),
                       na.rm = TRUE) %>%
                group_by(response) %>%
                summarize(count = n()) %>%
                arrange(desc(count))
})

names(mcq_data) <- paste(survey_schema$question_label, 
                         survey_schema$question_text,
                         sep = ": ")

head(mcq_data)

mcq_data$`Q9: What is your current yearly compensation (approximate $USD)?`

saveRDS(mcq_data, file = "./OUT/mcq_summary.RDS")
