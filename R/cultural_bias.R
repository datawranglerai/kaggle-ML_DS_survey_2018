library(readr)
library(tidyr)
library(dplyr)
library(countrycode)

multiple_choice_responses <- read_csv(
        "./DATA/kaggle-survey-2018/multipleChoiceResponses.csv",
        progress = FALSE
        ) 

# Country: Q3
# Importance of bias: Q41_Part_1
# % of time spent working on bias: Q43
# Most difficult thing about being unbiased: Q44

importance_lvls <- c(NA,
                     "No opinion; I do not know",
                     "Not at all important",
                     "Slightly important",
                     "Very important")

importance_lbls <- c("Didn't answer",
                     "No opinion/don't know",
                     "Not important",
                     "Somewhat important",
                     "Very important")

country_importances <- multiple_choice_responses %>%
        filter(row_number() > 1) %>%
        group_by(Q3, Q41_Part_1) %>% 
        summarize(count = n()) %>%
        
        mutate(
                Q41_Part_1 = factor(Q41_Part_1,
                                   levels = importance_lvls,
                                   labels = importance_lbls,
                                   ordered = TRUE,
                                   exclude = NULL)
                ) %>%
        # Want to order bar graph by number of people answering "very important"
        mutate(pc_v_important = count[Q41_Part_1 == "Very important"] / sum(count, na.rm = TRUE))


ggplot(country_importances, aes(reorder(Q3, pc_v_important), count, fill = Q41_Part_1)) +
        geom_bar(stat = "identity", position = "fill") + 
        scale_fill_manual(values = wes_palette("Cavalcanti1", 5, "discrete")) +
        scale_y_continuous(labels = percent) +
        coord_flip() +
        guides(fill = guide_legend(reverse = TRUE)) +
        theme_jw() +
        theme(
                legend.position = "top",
                legend.title = element_blank(),
                legend.text = element_text(size = 6),
                axis.text.y = element_text(size = 5)
        ) 

continent_importances <- country_importances %>%
        filter(
                Q3 != "Other",
                !grepl("^I do not", Q3)
        ) %>%
        mutate(
                continent = countrycode(sourcevar = Q3,
                                        origin = "country.name",
                                        destination = "continent")
        ) %>%
        group_by(continent, Q41_Part_1) %>%
        summarize(count = sum(count)) %>%
        # Want to order bar graph by number of people answering "very important"
        mutate(pc_v_important = sum(count[grepl("Very|Somewhat", Q41_Part_1)]) / sum(count, na.rm = TRUE))

ggplot(continent_importances, aes(reorder(continent, pc_v_important), count, fill = Q41_Part_1)) +
        geom_bar(stat = "identity", position = "fill") + 
        scale_fill_manual(values = wes_palette("Cavalcanti1", 5, "discrete")) +
        scale_y_continuous(labels = percent) +
        coord_flip() +
        guides(fill = guide_legend(reverse = TRUE)) +
        theme_jw() +
        theme(
                legend.position = "top",
                legend.title = element_blank(),
                legend.text = element_text(size = 6),
                axis.text.y = element_text(size = 5)
        ) 
