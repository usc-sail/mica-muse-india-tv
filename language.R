# load libraries ----
library(lme4)
library(lmerTest)
library(emmeans)
library(tidyverse)
library(Kendall)

emm_options(pbkrtest.limit = 28776, lmertest.limit = 28776)

# read data ----
lang_data <- read.csv("data/wide_lang.csv")
lang_religion_data <- read.csv("data/long_lang_religion.csv")
lang_gender_data <- read.csv("data/long_lang_gender.csv")

## derogatory ----
lang_data$derogatory_per <- 100 * lang_data$derogatory / lang_data$transcript
lang_data$derogatory_nonstop_per <-
  100 * lang_data$derogatory / lang_data$non_stopword_transcript

## religion ----
lang_religion_data$name_count_per <- 
  100 * lang_religion_data$name_count / lang_religion_data$transcript
lang_religion_data$name_count_nonstop_per <- 
  100 * lang_religion_data$name_count / lang_religion_data$non_stopword_transcript

## gender ----
lang_gender_data$name_count_per <- 
  100 * lang_gender_data$name_count / lang_gender_data$transcript
lang_gender_data$name_count_nonstop_per <- 
  100 * lang_gender_data$name_count / lang_gender_data$non_stopword_transcript

## models ---
# derogatory words
m_derogatory <- lmer(derogatory_per ~ year 
                     + (1|program) + (1|lang) + (1|genre), lang_data)
m_derogatory_x_lang <- lmer(derogatory_per ~ lang * year 
                          + (1|program) + (1|genre), lang_data)
derogatory_trends <- emtrends(m_derogatory, ~1, var = "year")
derogatory_x_lang_trends <- emtrends(m_derogatory_x_lang, ~lang, var = "year")

# religious person names
m_religion <- lmer(name_count_per ~ religion * year 
                   + (1|program) + (1|lang) + (1|genre), lang_religion_data)
m_religion_x_lang <- lmer(name_count_per ~ religion * lang * year 
                        + (1|program) + (1|genre), lang_religion_data)
religion_trends <- emtrends(m_religion, ~religion, var = "year")
religion_x_lang_trends <- emtrends(m_religion_x_lang, ~lang + religion, var = "year") %>%
  as_tibble() %>% arrange(lang, religion)

# gendered person names
m_gendername <- lmer(name_count_per ~ gender * year 
                      + (1|program) + (1|lang) + (1|genre), lang_gender_data)
m_gendername_x_lang <- lmer(name_count_per ~ gender * lang * year 
                           + (1|program) + (1|genre), lang_gender_data)
gendername_trends <- emtrends(m_gendername, ~gender, var = "year")
gendername_x_lang_trends <- emtrends(m_gendername_x_lang, ~lang + gender, var = "year") %>%
  as_tibble() %>% arrange(lang, gender)

## plots ---
# derogatory words
ggplot(lang_data, aes(x = year, y = derogatory_per)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percentage of Derogatory Words")
ggsave("data/plots/derogatory_trend.png", device = "png")

ggplot(lang_data, aes(x = year, y = derogatory_per, group = lang, 
                      color = lang)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percentage of Derogatory Words")
ggsave("data/plots/derogatory_x_lang_trend.png", device = "png")

# religion person names
ggplot(lang_religion_data, aes(x = year, y = name_count_per, 
                               group = religion, color = religion)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percentage of person names of different religions")
ggsave("data/plots/religion_trend.png", device = "png")

ggplot(lang_religion_data, aes(x = year, y = name_count_per, 
                               group = religion, color = religion)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percentage of person names of different religions") +
  facet_wrap(~lang)
ggsave("data/plots/religion_x_lang_trend.png", device = "png")

# gender person names
ggplot(lang_gender_data, aes(x = year, y = name_count_per, 
                               group = gender, color = gender)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percentage of person names of different genders")
ggsave("data/plots/gendername_trend.png", device = "png")

ggplot(lang_gender_data, aes(x = year, y = name_count_per, 
                               group = gender, color = gender)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percentage of person names of different genders") +
  facet_wrap(~lang)
ggsave("data/plots/gendername_x_lang_trend.png", device = "png")