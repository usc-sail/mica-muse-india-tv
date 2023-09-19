# set up ----------------------------------------------------------------------
## load libraries ----
library(lme4)
library(lmerTest)
library(emmeans)
library(tidyverse)
library(Kendall)

emm_options(pbkrtest.limit = 28776, lmertest.limit = 28776)

## read data ----
## read long-form data (created and cleaned by @Sabyasachee)
lang_data <- read.csv("data/wide_lang.csv")
lang_religion_data <- read.csv("data/long_lang_religion.csv")
lang_gender_data <- read.csv("data/long_lang_gender.csv")

# derogatory
lang_data$derogatory_per <- 100 * lang_data$derogatory / lang_data$transcript
lang_data$derogatory_nonstop_per <-
  100 * lang_data$derogatory / lang_data$non_stopword_transcript

# religion
lang_religion_data$name_count_per <- 
  100 * lang_religion_data$name_count / lang_religion_data$transcript
lang_religion_data$name_count_nonstop_per <- 
  100 * lang_religion_data$name_count / lang_religion_data$non_stopword_transcript

# gender
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

# gendered person names
m_gendername <- lmer(name_count_per ~ gender * year 
                      + (1|program) + (1|lang) + (1|genre), lang_gender_data)
m_gendername_x_lang <- lmer(name_count_per ~ gender * lang * year 
                           + (1|program) + (1|genre), lang_gender_data)

## plots ---
# derogatory words
ggplot(lang_data, aes(x = year, y = derogatory_per)) +
  # geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percentage of Derogatory Words")
ggsave("data/derogatory_trend.eps", device = "eps")

ggplot(lang_data, aes(x = year, y = derogatory_per, group = lang, 
                      color = lang)) +
  # geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percentage of Derogatory Words")
ggsave("data/derogatory_x_lang_trend.eps", device = "eps")

# religion person names
ggplot(lang_religion_data, aes(x = year, y = name_count_per, 
                               group = religion, color = religion)) +
  # geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percentage of person names of different religions")
ggsave("data/religion_trend.eps", device = "eps")

ggplot(lang_religion_data, aes(x = year, y = name_count_per, 
                               group = religion, color = religion)) +
  # geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percentage of person names of different religions") +
  facet_wrap(~lang)
ggsave("data/religion_x_lang_trend.eps", device = "eps")

# gender person names
ggplot(lang_gender_data, aes(x = year, y = name_count_per, 
                               group = gender, color = gender)) +
  # geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percentage of person names of different genders")
ggsave("data/gendername_trend.eps", device = "eps")

ggplot(lang_gender_data, aes(x = year, y = name_count_per, 
                               group = gender, color = gender)) +
  # geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percentage of person names of different genders") +
  facet_wrap(~lang)
ggsave("data/gendername_x_lang_trend.eps", device = "eps")