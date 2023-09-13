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
lang_data <- read.csv("lang_only_wide_form.csv")
lang_religion_data <- read.csv("lang_only_religion_long_form.csv")
lang_gender_data <- read.csv("lang_only_gender_long_form.csv")

# derogatory words
lang_data$derogatory_per <- 100 * lang_data$derogatory / lang_data$transcript

# controversial words
lang_data$controversial_per <- 
  100 * lang_data$controversial / lang_data$transcript

# person name count
lang_religion_data$person_name_count_per <- 
  100 * lang_religion_data$person_name_count / lang_data$transcript

lang_gender_data$person_name_count_per <- 
  100 * lang_gender_data$person_name_count / lang_data$transcript

## models ---
# derogatory words
m_derogatory <- lmer(derogatory_per ~ year 
                     + (1|program) + (1|lang) + (1|genre), lang_data)
m_derogatory_lang <- lmer(derogatory_per ~ lang * year 
                          + (1|program) + (1|genre), lang_data)

# religious person names
m_religion <- lmer(person_name_count_per ~ religion * year 
                   + (1|program) + (1|lang) + (1|genre), lang_religion_data)
m_religion_lang <- lmer(person_name_count_per ~ religion * lang * year 
                        + (1|program) + (1|genre), lang_religion_data)

# gendered person names
m_gender_name <- lmer(person_name_count_per ~ gender * year 
                      + (1|program) + (1|lang) + (1|genre), lang_gender_data)
m_gender_name_lang <- lmer(person_name_count_per ~ gender * lang * year 
                           + (1|program) + (1|genre), lang_gender_data)

## plots ---
# derogatory words
ggplot(lang_data, aes(x = year, y = derogatory_per)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percentage of Derogatory Words")
ggsave("data/derogatory_trend.png", device = "png")

ggplot(lang_data, aes(x = year, y = derogatory_per, group = lang, 
                      color = lang)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percentage of Derogatory Words")
ggsave("data/derogatory_x_lang_trend.png", device = "png")

# religion person names
ggplot(lang_religion_data, aes(x = year, y = person_name_count_per, 
                               group = religion, color = religion)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percentage of person names of different religions")
ggsave("data/religion_trend.png", device = "png")

ggplot(lang_religion_data, aes(x = year, y = person_name_count_per, 
                               group = religion, color = religion)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percentage of person names of different religions") +
  facet_wrap(~lang)
ggsave("data/religion_x_lang_trend.png", device = "png")

# gender person names
ggplot(lang_gender_data, aes(x = year, y = person_name_count_per, 
                               group = gender, color = gender)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percentage of person names of different genders")
ggsave("data/gender_name_trend.png", device = "png")

ggplot(lang_gender_data, aes(x = year, y = person_name_count_per, 
                               group = gender, color = gender)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percentage of person names of different genders") +
  facet_wrap(~lang)
ggsave("data/gender_name_x_lang_trend.png", device = "png")