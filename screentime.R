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
data <- read.csv("video_only_long_form.csv")

gender_data <- data %>%
  group_by(video_key, program, lang, genre, year, gender) %>% 
  summarise(n = sum(faces), .groups = "drop") %>% 
  group_by(video_key, program, lang, genre, year) %>% 
  mutate(screentime = n / (sum(n) + 1e-23) * 100) %>% 
  ungroup()

age_data <- data %>%
  group_by(video_key, program, lang, genre, year, age) %>% 
  summarise(n = sum(faces), .groups = "drop") %>% 
  group_by(video_key, program, lang, genre, year) %>% 
  mutate(screentime = n / (sum(n) + 1e-23) * 100) %>% 
  ungroup()

skintone_data <- data %>%
  group_by(video_key, program, lang, genre, year, skintone) %>% 
  summarise(n = sum(faces), .groups = "drop") %>% 
  group_by(video_key, program, lang, genre, year) %>% 
  mutate(screentime = n / (sum(n) + 1e-23) * 100) %>% 
  ungroup()

gender_x_age_data <- data %>%
  group_by(video_key, program, lang, genre, year, gender, age) %>% 
  summarise(n = sum(faces), .groups = "drop") %>% 
  group_by(video_key, program, lang, genre, year) %>% 
  mutate(screentime = n / (sum(n) + 1e-23) * 100) %>% 
  ungroup()

gender_x_skintone_data <- data %>%
  group_by(video_key, program, lang, genre, year, gender, skintone) %>% 
  summarise(n = sum(faces), .groups = "drop") %>% 
  group_by(video_key, program, lang, genre, year) %>% 
  mutate(screentime = n / (sum(n) + 1e-23) * 100) %>% 
  ungroup()

age_x_skintone_data <- data %>%
  group_by(video_key, program, lang, genre, year, age, skintone) %>% 
  summarise(n = sum(faces), .groups = "drop") %>% 
  group_by(video_key, program, lang, genre, year) %>% 
  mutate(screentime = n / (sum(n) + 1e-23) * 100) %>% 
  ungroup()

gender_x_age_x_skintone_data <- data %>%
  group_by(video_key, program, lang, genre, year, gender, age, skintone) %>% 
  summarise(n = sum(faces), .groups = "drop") %>% 
  group_by(video_key, program, lang, genre, year) %>% 
  mutate(screentime = n / (sum(n) + 1e-23) * 100) %>% 
  ungroup()

# models
# gender
m_gender <- lmer(screentime ~ gender * year 
                 + (1|program:gender) 
                 + (1|lang:gender) 
                 + (1|genre:gender), gender_data)
gender_trends <- emtrends(m_gender, ~gender, var="year") %>% as_tibble()

m_gender_lang <- lmer(screentime ~ gender * lang * year
                      + (1|program:gender)
                      + (1|genre:gender), gender_data)
gender_lang_trends <- emtrends(m_gender_lang, ~gender + lang, var = "year") %>% 
  as_tibble() %>% arrange(gender, lang)

# age
m_age <- lmer(screentime ~ age * year 
              + (1|program:age) 
              + (1|lang:age) 
              + (1|genre:age), age_data)
age_trends <- emtrends(m_age, ~age, var="year") %>% as_tibble()

m_age_lang <- lmer(screentime ~ age * lang * year
                      + (1|program:age)
                      + (1|genre:age), age_data)
age_lang_trends <- emtrends(m_age_lang, ~age + lang, var = "year") %>% 
  as_tibble() %>% arrange(age, lang)

# skintone
m_skintone <- lmer(screentime ~ skintone * year 
                   + (1|program:skintone) 
                   + (1|lang:skintone) 
                   + (1|genre:skintone), skintone_data)
skintone_trends <- emtrends(m_skintone, ~skintone, var="year") %>% as_tibble()

m_skintone_lang <- lmer(screentime ~ skintone * lang * year
                      + (1|program:skintone)
                      + (1|genre:skintone), skintone_data)
skintone_lang_trends <- emtrends(m_skintone_lang, ~skintone + lang, 
                                 var = "year") %>% 
  as_tibble() %>% arrange(skintone, lang)

# gender x age
m_gender_x_age <- lmer(screentime ~ gender * age * year 
                       + (1|program:gender:age) 
                       + (1|lang:gender:age) 
                       + (1|genre:gender:age), 
                       gender_x_age_data)
gender_x_age_trends <- emtrends(m_gender_x_age, ~ age + gender, var="year") %>% 
  as_tibble()

# gender x skintone
m_gender_x_skintone <- lmer(screentime ~ gender * skintone * year 
                            + (1|program:gender:skintone) 
                            + (1|lang:gender:skintone) 
                            + (1|genre:gender:skintone), gender_x_skintone_data)
gender_x_skintone_trends <- emtrends(m_gender_x_skintone, ~ skintone + gender, 
                                     var="year") %>% as_tibble()

# age x skintone
m_age_x_skintone <- lmer(screentime ~ age * skintone * year 
                            + (1|program:age:skintone) 
                            + (1|lang:age:skintone) 
                            + (1|genre:age:skintone), age_x_skintone_data)
age_x_skintone_trends <- emtrends(m_age_x_skintone, ~ skintone + age, 
                                     var="year") %>% as_tibble()

# gender x age x skintone
m_gender_x_age_x_skintone <- lmer(screentime ~ gender * age * skintone * year
                                  + (1|program:gender:age:skintone)
                                  + (1|lang:gender:age:skintone)
                                  + (1|genre:gender:age:skintone),
                                  gender_x_age_x_skintone_data)
gender_x_age_skintone_trends <- emtrends(m_gender_x_age_x_skintone, 
                                         ~ skintone + age + gender, 
                                         var="year") %>% as_tibble()

## plots
# gender
ggplot(gender_data, 
       aes(x = year, y = screentime, group = gender, color = gender)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  labs(y = "Screentime Percentage of Gender Categories")
ggsave("data/gender_trends.png", device = "png")

ggplot(gender_data,
       aes(x = year, y = screentime, group = lang, color = lang)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Screentime Percentage of Gender Categories across Languages") +
  facet_wrap(~gender)
ggsave("data/gender_x_lang_trends.png", device = "png")

# age
ggplot(age_data, 
       aes(x = year, y = screentime, group = age, color = age)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  labs(y = "Screentime Percentage of Age Categories")
ggsave("data/age_trends.png", device = "png")

ggplot(age_data,
       aes(x = year, y = screentime, group = lang, color = lang)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Screentime Percentage of Age Categories across Languages") +
  facet_wrap(~age)
ggsave("data/age_x_lang_trends.png", device = "png")

# skintone
ggplot(skintone_data, 
       aes(x = year, y = screentime, group = skintone, color = skintone)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  labs(y = "Screentime Percentage of Skintone Categories")
ggsave("data/skintone_trends.png", device = "png")

ggplot(skintone_data,
       aes(x = year, y = screentime, group = lang, color = lang)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Screentime Percentage of Skintone Categories across Languages") +
  facet_wrap(~skintone)
ggsave("data/skintone_x_lang_trends.png", device = "png")

# gender x age
ggplot(gender_x_age_data, 
       aes(x = year, y = screentime, group = age, color = age)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  labs(y = "Screentime Percentage of Age Groups Within Gender Categories") +
  facet_wrap(~gender)
ggsave("data/gender_x_age_trends.png", device = "png")

# gender x skintone
ggplot(gender_x_skintone_data, 
       aes(x = year, y = screentime, group = skintone, color = skintone)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  labs(y = "Screentime Percentage of Skintone Groups Within Gender Categories") +
  facet_wrap(~gender)
ggsave("data/gender_x_skintone_trends.png", device = "png")

# age x skintone
ggplot(age_x_skintone_data, 
       aes(x = year, y = screentime, group = skintone, color = skintone)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  labs(y = "Screentime Percentage of Skintone Groups Within Age Categories") +
  facet_wrap(~age)
ggsave("data/age_x_skintone_trends.png", device = "png")

# gender x age x skintone
ggplot(gender_x_age_x_skintone_data, 
       aes(x = year, y = screentime, group = skintone, color = skintone)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  labs(y = "Screentime Percentage of Skintone Groups Within Gender and Age Categories") +
  facet_wrap(~gender*age, nrow = 4)
ggsave("data/gender_x_age_x_skintone_trends.png", device = "png")