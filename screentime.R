# load libraries ----
library(lme4)
library(lmerTest)
library(emmeans)
library(tidyverse)
library(Kendall)

emm_options(pbkrtest.limit = 30000, lmertest.limit = 30000)

# read data ----
## long form ----
data <- read.csv("data/long_video.csv")

## gender ----
gender_data <- data %>%
  group_by(file_key, program, lang, genre, year, gender) %>% 
  summarise(n = sum(faces), .groups = "drop") %>% 
  group_by(file_key, program, lang, genre, year) %>% 
  mutate(screentime = n / (sum(n) + 1e-23) * 100) %>% 
  ungroup()

## age ----
age_data <- data %>%
  group_by(file_key, program, lang, genre, year, age) %>% 
  summarise(n = sum(faces), .groups = "drop") %>% 
  group_by(file_key, program, lang, genre, year) %>% 
  mutate(screentime = n / (sum(n) + 1e-23) * 100) %>% 
  ungroup()

## skintone ----
skintone_data <- data %>%
  group_by(file_key, program, lang, genre, year, skintone) %>% 
  summarise(n = sum(faces), .groups = "drop") %>% 
  group_by(file_key, program, lang, genre, year) %>% 
  mutate(screentime = n / (sum(n) + 1e-23) * 100) %>% 
  ungroup()

## gender x age ----
gender_x_age_data <- data %>%
  group_by(file_key, program, lang, genre, year, gender, age) %>% 
  summarise(n = sum(faces), .groups = "drop") %>% 
  group_by(file_key, program, lang, genre, year) %>% 
  mutate(screentime = n / (sum(n) + 1e-23) * 100) %>% 
  ungroup()

## gender x skintone ----
gender_x_skintone_data <- data %>%
  group_by(file_key, program, lang, genre, year, gender, skintone) %>% 
  summarise(n = sum(faces), .groups = "drop") %>% 
  group_by(file_key, program, lang, genre, year) %>% 
  mutate(screentime = n / (sum(n) + 1e-23) * 100) %>% 
  ungroup()

## age x skintone ----
age_x_skintone_data <- data %>%
  group_by(file_key, program, lang, genre, year, age, skintone) %>% 
  summarise(n = sum(faces), .groups = "drop") %>% 
  group_by(file_key, program, lang, genre, year) %>% 
  mutate(screentime = n / (sum(n) + 1e-23) * 100) %>% 
  ungroup()

## gender x age x skintone ----
gender_x_age_x_skintone_data <- data %>%
  group_by(file_key, program, lang, genre, year, gender, age, skintone) %>% 
  summarise(n = sum(faces), .groups = "drop") %>% 
  group_by(file_key, program, lang, genre, year) %>% 
  mutate(screentime = n / (sum(n) + 1e-23) * 100) %>% 
  ungroup()

# models ----
## gender ----
print("screentime ~ gender * year + (1|program:gender) + (1|lang:gender) + (1|genre:gender)")
m_gender <- lmer(screentime ~ gender * year 
                 + (1|program:gender) 
                 + (1|lang:gender) 
                 + (1|genre:gender), gender_data)
gender_trends <- emtrends(m_gender, ~gender, var="year")

### gender x lang ----
print("screentime ~ gender * lang * year + (1|program:gender) + (1|genre:gender)")
m_gender_x_lang <- lmer(screentime ~ gender * lang * year
                      + (1|program:gender)
                      + (1|genre:gender), gender_data)
gender_x_lang_trends <- emtrends(m_gender_x_lang, ~gender + lang, var = "year") %>% 
  as_tibble() %>% arrange(gender, lang)

## age ----
print("screentime ~ age * year + (1|program:age) + (1|lang:age) + (1|genre:age)")
m_age <- lmer(screentime ~ age * year 
              + (1|program:age) 
              + (1|lang:age) 
              + (1|genre:age), age_data)
age_trends <- emtrends(m_age, ~age, var="year")

### age x lang ----
print("screentime ~ age * lang * year + (1|program:age) + (1|genre:age)")
m_age_x_lang <- lmer(screentime ~ age * lang * year
                      + (1|program:age)
                      + (1|genre:age), age_data)
age_x_lang_trends <- emtrends(m_age_x_lang, ~age + lang, var = "year") %>% 
  as_tibble() %>% arrange(age, lang)

## skintone ----
print("screentime ~ skintone * year + (1|program:skintone) + (1|lang:skintone) + (1|genre:skintone)")
m_skintone <- lmer(screentime ~ skintone * year 
                   + (1|program:skintone) 
                   + (1|lang:skintone) 
                   + (1|genre:skintone), skintone_data)
skintone_trends <- emtrends(m_skintone, ~skintone, var="year")

### skintone x lang ----
print("screentime ~ skintone * lang * year + (1|program:skintone) + (1|genre:skintone)")
m_skintone_x_lang <- lmer(screentime ~ skintone * lang * year
                      + (1|program:skintone)
                      + (1|genre:skintone), skintone_data)
skintone_x_lang_trends <- emtrends(m_skintone_x_lang, ~skintone + lang, 
                                 var = "year") %>% 
  as_tibble() %>% arrange(skintone, lang)

## gender x age ----
print("screentime ~ gender * age * year + (1|program:gender:age) + (1|lang:gender:age) + (1|genre:gender:age)")
m_gender_x_age <- lmer(screentime ~ gender * age * year 
                       + (1|program:gender:age) 
                       + (1|lang:gender:age) 
                       + (1|genre:gender:age), 
                       gender_x_age_data)
gender_x_age_trends <- emtrends(m_gender_x_age, ~ age + gender, var="year") %>% 
  as_tibble() %>% arrange(age, gender)

## gender x skintone ----
print("screentime ~ gender * skintone * year + (1|program:gender:skintone) + (1|lang:gender:skintone) + (1|genre:gender:skintone)")
m_gender_x_skintone <- lmer(screentime ~ gender * skintone * year 
                            + (1|program:gender:skintone) 
                            + (1|lang:gender:skintone) 
                            + (1|genre:gender:skintone), gender_x_skintone_data)
gender_x_skintone_trends <- emtrends(m_gender_x_skintone, ~ skintone + gender, 
                                     var="year") %>% 
  as_tibble() %>% arrange(skintone, gender)

## age x skintone ----
print("screentime ~ age * skintone * year + (1|program:age:skintone) + (1|lang:age:skintone) + (1|genre:age:skintone)")
m_age_x_skintone <- lmer(screentime ~ age * skintone * year 
                            + (1|program:age:skintone) 
                            + (1|lang:age:skintone) 
                            + (1|genre:age:skintone), age_x_skintone_data)
age_x_skintone_trends <- emtrends(m_age_x_skintone, ~ age + skintone, 
                                     var="year") %>% 
  as_tibble() %>% arrange(age, skintone)

## gender x age x skintone ----
print("screentime ~ gender * age * skintone * year + (1|program:gender:age:skintone) + (1|lang:gender:age:skintone) + (1|genre:gender:age:skintone)")
m_gender_x_age_x_skintone <- lmer(screentime ~ gender * age * skintone * year
                                  + (1|program:gender:age:skintone)
                                  + (1|lang:gender:age:skintone)
                                  + (1|genre:gender:age:skintone),
                                  gender_x_age_x_skintone_data)
gender_x_age_x_skintone_trends <- emtrends(m_gender_x_age_x_skintone,
                                         ~ skintone + age + gender,
                                         var="year") %>%
  as_tibble() %>% arrange(skintone, age, gender)

# plots ----
## gender ----
ggplot(gender_data, 
       aes(x = year, y = screentime, group = gender, color = gender)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  labs(y = "Screentime Percentage of Gender Categories")
ggsave("data/plots/gender_trends.eps", device = "eps")

### gender x lang ----
ggplot(gender_data,
       aes(x = year, y = screentime, group = lang, color = lang)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Screentime Percentage of Gender Categories across Languages") +
  facet_wrap(~gender)
ggsave("data/plots/gender_x_lang_trends.eps", device = "eps")

## age ----
ggplot(age_data, 
       aes(x = year, y = screentime, group = age, color = age)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  labs(y = "Screentime Percentage of Age Categories")
ggsave("data/plots/age_trends.eps", device = "eps")

### age x lang ----
ggplot(age_data,
       aes(x = year, y = screentime, group = lang, color = lang)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Screentime Percentage of Age Categories across Languages") +
  facet_wrap(~age)
ggsave("data/plots/age_x_lang_trends.eps", device = "eps")

## skintone ----
ggplot(skintone_data, 
       aes(x = year, y = screentime, group = skintone, color = skintone)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  labs(y = "Screentime Percentage of Skintone Categories")
ggsave("data/plots/skintone_trends.eps", device = "eps")

### skintone x lang ----
ggplot(skintone_data,
       aes(x = year, y = screentime, group = lang, color = lang)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Screentime Percentage of Skintone Categories across Languages") +
  facet_wrap(~skintone)
ggsave("data/plots/skintone_x_lang_trends.eps", device = "eps")

## gender x age ----
ggplot(gender_x_age_data, 
       aes(x = year, y = screentime, group = age, color = age)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  labs(y = "Screentime Percentage of Age Groups Within Gender Categories") +
  facet_wrap(~gender)
ggsave("data/plots/gender_x_age_trends.eps", device = "eps")

## gender x skintone ----
ggplot(gender_x_skintone_data, 
       aes(x = year, y = screentime, group = skintone, color = skintone)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  labs(y = "Screentime Percentage of Skintone Groups Within Gender Categories") +
  facet_wrap(~gender)
ggsave("data/plots/gender_x_skintone_trends.eps", device = "eps")

## age x skintone ----
ggplot(age_x_skintone_data, 
       aes(x = year, y = screentime, group = skintone, color = skintone)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  labs(y = "Screentime Percentage of Skintone Groups Within Age Categories") +
  facet_wrap(~age)
ggsave("data/plots/age_x_skintone_trends.eps", device = "eps")

## gender x age x skintone ----
ggplot(gender_x_age_x_skintone_data, 
       aes(x = year, y = screentime, group = skintone, color = skintone)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() + 
  labs(y = "Screentime Percentage of Skintone Groups Within Gender and Age Categories") +
  facet_wrap(~gender*age, nrow = 4)
ggsave("data/plots/gender_x_age_x_skintone_trends.eps", device = "eps")