# set up ----------------------------------------------------------------------
## load libraries ----
library(lme4)
library(lmerTest)
library(emmeans)
library(tidyverse)
library(Kendall)

## pbkrtest limit ----
emm_options(pbkrtest.limit = 11520)

## read data ----
## read long-form data (created and cleaned by Meredith@GDI)
dat <- read.csv("data/RawResults_CLEANED.csv")

## create skin tone groups ----
dat<-dat%>%
  mutate(skin_tone=case_when(
    monk=="[0, 1.1)"~"light",
    monk=="[1.1, 2.1)"~"light",
    monk=="[2.1, 3.1)"~"light",
    monk=="[3.1, 4.1)"~"medium",
    monk=="[4.1, 5.1)"~"medium",
    monk=="[5.1, 6.1)"~"medium",
    monk=="[6.1, 7.1)"~"medium",
    monk=="[7.1, 8.1)"~"dark",
    monk=="[8.1, 9.1)"~"dark",
    monk=="[9.1, 10.1)"~"dark"
  ))

## create age groups ----
dat<-dat%>%
  mutate(age=case_when(
    age=="[0, 18)"~"young",
    age=="[18, 33)"~"adult",
    age=="[33, 60)"~"middle_aged",
    age=="[60, 100)"~"old"
  ))

genders = list("female", "male")
age_groups = list("young", "adult", "middle_aged", "old")
skin_tone_groups = list("light", "medium", "dark")

# gender analysis -------------------------------------------------------------
## visual speaking time of gender groups
gender_dat <- dat %>% 
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>% 
  group_by(video_key, program, gender, year, speaking) %>% 
  summarise(n = sum(count), .groups = "drop") %>% 
  group_by(video_key, program, year, gender) %>% 
  mutate(pct = n / (sum(n) + 1e-23) * 100) %>% 
  ungroup() %>% 
  filter(speaking == "is_speaking")

## model ----
m_gender <- lmer(pct ~ year * gender + (1 | program), gender_dat)
summary(m_gender)
anova(m_gender)

## predicted means ----
## expected marginal means for gender and year
gender_year_mm_dat <- emmeans(m_gender, ~ gender | year, 
                              at = list(year = unique(gender_dat$year))) %>%
  as_tibble() %>%
  arrange(year, gender)

## macro means ----
# macro means for each year and gender
gender_year_macro_dat <- gender_dat %>% 
  group_by(year, gender) %>% 
  summarise(pct = mean(pct)) %>% 
  arrange(year, gender)

## micro means ----
# micro means for each year and gender
gender_year_micro_dat <- dat %>%
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>%
  group_by(year, gender, speaking) %>%
  summarise(n = sum(count), .groups = "drop") %>%
  group_by(year, gender) %>%
  mutate(pct = n / (sum(n) + 1e-23) * 100) %>%
  ungroup() %>%
  filter(speaking == "is_speaking") %>%
  arrange(year, gender)

## groups means ----
# macro means for each gender
gender_macro_dat <- gender_dat %>% group_by(gender) %>% 
  summarise(pct = mean(pct))

## linear trends ----
gender_trends_dat <- emtrends(m_gender, ~ gender, var = "year") %>%
  as_tibble()

## plot trends ----
gender_plot <- ggplot(gender_dat, aes(x = year, y = pct, group = gender, color = gender)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percent Speaking Within Gender Category")

## paired t-test ----
female_arr <- subset(gender_dat, gender == "female")$pct
male_arr <- subset(gender_dat, gender == "male")$pct
t.test(female_arr, male_arr, paired = TRUE)

## Mann Kendall test ----
gender_year_dat <- gender_dat %>%
  group_by(year, program, gender) %>%
  summarise(m = mean(pct), .groups = "drop") %>%
  group_by(year, gender) %>%
  summarise(med = median(m), .groups = "drop")
MannKendall(subset(gender_year_dat, gender == "female")$med)
MannKendall(subset(gender_year_dat, gender == "male")$med)

## save ----
write.table(gender_dat, "data/gender/gender.tsv", sep = "\t", row.names = FALSE)
write.table(gender_macro_dat, "data/gender/gender_macro.tsv", sep = "\t", 
            row.names = FALSE)
write.table(gender_year_mm_dat, "data/gender/gender_year_mm.tsv", sep = "\t", 
            row.names = FALSE)
write.table(gender_year_macro_dat, "data/gender/gender_year_macro.tsv", 
            sep = "\t", row.names = FALSE)
write.table(gender_year_micro_dat, "data/gender/gender_year_micro.tsv", 
            sep = "\t", row.names = FALSE)
write.table(gender_trends_dat, "data/gender/gender_trends.tsv", sep = "\t", 
            row.names = FALSE)
ggsave("plots/gender.png", gender_plot, bg = "white")

# skin tone analysis ----------------------------------------------------------
## visual speaking time of skin tone groups
monk_dat <- dat %>% 
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>% 
  group_by(video_key, program, skin_tone, year, speaking) %>% 
  summarise(n = sum(count), .groups = "drop") %>% 
  group_by(video_key, program, year, skin_tone) %>% 
  mutate(pct = n / (sum(n) + 1e-23) * 100) %>% 
  ungroup() %>% 
  filter(speaking == "is_speaking")

## model ----
m_monk <- lmer(pct ~ year * skin_tone + (1 | program), monk_dat)
summary(m_monk)
anova(m_monk, test = "LRT")

## predicted means ----
# expected marginal means for skin tone and year
monk_year_mm_dat <- emmeans(m_monk, ~ skin_tone | year, 
                            at = list(year = unique(monk_dat$year))) %>%
  as_tibble() %>%
  arrange(year, skin_tone)

## macro means ----
# macro means for each skin tone and year
monk_year_macro_dat <- monk_dat %>% 
  group_by(year, skin_tone) %>% 
  summarise(pct = mean(pct)) %>% 
  arrange(year, skin_tone)

## micro means ----
# micro means for each year and gender
monk_year_micro_dat <- dat %>%
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>%
  group_by(year, skin_tone, speaking) %>%
  summarise(n = sum(count), .groups = "drop") %>%
  group_by(year, skin_tone) %>%
  mutate(pct = n / (sum(n) + 1e-23) * 100) %>%
  ungroup() %>%
  filter(speaking == "is_speaking") %>%
  arrange(year, skin_tone)

# macro means for each gender
monk_macro_dat <- monk_dat %>% group_by(skin_tone) %>% 
  summarise(pct = mean(pct))

# marginal linear trends
emtrends(m_monk, ~ skin_tone, var = "year")

# plot marginal linear trends
ggplot(monk_dat, aes(x = year, y = pct, group = skin_tone, color = skin_tone)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percent Speaking Within Skin Tone Category")

## multiple pairwise t-test
pairwise.t.test(monk_dat$pct, monk_dat$skin_tone, 
                p.adjust.method = "bonferroni", paired = TRUE)

## Mann Kendall test
monk_year_dat <- monk_dat %>%
  group_by(year, program, skin_tone) %>%
  summarise(m = mean(pct), .groups = "drop") %>%
  group_by(year, skin_tone) %>%
  summarise(med = median(m), .groups = "drop")
MannKendall(subset(monk_year_dat, skin_tone == "light")$med)
MannKendall(subset(monk_year_dat, skin_tone == "medium")$med)
MannKendall(subset(monk_year_dat, skin_tone == "dark")$med)

## save
write.table(monk_dat, "data/skin_tone/monk.tsv", sep = "\t", row.names = FALSE)
write.table(monk_macro_dat, "data/skin_tone/monk_macro.tsv", sep = "\t", 
            row.names = FALSE)
write.table(monk_year_mm_dat, "data/skin_tone/monk_year_mm.tsv", sep = "\t", 
            row.names = FALSE)
write.table(monk_year_macro_dat, "data/skin_tone/monk_year_macro.tsv", sep = "\t", 
            row.names = FALSE)
write.table(monk_year_micro_dat, "data/skin_tone/monk_year_micro.tsv", sep = "\t", 
            row.names = FALSE)

# age analysis ----------------------------------------------------------------
## visual speaking time of age groups
age_dat <- dat %>% 
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>% 
  group_by(video_key, program, age, year, speaking) %>% 
  summarise(n = sum(count), .groups = "drop") %>% 
  group_by(video_key, program, year, age) %>% 
  mutate(pct = n / (sum(n) + 1e-23) * 100) %>% 
  ungroup() %>% 
  filter(speaking == "is_speaking")

# model
m_age <- lmer(pct ~ year * age + (1 | program), age_dat)
summary(m_age)
anova(m_age, test = "LRT")

# expected marginal means for age and year
age_year_mm_dat <- emmeans(m_age, ~ age | year, 
                           at = list(year = unique(age_dat$year))) %>%
  as_tibble() %>%
  arrange(year, age)

# macro means for each year and age
age_year_macro_dat <- age_dat %>% 
  group_by(year, age) %>% 
  summarise(pct = mean(pct)) %>% 
  arrange(year, age)

# micro means for each year and age
age_year_micro_dat <- dat %>%
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>%
  group_by(year, age, speaking) %>%
  summarise(n = sum(count), .groups = "drop") %>%
  group_by(year, age) %>%
  mutate(pct = n / (sum(n) + 1e-23) * 100) %>%
  ungroup() %>%
  filter(speaking == "is_speaking") %>%
  arrange(year, age)

# macro means for each age
age_macro_dat <- age_dat %>% group_by(age) %>% summarise(pct = mean(pct))

# marginal linear trends
emtrends(m_age, ~ age, var = "year")

# plot marginal linear trends
ggplot(age_dat, aes(x = year, y = pct, group = age, color = age)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percent Speaking Within Age Category")

## multiple pairwise t-test
pairwise.t.test(age_dat$pct, age_dat$age, p.adjust.method = "bonferroni", 
                paired = TRUE)

## Mann Kendall test
age_year_dat <- age_dat %>%
  group_by(year, program, age) %>%
  summarise(m = mean(pct), .groups = "drop") %>%
  group_by(year, age) %>%
  summarise(med = median(m), .groups = "drop")
MannKendall(subset(age_year_dat, age == "young")$med)
MannKendall(subset(age_year_dat, age == "adult")$med)
MannKendall(subset(age_year_dat, age == "middle_aged")$med)
MannKendall(subset(age_year_dat, age == "old")$med)

## save
write.table(age_dat, "data/age/age.tsv", sep = "\t", row.names = FALSE)
write.table(age_macro_dat, "data/age/age_macro.tsv", sep = "\t", 
            row.names = FALSE)
write.table(age_year_mm_dat, "data/age/age_year_mm.tsv", sep = "\t", 
            row.names = FALSE)
write.table(age_year_macro_dat, "data/age/age_year_macro.tsv", sep = "\t", 
            row.names = FALSE)
write.table(age_year_micro_dat, "data/age/age_year_micro.tsv", sep = "\t", 
            row.names = FALSE)

# gender x age analysis -------------------------------------------------------
## visual speaking time of gender x age groups
gender_x_age_dat <- dat %>%
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>%
  group_by(video_key, program, year, gender, age, speaking) %>%
  summarise(n = sum(count), .groups = "drop") %>%
  group_by(video_key, program, year, gender, age) %>%
  mutate(pct = n / (sum(n) + 1e-23) * 100) %>%
  ungroup() %>%
  filter(speaking == "is_speaking")

# model
m_gender_x_age <- lmer(pct ~ year * gender * age + (1|program), 
                       gender_x_age_dat)
summary(m_gender_x_age)
anova(m_gender_x_age, test = "LRT")

# expected marginal means for gender, age, and year
gender_x_age_year_mm_dat <- emmeans(
  m_gender_x_age, ~ age + gender | year, 
  at = list(year = unique(gender_x_age_dat$year))) %>%
  as_tibble() %>%
  arrange(age, year)

# macro means for gender, age, and year
gender_x_age_year_macro_dat <- gender_x_age_dat %>% 
  group_by(year, gender, age) %>% 
  summarise(pct = mean(pct)) %>% 
  arrange(year, gender, age)

# micro means for gender, age, and year
gender_x_age_year_micro_dat <- dat %>%
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>%
  group_by(year, gender, age, speaking) %>%
  summarise(n = sum(count), .groups = "drop") %>%
  group_by(year, gender, age) %>%
  mutate(pct = n / (sum(n) + 1e-23) * 100) %>%
  ungroup() %>%
  filter(speaking == "is_speaking") %>%
  arrange(year, gender, age)

# macro means for each gender and age
gender_x_age_macro_dat <- gender_x_age_dat %>% group_by(gender, age) %>% 
  summarise(pct = mean(pct))

# marginal linear trends
emtrends(m_gender_x_age, ~ age + gender, var = "year")

# plot marginal means
ggplot(gender_x_age_dat, aes(x = year, y = pct, group = age, color = age)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() +
  labs(y = "Percent Speaking Within Gender x Age Category") +
  facet_wrap(~gender)

# multiple pairwise t-tests by gender
female_x_age_dat <- filter(gender_x_age_dat, gender == "female")
male_x_age_dat <- filter(gender_x_age_dat, gender == "male")
pairwise.t.test(female_x_age_dat$pct, female_x_age_dat$age, 
                p.adjust.method = "bonferroni", paired = TRUE)
pairwise.t.test(male_x_age_dat$pct, male_x_age_dat$age, 
                p.adjust.method = "bonferroni", paired = TRUE)
pairwise.t.test(gender_x_age_dat$pct,
                paste(gender_x_age_dat$age, gender_x_age_dat$gender), 
                p.adjust.method = "bonferroni", paired = TRUE)

# Mann Kendall test
gender_x_age_year_dat <- gender_x_age_dat %>%
  group_by(year, program, gender, age) %>%
  summarise(m = mean(pct), .groups = "drop") %>%
  group_by(year, gender, age) %>%
  summarise(med = median(m), .groups = "drop")

for (G in genders) {
  for (A in age_groups) {
    mk <- MannKendall(subset(gender_x_age_year_dat, 
                             (gender == G) & (age == A))$med)
    cat(A, G, "\n")
    print(mk)
  }
}

## save
write.table(gender_x_age_dat, "data/gender_x_age/gender_x_age.tsv", sep = "\t", 
            row.names = FALSE)
write.table(gender_x_age_macro_dat, "data/gender_x_age/gender_x_age_macro.tsv", 
            sep = "\t", row.names = FALSE)
write.table(gender_x_age_year_mm_dat, "data/gender_x_age/gender_x_age_year_mm.tsv", 
            sep = "\t", row.names = FALSE)
write.table(gender_x_age_year_macro_dat, "data/gender_x_age/gender_x_age_year_macro.tsv", 
            sep = "\t", row.names = FALSE)
write.table(gender_x_age_year_micro_dat, "data/gender_x_age/gender_x_age_year_micro.tsv", 
            sep = "\t", row.names = FALSE)

# gender x skin tone analysis -------------------------------------------------
## visual speaking time of gender x skin tone groups
gender_x_monk_dat <- dat %>%
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>%
  group_by(video_key, program, year, gender, skin_tone, speaking) %>%
  summarise(n = sum(count), .groups = "drop") %>%
  group_by(video_key, program, year, gender, skin_tone) %>%
  mutate(pct = n / (sum(n) + 1e-23) * 100) %>%
  ungroup() %>%
  filter(speaking == "is_speaking")

# model
m_gender_x_monk <- lmer(pct ~ year * gender * skin_tone + (1|program), 
                        gender_x_monk_dat)
summary(m_gender_x_monk)
anova(m_gender_x_monk, test = "LRT")

# expected marginal means for gender, skin tone, and year
gender_x_monk_year_mm_dat <- emmeans(
  m_gender_x_monk, ~ skin_tone + gender | year, 
  at = list(year = unique(gender_x_monk_dat$year))) %>%
  as_tibble() %>%
  arrange(year, gender, skin_tone)

# macro means for gender, skin tone, and year
gender_x_monk_year_macro_dat <- gender_x_monk_dat %>% 
  group_by(year, gender, skin_tone) %>% 
  summarise(pct = mean(pct)) %>% 
  arrange(year, gender, skin_tone)

# micro means for gender, skin tone, and year
gender_x_monk_year_micro_dat <- dat %>%
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>%
  group_by(year, gender, skin_tone, speaking) %>%
  summarise(n = sum(count), .groups = "drop") %>%
  group_by(year, gender, skin_tone) %>%
  mutate(pct = n / (sum(n) + 1e-23) * 100) %>%
  ungroup() %>%
  filter(speaking == "is_speaking") %>%
  arrange(year, gender, skin_tone)

# macro means for each gender and skin tone
gender_x_monk_macro_dat <- gender_x_monk_dat %>% 
  group_by(gender, skin_tone) %>% 
  summarise(pct = mean(pct))

# marginal linear trends
emtrends(m_gender_x_monk, ~ gender + skin_tone, var = "year")

# marginal means for each year
m_gender_x_monk_mm <- emmeans(
  m_gender_x_monk, ~ skin_tone + gender | year, 
  at = list(year = unique(gender_x_monk_dat$year))) %>%
  as_tibble() %>%
  arrange(skin_tone, year)

# plot marginal means
ggplot(gender_x_monk_dat, 
       aes(x = year, y = pct, group = skin_tone, color = skin_tone)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() +
  labs(y = "Percent Speaking Within Gender x Skin Tone Category") +
  facet_wrap(~gender)

# multiple pairwise t-tests by gender
female_x_monk_dat <- filter(gender_x_monk_dat, gender == "female")
male_x_monk_dat <- filter(gender_x_monk_dat, gender == "male")
pairwise.t.test(female_x_monk_dat$pct, female_x_monk_dat$skin_tone, 
                p.adjust.method = "bonferroni", paired = TRUE)
pairwise.t.test(male_x_monk_dat$pct, male_x_monk_dat$skin_tone, 
                p.adjust.method = "bonferroni", paired = TRUE)
pairwise.t.test(gender_x_monk_dat$pct, 
                paste(gender_x_monk_dat$skin_tone, gender_x_monk_dat$gender), 
                p.adjust.method = "bonferroni", paired = TRUE)

# Mann Kendall test
gender_x_monk_year_dat <- gender_x_monk_dat %>%
  group_by(year, program, gender, skin_tone) %>%
  summarise(m = mean(pct), .groups = "drop") %>%
  group_by(year, gender, skin_tone) %>%
  summarise(med = median(m), .groups = "drop")

for (G in genders) {
  for (S in skin_tone_groups) {
    mk <- MannKendall(subset(gender_x_monk_year_dat, 
                             (gender == G) & (skin_tone == S))$med)
    cat(S, G, "\n")
    print(mk)
  }
}

## save
write.table(gender_x_monk_dat, "data/gender_x_skin_tone/gender_x_monk.tsv", sep = "\t", 
            row.names = FALSE)
write.table(gender_x_monk_macro_dat, "data/gender_x_skin_tone/gender_x_monk_macro.tsv", 
            sep = "\t", row.names = FALSE)
write.table(gender_x_monk_year_mm_dat, "data/gender_x_skin_tone/gender_x_monk_year_mm.tsv", 
            sep = "\t", row.names = FALSE)
write.table(gender_x_monk_year_macro_dat, "data/gender_x_skin_tone/gender_x_monk_year_macro.tsv", 
            sep = "\t", row.names = FALSE)
write.table(gender_x_monk_year_micro_dat, "data/gender_x_skin_tone/gender_x_monk_year_micro.tsv", 
            sep = "\t", row.names = FALSE)

# age x skin tone analysis ----------------------------------------------------
## visual speaking time of age x skin tone groups
age_x_monk_dat <- dat %>%
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>%
  group_by(video_key, program, year, age, skin_tone, speaking) %>%
  summarise(n = sum(count), .groups = "drop") %>%
  group_by(video_key, program, year, age, skin_tone) %>%
  mutate(pct = n / (sum(n) + 1e-23) * 100) %>%
  ungroup() %>%
  filter(speaking == "is_speaking")

# model
m_age_x_monk <- lmer(pct ~ year * age * skin_tone + (1|program), age_x_monk_dat)
summary(m_age_x_monk)
anova(m_age_x_monk, test = "LRT")

# expected marginal means for age, skin tone, and year
age_x_monk_year_mm_dat <- emmeans(
  m_age_x_monk, ~ skin_tone + age | year, 
  at = list(year = unique(age_x_monk_dat$year))) %>%
  as_tibble() %>%
  arrange(year, age, skin_tone)

# macro means for age, skin tone, and year
age_x_monk_year_macro_dat <- age_x_monk_dat %>% 
  group_by(year, age, skin_tone) %>% 
  summarise(pct = mean(pct)) %>% 
  arrange(year, age, skin_tone)

# micro means for age, skin tone, and year
age_x_monk_year_micro_dat <- dat %>%
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>%
  group_by(year, age, skin_tone, speaking) %>%
  summarise(n = sum(count), .groups = "drop") %>%
  group_by(year, age, skin_tone) %>%
  mutate(pct = n / (sum(n) + 1e-23) * 100) %>%
  ungroup() %>%
  filter(speaking == "is_speaking") %>%
  arrange(year, age, skin_tone)

# macro means for each age and skin tone
age_x_monk_macro_dat <- age_x_monk_dat %>% 
  group_by(age, skin_tone) %>% 
  summarise(pct = mean(pct))

# marginal linear trends
emtrends(m_age_x_monk, ~ skin_tone + age, var = "year")

# plot marginal means
ggplot(age_x_monk_dat, 
       aes(x = year, y = pct, group = skin_tone, color = skin_tone)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() +
  labs(y = "Percent Speaking Within Age x Skin Tone Category") +
  facet_wrap(~age)

# multiple pairwise t-tests
young_x_monk_dat <- subset(age_x_monk_dat, age == "young")
pairwise.t.test(young_x_monk_dat$pct, young_x_monk_dat$skin_tone, 
                p.adjust.method = "bonferroni", paired = TRUE)
adult_x_monk_dat <- subset(age_x_monk_dat, age == "adult")
pairwise.t.test(adult_x_monk_dat$pct, adult_x_monk_dat$skin_tone, 
                p.adjust.method = "bonferroni", paired = TRUE)
middle_aged_x_monk_dat <- subset(age_x_monk_dat, age == "middle_aged")
pairwise.t.test(middle_aged_x_monk_dat$pct, middle_aged_x_monk_dat$skin_tone, 
                p.adjust.method = "bonferroni", paired = TRUE)
old_x_monk_dat <- subset(age_x_monk_dat, age == "old")
pairwise.t.test(old_x_monk_dat$pct, old_x_monk_dat$skin_tone, 
                p.adjust.method = "bonferroni", paired = TRUE)
pairwise.t.test(age_x_monk_dat$pct, 
                paste(age_x_monk_dat$skin_tone, age_x_monk_dat$age), 
                p.adjust.method = "bonferroni", paired = TRUE)

# Mann Kendall test
age_x_monk_year_dat <- age_x_monk_dat %>%
  group_by(year, program, age, skin_tone) %>%
  summarise(m = mean(pct), .groups = "drop") %>%
  group_by(year, age, skin_tone) %>%
  summarise(med = median(m), .groups = "drop")

for (A in age_groups) {
  for (S in skin_tone_groups) {
    mk <- MannKendall(subset(age_x_monk_year_dat, 
                             (age == A) & (skin_tone == S))$med)
    cat(A, S, "\n")
    print(mk)
  }
}

## save
write.table(age_x_monk_dat, "data/age_x_skin_tone/age_x_monk.tsv", sep = "\t", 
            row.names = FALSE)
write.table(age_x_monk_macro_dat, "data/age_x_skin_tone/age_x_monk_macro.tsv", 
            sep = "\t", row.names = FALSE)
write.table(age_x_monk_year_mm_dat, "data/age_x_skin_tone/age_x_monk_year_mm.tsv", 
            sep = "\t", row.names = FALSE)
write.table(age_x_monk_year_macro_dat, "data/age_x_skin_tone/age_x_monk_year_macro.tsv", 
            sep = "\t", row.names = FALSE)
write.table(age_x_monk_year_micro_dat, "data/age_x_skin_tone/age_x_monk_year_micro.tsv", 
            sep = "\t", row.names = FALSE)

# gender x age x skin tone analysis ------------------------------------------
## visual speaking time of gender x age x skin tone groups
gender_x_age_x_monk_dat <- dat %>%
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>%
  group_by(video_key, program, year, gender, age, skin_tone, speaking) %>%
  summarise(n = sum(count), .groups = "drop") %>%
  group_by(video_key, program, year, gender, age, skin_tone) %>%
  mutate(pct = n / (sum(n) + 1e-23) * 100) %>%
  ungroup() %>%
  filter(speaking == "is_speaking")

# model
m_gender_x_age_x_monk <- lmer(pct ~ year * gender * age * skin_tone + 
                                (1|program), gender_x_age_x_monk_dat)
summary(m_gender_x_age_x_monk)
anova(m_gender_x_age_x_monk, test = "LRT")

# expected marginal means for gender, age, skin tone, and year
gender_x_age_x_monk_year_mm_dat <- emmeans(
  m_gender_x_age_x_monk, ~ skin_tone + age + gender | year, 
  at = list(year = unique(gender_x_age_x_monk_dat$year))) %>%
  as_tibble() %>%
  arrange(year, gender, age, skin_tone)

# macro means for gender, age, skin tone, and year
gender_x_age_x_monk_year_macro_dat <- gender_x_age_x_monk_dat %>% 
  group_by(year, gender, age, skin_tone) %>% 
  summarise(pct = mean(pct)) %>% 
  arrange(year, gender, age, skin_tone)

# micro means for gender, age, skin tone, and year
gender_x_age_x_monk_year_micro_dat <- dat %>%
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>%
  group_by(year, gender, age, skin_tone, speaking) %>%
  summarise(n = sum(count), .groups = "drop") %>%
  group_by(year, gender, age, skin_tone) %>%
  mutate(pct = n / (sum(n) + 1e-23) * 100) %>%
  ungroup() %>%
  filter(speaking == "is_speaking") %>%
  arrange(year, gender, age, skin_tone)

# macro means for gender, age and skin tone
gender_x_age_x_monk_macro_dat <- gender_x_age_x_monk_dat %>% 
  group_by(gender, age, skin_tone) %>% 
  summarise(pct = mean(pct))

# marginal linear trends
emtrends(m_gender_x_age_x_monk, ~ skin_tone + age + gender, var = "year")

# plot marginal trends
ggplot(gender_x_age_x_monk_dat, 
       aes(x = year, y = pct, group = skin_tone, color = skin_tone)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(y = "Percent Speaking Within Gender x Age x Skin Tone Category") +
  facet_wrap(~gender*age)

# multiple pairwise t-tests by gender
female_x_age_x_monk_dat <- filter(gender_x_age_x_monk_dat, gender == "female")
male_x_age_x_monk_dat <- filter(gender_x_age_x_monk_dat, gender == "male")
pairwise.t.test(
  female_x_age_x_monk_dat$pct, 
  paste(female_x_age_x_monk_dat$age, female_x_age_x_monk_dat$skin_tone), 
  p.adjust.method = "bonferroni", paired = TRUE)
pairwise.t.test(
  male_x_age_x_monk_dat$pct, 
  paste(male_x_age_x_monk_dat$age, male_x_age_x_monk_dat$skin_tone), 
  p.adjust.method = "bonferroni", paired = TRUE)

# Mann Kendall test
gender_x_age_x_monk_year_dat <- gender_x_age_x_monk_dat %>%
  group_by(year, program, gender, age, skin_tone) %>%
  summarise(m = mean(pct), .groups = "drop") %>%
  group_by(year, gender, age, skin_tone) %>%
  summarise(med = median(m), .groups = "drop")

for (G in genders) {
  for (A in age_groups) {
    for (S in skin_tone_groups) {
      mk <- MannKendall(
        subset(gender_x_age_x_monk_year_dat, 
               (gender == G) & (age == A) & (skin_tone == S))$med)
      cat(G, A, S, "\n")
      print(mk)
    }
  }
}

## save
write.table(gender_x_age_x_monk_dat, "data/gender_x_age_x_skin_tone/gender_x_age_x_monk.tsv", sep = "\t", 
            row.names = FALSE)
write.table(gender_x_age_x_monk_macro_dat, "data/gender_x_age_x_skin_tone/gender_x_age_x_monk_macro.tsv", 
            sep = "\t", row.names = FALSE)
write.table(gender_x_age_x_monk_year_mm_dat, 
            "data/gender_x_age_x_skin_tone/gender_x_age_x_monk_year_mm.tsv", sep = "\t", 
            row.names = FALSE)
write.table(gender_x_age_x_monk_year_macro_dat, 
            "data/gender_x_age_x_skin_tone/gender_x_age_x_monk_year_macro.tsv", sep = "\t", 
            row.names = FALSE)
write.table(gender_x_age_x_monk_year_micro_dat, 
            "data/gender_x_age_x_skin_tone/gender_x_age_x_monk_year_micro.tsv", sep = "\t", 
            row.names = FALSE)