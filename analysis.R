# import libraries
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(scales)
library(rcompanion)

# import dataset
df <- read.csv("fatal-police-shootings-data.csv")

df <- df[(df$gender == "male" | df$gender == "female"), ]

age_df <- df[!is.na(df$age), c("gender", "age")]

## filter each gender data
male_df <- age_df |>
  filter(gender == "male") |>
  group_by(age) |>
  count(name = "count")

female_df <- age_df |>
  filter(gender == "female") |>
  group_by(age) |>
  count(name = "count")


## bar chart of the distribution of age for male and female victims
p_male <- ggplot(male_df, aes(x = age, y = count)) + 
  geom_col(
    color = "deepskyblue4",
    fill = "lightskyblue"
  ) + 
  labs(
    x = "Age",
    y = "Number of male victims",
  ) +
  theme_classic()

p_female <- ggplot(female_df, aes(x = age, y = count)) + 
  geom_col(
    color = "deeppink3",
    fill = "pink"
  ) + 
  labs(
    x = "Age",
    y = "Number of female victims",
  ) +
  theme_classic()

grid.arrange(p_male, p_female, ncol=2)


police.data <- read.csv("fatal-police-shootings-data.csv")

processed_police.data <- police.data %>%
  select(armed_with, race) %>%
  separate_rows(armed_with, sep=";") %>% 
  mutate(armed_with = case_when(
    armed_with == "" ~ "Unlisted",
    armed_with == "blunt_object" ~ "Melee Weapon",
    armed_with == "gun" ~ "Gun",
    armed_with == "knife" ~ "Melee Weapon",
    armed_with == "other" ~ "Other",
    armed_with == "replica" ~ "Replica",
    armed_with == "unarmed" ~ "Unarmed",
    armed_with == "undetermined" ~ "Unknown",
    armed_with == "unknown" ~ "Unknown",
    armed_with == "vehicle" ~ "Vehicle"
  )) %>%
  filter(armed_with != "Unlisted") %>% 
  separate_rows(race, sep=";") %>% 
  mutate(race = case_when(
    race == "" ~ "Unlisted",
    race == "A" ~ "Asian",
    race == "B" ~ "Black",
    race == "H" ~ "Hispanic",
    race == "N" ~ "None",
    race == "O" ~ "Other",
    race == "W" ~ "White"
  ))

freq_table_armed_cam <- processed_police.data %>% 
  group_by(armed_with, race) %>%
  count() %>%
  ungroup() %>%
  spread(armed_with, n)

chisq_analysis <- chisq.test(processed_police.data$armed_with,
                             processed_police.data$race,
                             simulate.p.value = TRUE)

observed = data.frame(chisq_analysis$observed)
expected = data.frame(chisq_analysis$expected)

observed_black <- observed[observed$processed_police.data.race
                           == "Black", ]
observed_white <- observed[observed$processed_police.data.race
                           == "White", ]

diff_black <- label_percent()(((
  observed_black$Freq -   
    expected$Black) /
    expected$Black))

diff_white <- label_percent()(((
  observed_white$Freq -   
    expected$White) /
    expected$White))

compare_df <- data.frame(observed_black$Var1, expected$Black, 
                         observed_black$Freq, diff_black,
                         expected$White, observed_white$Freq, 
                         diff_white)

compare_black <- 
  data.frame(observed_black$Var1, expected$Black, observed_black$Freq)
compare_black$diff <- label_percent()(((
  compare_black$observed_black.Freq -   
    compare_black$expected.Black) /
    compare_black$expected.Black))

compare_white <- 
  data.frame(observed_white$Var1, expected$White, observed_white$Freq)
compare_white$diff <- label_percent()(((
  compare_white$observed_white.Freq -   
    compare_white$expected.White) /
    compare_white$expected.White))

kable(compare_df, col.names=c("Weapon", "Expected (Black)", "Actual (Black)", "Difference (Black)", "Expected (White)", "Actual (White)", "Difference (White)"), caption="Expected Frequency vs. Actual Frequency for Black and White Victims")



data <- read.csv("fatal-police-shootings-data.csv")

# boxplot of the distribution of age for each gender
data %>% ggplot() +
  geom_boxplot(aes(x = gender, y = age))


