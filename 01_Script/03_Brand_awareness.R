#Input: cleaned dataframe; aware_long
#Out: Brand awareness stacked barplot
#Load library
library(here)
library(tidyverse)
library(readxl)
library(ggimage)
library(scales)
library(magick)
library(grid)
library(ggtext)

#Load data
load(here::here("03_Df_output", "cleaned_data.RData"))
code_to_label <- read_excel(here::here("02_Input", "Code_to_label.xlsx"), sheet = "Bank_code")
#Load function
round_excel <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10^digits
  z * posneg
}
#Just replace aware_long_complete because it is too long
aware_df <-aware_long_complete
rm(aware_long_complete)
#Arrange the data
brand_see <- aware_df %>%
  # First, get the total distinct respondents
  mutate(total = n_distinct(QUEST)) %>%
  # Then count by type and brand
  count(type, brand, total, name = "n") %>%
  arrange(brand, type) %>%
  relocate(total, .after = last_col()) %>% #relocate the total column at the end
  mutate(
    percentage = n / total * 100,  # Calculate percentage
    perc_text = round_excel(percentage, 1)  # Excel-style rounded text
  )


#Coding the brand_see  
brand_see <- brand_see %>%
  left_join(code_to_label, by = c("brand" = "Code")) %>%  # Correct join syntax
  mutate(brand = Label) %>%  # Overwrite brand with Label (not Label = brand)
  select(-Label)  # Drop the Label column since we don't need it anymore
#Intergrate Total on brand_awareness
# Calculate actual reach (unique respondents per brand, regardless of type)
brand_reach <- aware_df %>%
  group_by(brand) %>%
  summarise(
    reach = n_distinct(QUEST),  # Actual unique people who mentioned this brand
    reach_percentage = reach / n_distinct(aware_df$QUEST) * 100
  ) %>%
  left_join(code_to_label, by = c("brand" = "Code")) %>%
  mutate(brand = Label) %>%
  select(-Label)

# Add Total rows with actual reach
total_rows <- brand_reach %>%
  mutate(
    type = "Total",
    n = reach,
    percentage = reach_percentage,
    perc_text =round_excel(reach_percentage),
    total_respondents = n_distinct(aware_df$QUEST)
  ) %>%
  select(type, brand, n, total_respondents, percentage, perc_text)

# Combine with original data
brand_see_with_total <- bind_rows(brand_see, total_rows) %>%
  mutate(type = factor(type, levels = c("TOM", "Spontaneous", "Assisted", "Total"))) %>%
  arrange(brand, type)
