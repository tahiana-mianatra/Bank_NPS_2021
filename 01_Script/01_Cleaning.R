#Input : Df from the result of the survey
#Output : Cleaned df ready for analysis
#NPS: Promoter = 9 to 10, neutral 7 to 8, detractor 6 to 0
#I hope this will be seen by git
#Load library
library(readxl)
library(openxlsx)
library(tidyverse)
library(here)
#Load the data
gross_data<- read_excel(here::here("02_Input", "Base_VF.xlsx"))
# Step 1 Excel has unwritten cells, remove QUEST = N/A and QUEST = blank
gross_data <- gross_data%>%
  filter(!is.na(QUEST & QUEST != ""))
# Step 2: Removing all the 11 (don't know from the note)
survey <- gross_data %>%
  mutate(across(
    .cols = c(Q7, Q9, Q11, Q13), # Specify columns
    .fns = ~ replace(.x, .x == 11, NA)                # Replace function
  ))

#Step 3 : Separating TOM from spontaneous
aware <- survey %>%   #Isolating the MC question
  select(QUEST, Q5A, Q5B)
tom <- aware %>%
  filter(!is.na(Q5A)) %>%
  transmute(QUEST, brand = Q5A, type = "TOM")
#Check for the N/A on TOM
missing_tom <- aware %>%
  filter(is.na(Q5A) | Q5A == "") %>%
  select(QUEST, Q5A)

spont <- aware %>%
  filter(!is.na(Q5B)) %>%
  separate_rows(Q5B, sep = "-") %>%
  filter(Q5B != "" & !is.na(Q5B)) %>%
  transmute(QUEST, brand = Q5B, type = "Spontaneous")
spont <- spont %>%
  mutate(brand = as.numeric(brand))
#Check for duplicate within spontaneous (should be no data)
dup_spont <- aware %>%
  filter(!is.na(Q5B)) %>%
  separate_rows(Q5B, sep = "-") %>%
  count(QUEST, Q5B, name = "n") %>%
  filter(n > 1) %>%
  rename(brand = Q5B)
#Check for duplicate for TOM and spontaneous (should be no data)
tom_spont_conflict <- aware %>%
  filter(!is.na(Q5B)) %>%
  separate_rows(Q5B, sep = "-") %>%
  inner_join(
    aware %>% select(QUEST, TOM = Q5A),
    by = "QUEST"
  ) %>%
  filter(Q5B == TOM) %>%
  select(QUEST, brand = TOM)

#Bind the two and remove any duplicates (best practices if there is double spontaneous)
aware_long <- bind_rows(tom, spont)  
aware_long <- aware_long %>%
  distinct(QUEST, brand, type)

# Step 4: Process assisted awareness (binary columns Q6_1 to Q6_12)
assisted <- survey %>%
  select(QUEST, starts_with("Q6_")) %>%
  pivot_longer(
    cols = starts_with("Q6_"),
    names_to = "brand_code",
    values_to = "awareness"
  ) %>%
  filter(awareness == 1) %>%  # Keep only "Yes" responses
  mutate(
    brand = as.numeric(str_remove(brand_code, "Q6_")),  # Extract brand number
    type = "Assisted"
  ) %>%
  select(QUEST, brand, type)

# Combine all three types
aware_long_complete <- bind_rows(tom, spont, assisted) %>%
  distinct(QUEST, brand, type)

# Create wide format with explicit columns
# First, let's add sequence numbers within each respondent and awareness type
aware_wide <- aware_long_complete %>%
  group_by(QUEST, type) %>%
  mutate(seq_num = row_number()) %>%
  ungroup()

# Create TOM (single column)
tom_wide <- aware_wide %>%
  filter(type == "TOM") %>%
  select(QUEST, TOM = brand)

# Create spontaneous (multiple columns)
spont_wide <- aware_wide %>%
  filter(type == "Spontaneous") %>%
  select(QUEST, brand, seq_num) %>%
  pivot_wider(
    names_from = seq_num,
    values_from = brand,
    names_prefix = "Spont_",
    values_fill = NA
  )

# Create assisted (multiple columns)  
assisted_wide <- aware_wide %>%
  filter(type == "Assisted") %>%
  select(QUEST, brand, seq_num) %>%
  pivot_wider(
    names_from = seq_num,
    values_from = brand,
    names_prefix = "Assist_",
    values_fill = NA
  )

# Combine everything back to main survey data
survey_complete <- survey %>%
  left_join(tom_wide, by = "QUEST") %>%
  left_join(spont_wide, by = "QUEST") %>%
  left_join(assisted_wide, by = "QUEST")

# Quick check of the results
bb <- aware_long_complete %>%
  count(type, brand) %>%
  arrange(brand, type)

#Writing of all the cleaned result
cleaned_data <- list(
  "survey" = survey,
  "aware_long" = aware_long,
  "survey_complete" = survey_complete)

write.xlsx(cleaned_data,here::here("03_Df_output", "cleaned_data.xlsx"))

#Wrting in rdata
save(survey, aware_long,
     file = here::here("03_Df_output", "cleaned_data.RData"))
