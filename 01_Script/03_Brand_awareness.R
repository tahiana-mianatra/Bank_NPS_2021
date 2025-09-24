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
    total_percentage = reach / n_distinct(aware_df$QUEST) * 100,
    total_perc_text = round_excel(total_percentage)
  ) %>%
  left_join(code_to_label, by = c("brand" = "Code")) %>%
  mutate(brand = Label) %>%
  select(-Label)

brand_see <- brand_see %>%
  left_join(brand_reach %>%
              select(brand, total_percentage, total_perc_text),
            by = "brand")
#Arranging the order of type of awareness
brand_see <- brand_see %>%
  mutate(type = factor(type,
                       levels = c("Assisted","Spontaneous", "TOM"),
                       ordered = TRUE)) %>%
  mutate(brand = factor(brand,
                       levels = c("Bank1", "Bank2", "Bank3", "Bank4",
                                  "Bank5", "Bank6", "Bank7", "Bank8",
                                  "Bank9", "Bank10", "Bank11", "Bank12", "Other"),
                       ordered = TRUE))
#Plotting
p <- ggplot(brand_see, aes( x =brand, y = percentage, fill = type ))+
  geom_col(position = "stack")+
  # Add triangle markers for total
  geom_point(data = brand_see,
             aes(x = brand, y = total_percentage),
             shape = 17, size = 3, color = "green",
             inherit.aes = FALSE)  # shape 17 = triangle

print(p)  

