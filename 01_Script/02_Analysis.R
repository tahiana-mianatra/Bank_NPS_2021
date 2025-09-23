#Input: cleaned dataframe; survey & aware_long
#Out: NPS score graphics & brand awareness stacked barplot
#NPS: Promoter = 9 to 10, neutral 7 to 8, detractor 6 to 0
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
#Load function
round_excel <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10^digits
  z * posneg
}
categorize_nps <- function(score) {
  ifelse(score >= 9, "Promoter",
         ifelse(score >= 7, "Passive", 
                ifelse(score >= 0, "Detractor", NA)))
}
#NPS
NPS_need <- survey %>%
  select(QUEST, Q7, Q9, Q11, Q13) %>%
  rename (
    "BNI" = Q7,
    "BMOI" = Q9,
    "BOA" = Q11,
    "BRED" = Q13
  )%>%
  pivot_longer(
    cols = c(BNI, BMOI, BOA, BRED),
    names_to = "bank",
    values_to = "nps_score"
  ) %>%
  # Remove rows where NPS score is NA (no response for that bank)
  filter(!is.na(nps_score))
#Just to check if na was really droped
See <- NPS_need %>%
  filter(is.na(nps_score)) %>%
  select(QUEST)

#Create a function to categorize NPS

#Create the NPS summary dataframe
NPS_summary <- NPS_need %>%
  mutate(nps_category = categorize_nps(nps_score)) %>%
  group_by(bank) %>%
  summarise(
    total_respondents = n(),
    promoters = sum(nps_category == "Promoter"),
    passives = sum(nps_category == "Passive"),
    detractors = sum(nps_category == "Detractor"),
    # Calculate percentages based on actual respondents for each bank
    promoter_pct = promoters / total_respondents * 100,
    passive_pct = passives / total_respondents * 100,
    detractor_pct = detractors / total_respondents * 100,
    # Calculate NPS score (Promoters % - Detractors %)
    nps_score = promoter_pct - detractor_pct,
    .groups = 'drop'
  )
#Arrange 
NPS_summary <- NPS_summary%>%
  mutate(bank = factor(bank,
                     levels = c("BNI", "BMOI", "BOA", "BRED"),
                     ordered = TRUE))



#Making NPS graphics
NPS_to_plot <- NPS_summary %>%
  select(bank, promoter_pct, passive_pct, detractor_pct, nps_score) %>%
  rename (
    "Promoter" = promoter_pct,
    "Passive" = passive_pct,
    "Detractor" = detractor_pct
  )
NPS_to_plot <- NPS_to_plot %>%
  mutate(
    Promoter_text = round_excel(Promoter, 1),
    Passive_text = round_excel(Passive, 1), 
    Detractor_text = round_excel(Detractor, 1),
    nps_score_text = round_excel(nps_score, 1)
  )

NPS_long <- NPS_to_plot %>%
  pivot_longer(
    cols = c(Detractor, Passive, Promoter),  # Columns to pivot
    names_to = "category",                   # New column for category names
    values_to = "percentage"                 # New column for percentage values
  )
#Preparing the image
logo_mapping <- data.frame(
  bank = c("BNI", "BMOI", "BOA", "BRED"),
  logo_path = c(here::here("02_Input", "BNI_logo.png"),
                here::here("02_Input", "BMOI_logo.png"),
                here::here("02_Input", "BOA_logo.png"), 
                here::here("02_Input","BRED_logo.jpg"))
)
NPS_long <- NPS_long %>%
  left_join(logo_mapping, by = "bank") #Join logo_path to main data
#Manially arrange the size
NPS_long <- NPS_long %>%
  mutate(bank_label = case_when(
    bank == "BNI" ~ paste0("<img src='", logo_path, "' width='80' height='70' />"),
    bank == "BMOI" ~ paste0("<img src='", logo_path, "' width='50' height='40' />"),  # Adjust BMOI
    bank == "BOA" ~ paste0("<img src='", logo_path, "' width='40' height='30' />"),
    bank == "BRED" ~ paste0("<img src='", logo_path, "' width='40' height='30' />"),  # Adjust BRED
    TRUE ~ paste0("<img src='", logo_path, "' width='40' height='30' />")  # Default
  ))

#Arranging the order of the logo
correct_order <- c("BNI", "BMOI", "BOA", "BRED")

# Reverse the order for the plot
plot_order <- rev(correct_order)  # This will put BNI at the top, BRED at the bottom

# Get labels in the correct order (but reversed for plotting)
correct_labels <- NPS_long %>%
  distinct(bank, .keep_all = TRUE) %>%
  arrange(match(bank, correct_order)) %>%  # Force the order we want
  pull(bank_label)

# But we need to reverse the labels too to match the reversed order
correct_labels <- rev(correct_labels)
#Plot
p <- ggplot(NPS_long, aes(y = bank, x = percentage, fill = category)) +
  geom_col(position = "stack") +
  geom_text(aes(label = paste0(round_excel(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5),  # Center in each segment
            color = "white", fontface = "bold", size = 3) +
  geom_label(data = NPS_long %>% distinct(bank, .keep_all = TRUE),
             aes(y = bank, x = -5,
                 label = paste0("NPS: ", ifelse(nps_score > 0, "+", ""), round_excel(nps_score, 1)),
                 color = ifelse(nps_score > 0, "#27AE60",  # Use COLOR instead of FILL
                                ifelse(nps_score < 0, "#E74C3C", "gray50"))),
             hjust = 0, size = 4, fontface = "bold", 
             fill = "white",  # White background for all NPS labels
             show.legend = FALSE) +  # Explicitly hide legend for this layer
  scale_fill_manual(values = c(
    "Detractor" = "#CC3D3D",   # deep red
    "Passive"   = "#E5B700",   # professional amber
    "Promoter"  = "#007F5F"    # dark teal green
  ))+
  scale_x_continuous(trans = "reverse", limits = c(100, -20)) +  # Expand left side+  # This flips left/right
  scale_y_discrete(
    limits = plot_order,  # Use the REVERSED order
    labels = correct_labels  # Use the REVERSED labels
  ) +
  labs(title = "Net Promoter Score")+
  theme(
    plot.title = element_text(hjust = 0.5),# Center the title
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_markdown()
  ) +
  labs(x = NULL, y = "Bank") 
print(p)
ggsave(here::here("04_Graphic_output","NPS.png"),
       plot = p, width = 10, height = 6, dpi = 600)
