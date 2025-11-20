# Step 1: Load Packages ----
library(tidyverse)
library(rvest)
library(googlesheets4)


# Step 2: Scrape Rank Data ----
JDK_Full_Raw_Ranks <- read_html("https://neilhatfield.github.io/Stat184_PayGradeRanks.html") %>%
  html_elements(css = "table") %>%
  html_table()

JDK_Raw_Ranks <- JDK_Full_Raw_Ranks[[1]]

# Step 3: Wrangle Rank Data ----
JDK_Raw_Ranks[1, 1] <- "Type"
JDK_Rank_Headers <- JDK_Raw_Ranks[1, ]
names(JDK_Raw_Ranks) <- JDK_Rank_Headers[1,]
JDK_Raw_Ranks <- JDK_Raw_Ranks[-c(1, 26),]

JDK_Clean_Ranks <- JDK_Raw_Ranks %>%
  dplyr::select(!Type) %>% 
  pivot_longer(
    cols = !`Pay Grade`,
    names_to = "Branch",
    values_to = "Rank"
  ) %>%
  mutate(
    Rank = na_if(x = Rank, y = "--")
  )

# Step 4: Load Armed Forces Data ----
gs4_deauth()
JDK_Forces_Headers <- read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/19xQnI1cBh6Jkw7eP8YQuuicMlVDF7Gr-nXCb5qbwb_E/edit?gid=597536282#gid=597536282",
  col_names = FALSE,
  n_max = 3
)

JDK_Raw_Forces <- read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/19xQnI1cBh6Jkw7eP8YQuuicMlVDF7Gr-nXCb5qbwb_E/edit?usp=sharing",
  col_names = FALSE,
  skip = 3,
  n_max = 28,
  na = c("N/A*")
)


# Step 5: Wrangle Armed Forces Data ----
JDK_Branch_Names <- rep(
  x = c("Army", "Navy", "Marine Corps", "Air Force", "Space Force", "Total"),
  each = 3
)

JDK_Temporary_Headers <- paste(
  c("", JDK_Branch_Names),
  JDK_Forces_Headers[3,],
  sep = "."
)

names(JDK_Raw_Forces) <- JDK_Temporary_Headers

JDK_Clean_Forces <- JDK_Raw_Forces %>%
  rename(Pay.Grade = `.Pay Grade`) %>%
  dplyr::select(!contains("Total")) %>%
  filter(
    Pay.Grade != "Total Enlisted" & 
      Pay.Grade != "Total Warrant Officers" & 
      Pay.Grade != "Total Officers" & 
      Pay.Grade != "Total"
  ) %>% 
  pivot_longer(
    cols = !Pay.Grade,
    names_to = "Branch.Sex",
    values_to = "Frequency"
  ) %>%
  separate_wider_delim(
    cols = Branch.Sex,
    delim = ".",
    names = c("Branch", "Sex")
  ) 

# Step 6: Merge Data Frames ----
JDK_Joint_Forces_Ranks <- left_join(
  x = JDK_Clean_Forces,
  y = JDK_Clean_Ranks,
  by = join_by(`Pay.Grade` == `Pay Grade`, `Branch` == `Branch`)
)

# Step 7: Transform Group into Individual ----
JDK_Armed_Forces_Individuals <- JDK_Joint_Forces_Ranks %>%
  filter(!is.na(Frequency)) %>% 
  uncount(
    weights = Frequency
  )

# Step 8: Create Data Table
JDK_Air_Force_Combined <- JDK_Joint_Forces_Ranks %>%
  filter(Branch == "Air Force")

JDK_Air_Force_Officer <- JDK_Air_Force_Combined %>%
  filter(Pay.Grade %in% c("O1","O2","O3","O4","O5","O6","O7","O8","O9","O10"))

Frequency_Table <- JDK_Air_Force_Officer %>%
  group_by(Sex, Pay.Grade) %>%
  summarise(Frequency = sum(Frequency)) %>%
  pivot_wider(names_from = Pay.Grade, values_from = Frequency, values_fill = 0)

Frequency_Table <- Frequency_Table[c("Sex", "O1","O2","O3","O4","O5","O6","O7","O8","O9","O10")] %>%
  rename(
    "Second Lieutenant" = O1,
    "First Lieutenant" = O2,
    "Captain" = O3,
    "Major" = O4,
    "Lieutenant Colonel" = O5,
    "Colonel" = O6,
    "Brigadier General" = O7,
    "Major General" = O8,
    "Lieutenant General" = O9,
    "General" = O10
  )

Frequency_Table %>%
  kable() %>%
  kableExtra::kable_classic()
  
