# Step 1: Load  packages ----
library(dplyr)
library(ggplot2)
library(dcData)

# Step 2: Load data ----
data("BabyNames")

# Step 3: Create Koewler Family Names
Koewler_Family_Names <- c("David", "Stefanie", "Jacob", "Thomas")

# Step 4: Create Count of Names
Koewler_Family <- BabyNames %>%
  filter(name %in% Koewler_Family_Names) %>%
  group_by(name, year) %>%
  summarize(
    total = sum(count),
    .groups = "drop"
  )

# Step 5: Create Plot
ggplot(Koewler_Family,
       mapping = aes(
         x = year,
         y = total,
         color = name,
         linetype = name)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Frequency of First Names within the Koewler Family from 1880 to 2013",
       x = "Year",
       y = "Count",
       color = "Name",
       linetype = "Name") +
  scale_y_continuous(
    expand = expansion(mult = 0.01)) +
  scale_x_continuous(limits = c(1880, 2013))


