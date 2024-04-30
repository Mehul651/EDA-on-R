install.packages(c("tidyverse", "babynames", "dplyr", "ggplot2","tibble")) #This installs all the necessary packages
library(tidyverse)
library(babynames)
library(tibble)
library(dplyr)

# Filter for most popular names between 1900 and 1999
most_popular_names <- babynames %>%  #here i have created a new table named most_popular_names that will extract data from babynames
  filter(sex == "M", between(year, 1900, 1999)) #filter function allows to filter the dataset, here i have filtered sex, and year columns 
View (most_popular_names) #View shows the most_popular_names table 

# Find the three most popular names
three_popular_names <- most_popular_names %>% #Created three_popular_names table 
  group_by(name) %>% #The group_by function is used to group rows of a data frame based on one or more variables
  summarize(total = sum(n)) %>%
  arrange(desc(total)) %>%
  head(3)

View(three_popular_names)

# Filter data for the three most popular names
filtered_data <- babynames %>%
  filter(sex == "M", name %in% three_popular_names$name, between(year, 1900, 1999)) 
#This section filters the original babynames data frame to include only rows where 'sex' is "M," the 'name' is one of the three popular names, and the 'year' is between 1900 and 1999.

# Create and customize the ggplot
myplot <- ggplot(filtered_data, aes(x = year, y = n, color = name)) +
  geom_line() +
  labs(
    title = "Three most popular baby names in 20th century",
    x = "Year",
    y = "No. of occurrences",
    color = "Name"
  ) +
  theme_minimal()
#This section uses the ggplot function to create a line plot (geom_line()) based on the filtered_data. The aesthetics (aes) are set to use 'year' on the x-axis, 'n' (number of occurrences) on the y-axis, and color the lines by the 'name' variable
# Display the plot
print(myplot) 

#Finally, the print function is used to display the plot created in the previous step. The resulting plot shows the trends of the three most popular baby names for males in the 20th century
