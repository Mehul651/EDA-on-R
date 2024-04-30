#Installing all the necessary packages 
install.packages("tidyverse")
install.packages("dplyr")
install.packages("tibble")
install.packages("ggplot2") 
install.packages("readr")
install.packages("ggrepel")
#Reads the .csv dataset 
EVcars <- read.csv("C:\\Users\\mehul\\OneDrive\\Desktop\\R assignment\\EVcars.csv")
library(tidyverse)
library(dplyr)
library(tibble)
library(ggrepel)
str(EVcars) 
View(EVcars)

unique_data <- distinct(EVcars) #This command helps to remove duplicate rows based on all columns and I have created a table naming unique_data which stores the values 
View(unique_data)

#As there are no repetitive or duplicate values in the dataset I have continued using EVcars in the following codes. 

#Filtered data for most efficient car with their respective prices
efficient_cars <- EVcars %>% 
  select(Car_name, Efficiency,Price.DE.) %>%  #selects the necessary columns to make changes 
  arrange(desc(Efficiency)) %>% 
  head(10)
print(efficient_cars)
View(efficient_cars) 

#Plotting a graph for the most efficient cars 
ggplot(efficient_cars, aes(x = Car_name, y = Efficiency, fill = Price.DE.)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 10 Efficient Cars",
       x = "Car Name",
       y = "Efficiency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

#Fast cars in cheap price 
fastest_car <- EVcars %>%
  select(Car_name, Top_speed) %>% 
  arrange(desc(Top_speed)) %>% 
  head(10)
  #This line omits the rows having 'NA' values in any cell

print(fastest_car) 
View(fastest_car) 


#Plotting graph for the most fast car in cheap price 
ggplot(fastest_car, aes(x = Car_name, y = Top_speed, group = 1)) +
  geom_line(color = "green", size = 2) +
  labs(title = "Top 10 Fast Cars",
       x = "Car Name",
       y = "Top Speed") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    text = element_text(color = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  ) 
#In these lines I have tried changing the theme of the graphs by adding color gradients to the backgrounds and text to make it look more attractive. 



#Cars with the price of at least 45000 and top speed is less than equal to 200 with acceleration more than 7
filtered_cars <- EVcars %>% 
  filter(Price.DE.>= 45000,Top_speed <= 200, acceleration..0.100.>7) %>% 
  select(Price.DE.,Top_speed, Car_name, acceleration..0.100.) %>% 
  head(10)
print(filtered_cars)
View(filtered_cars)


#Plotting graph for the cars having  prince under 45000, top speed less than equal to 200 and acceleration and acceleration more than 7
ggplot(filtered_cars, aes(x = Price.DE., y = Top_speed, size = acceleration..0.100., label = Car_name)) +
  geom_point(color = "red", alpha = 0.7) +
  geom_text_repel(
    box.padding = 0.5,
    segment.color = "transparent",
    segment.size = 0.5,
    size = 3,   # Adjust the font size
    nudge_x = 0.2,  # Optional: Adjust the x-axis nudging for better label placement
    nudge_y = 0.2  # Optional: Adjust the y-axis nudging for better label placement
  ) +
  labs(
    title = "Top 10 Cars with price < 45000, top speed < 200, acceleration > 7",
    x = "Price.DE.",
    y = "Top Speed"
  ) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "grey90"))


#Cars having battery between 80 to 70 and has a range of at least 425 
range_cars <- EVcars %>% 
  filter(between(Battery, 70 , 80), Range <= "425") %>% 
  select(Car_name, Range, Battery)%>% 
  arrange(desc(Range))%>%
  head(10)
print(range_cars)
View(range_cars)

#Plotting graph for Cars having battery between 70-80 and range of atleast 425 
ggplot(range_cars, aes(x = Car_name, y = Range, fill = Battery)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 10 Cars with Battery Between 70 and 80 and Range <= 425",
       x = "Car Name",
       y = "Range") +
  theme_minimal() 

#Fast charging capabilities
fast_charging <- EVcars %>% 
  select(Car_name,Fast_charge)%>% 
  arrange(desc(Fast_charge)) %>% 
  head (10)
print(fast_charging)
View (fast_charging)


#Plotting graph for the cars having the worst fast charge 
ggplot(fast_charging, aes(x = Car_name, y = Fast_charge, group = 1)) +
  geom_line(color = "skyblue", size = 2) +
  geom_point(color = "black", size = 3) +
  labs(title = "Cars taking longest time to charge",
       x = "Car Name",
       y = "Fast Charging Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Slant the x-axis labels


#Long lasting vehicle which is based upon the weighted scores of battery, efficiency range and price. 
longlasting_car <- EVcars %>%
  select(Car_name, Battery, Range, Efficiency, Price.DE.) %>%
  mutate(
    # Define weights for each criterion
    battery_weight = 0.3,
    range_weight = 0.3,
    efficiency_weight = 0.2,
    price_weight = 0.2,
    
    # Calculate the weighted score for each car
    longlasting_car = (Battery * battery_weight +
                        Range * range_weight +
                        Efficiency * efficiency_weight +
                        Price.DE. * price_weight) / sum(c(battery_weight, range_weight, efficiency_weight, price_weight))
  ) %>%
  arrange(desc(longlasting_car)) %>%
  head(10)

# Display the best car based on the weighted score
print(longlasting_car)
View(longlasting_car)

#Plotting graph for the long lasting vehicle in the list 
ggplot(longlasting_car, aes(x = Car_name, y = longlasting_car, fill = Car_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Long-lasting Cars",
       x = "Car Name",
       y = "Weighted Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability 


#Best EV sports car 
sports_car <- EVcars %>% 
  select(Car_name, Top_speed, acceleration..0.100., Fast_charge, Price.DE.) %>% 
  mutate(
    #Define weights 
    speed_weight = 0.45,
    acceleration_weight = 0.35, 
    charge_weight = 0.2, 
    
    sports_car = (Top_speed *speed_weight +
                    acceleration..0.100.*acceleration_weight + 
                    Fast_charge * charge_weight) / sum(c(speed_weight, acceleration_weight, charge_weight))
    )%>%
      arrange(desc(sports_car)) %>% 
head(10) 
print(sports_car)
View(sports_car)
  

#Plotting graph for best EV sports car category 
myplot <- ggplot(sports_car, aes(x = Car_name, y = sports_car, group = 1)) +
  geom_line(color = "skyblue") +
  geom_point(color = "red", size = 3) +  # Optional: Add points for emphasis
  labs(
    title = "Top 10 Sports Cars based on Weighted Score",
    x = "Car Name",
    y = "Total Weighted Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(myplot)


