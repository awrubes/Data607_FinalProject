library(stringr)
library(tidyverse)
library(dplyr)
library(wdman)
library(purrr)
library(ggplot2)
library(lubridate)

artist_data_raw <- read_csv("https://raw.githubusercontent.com/awrubes/Data607_FinalProject/main/ArtistData.csv")
View(artist_data_raw)

global_economic_raw <- read_csv("https://raw.githubusercontent.com/awrubes/Data607_FinalProject/main/global_economic.csv")
View(global_economic_raw)

#data manipulation

#artist data
#normalize data and values across columns

#extract creation dates, first four digits
artist_data_cleaned <- artist_data_raw %>%
  mutate(
    creation_date = str_extract(artwork_title, "\\d{4}") %>%
      str_replace_all("[()]", "")
  )

View(artist_data_cleaned)

#create col for average estimate (middle of low and high) 
artist_data_cleaned <- artist_data_cleaned %>%
  mutate(
    cleaned_estimate = str_replace_all(price_estimate, "\\$", ""), 
    cleaned_estimate = str_trim(cleaned_estimate),
    avg_estimate = if_else(
      str_detect(cleaned_estimate, "-"), # Detect if there's a range
      {
        num1 <- str_extract(cleaned_estimate, "^[\\d,]+") %>%
          str_replace_all(",", "") %>% 
          as.numeric()
        num2 <- str_extract(cleaned_estimate, "[\\d,]+$") %>%
          str_replace_all(",", "") %>% 
          as.numeric()
        # Output for debugging:
        (num1 + num2) / 2
      },
      str_replace_all(cleaned_estimate, ",", "") %>% 
        as.numeric()
    )
  )

View(artist_data_cleaned)

#normalize sale_price column
artist_data_cleaned <- artist_data_cleaned %>%
  mutate(
    sale_price = str_replace_all(sale_price, "\\$", "") %>%
      str_replace_all(",", "") %>%
      as.numeric()
  )

View(artist_data_cleaned)

#create estimate sale price ratio
artist_data_cleaned <- artist_data_cleaned %>%
  mutate(
    sale_estimate_ratio = if_else(avg_estimate == 0 | is.na(avg_estimate), NA_real_, sale_price / avg_estimate)
  )

View(artist_data_cleaned)

#convert sale_date column to date format for ease of use
artist_data_new <- artist_data_cleaned %>%
  mutate(
    sale_date = as.Date(sale_date, format = "%d %b %Y") 
  )

print(artist_data_new)

#normalize remaining columns
artist_data_new <- artist_data_new %>%
  mutate(
    artist_name = tolower(artist_name), 
    artwork_title = tolower(artwork_title),
    auction_house = tolower(auction_house),
  )

#convert global economic data to long format from wide, using regex to match year ignoring formatting and making integer
global_economic_clean <- global_economic_raw %>%
  pivot_longer(
    cols = matches("YR\\d{4}"), 
    names_to = "Year",
    names_prefix = ".*YR", 
    values_to = "Value"
  ) %>%
  mutate(
    Year = str_extract(Year, "\\d+") %>%  
      as.integer() 
  )

View(global_economic_clean)

#get rid of country code and series code columns
global_economic_clean <- global_economic_clean %>%
  select(-all_of(c("Country Code", "Series Code")))

View(global_economic_clean)

#look at the economic variables here
unique_series_names <- unique(global_economic_clean$`Series Name`)
print(unique_series_names)


#auction house sale/estimate mean
# Calculate mean sale/estimate ratio for each auction house
mean_ratios <- artist_data_new %>%
  group_by(auction_house) %>%
  filter(n() >= 5) %>%
  summarize(mean_ratio = base::mean(sale_estimate_ratio, na.rm = TRUE), .groups = 'drop')

# Create the bar chart
ggplot(mean_ratios, aes(x = auction_house, y = mean_ratio, fill = auction_house)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(title = "Mean Sale/Estimate Ratio by Auction House",
       x = "Auction House",
       y = "Mean Sale/Estimate Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#visualize avg sales/estimate ratio per artist
artist_avg_ratio <- artist_data_new %>%
  group_by(artist_name) %>%
  filter(n() >= 5) %>%  # Ensure enough data points per artist
  summarize(avg_ratio = base::mean(sale_estimate_ratio, na.rm = TRUE))

# bar chart visualization
ggplot(artist_avg_ratio, aes(x = reorder(artist_name, avg_ratio), y = avg_ratio)) +
  geom_col() +
  coord_flip() +  # flip coordinates for legibility
  labs(title = "Average Sale/Estimate Ratio by Artist",
       x = "Artist",
       y = "Average Ratio") +
  theme_minimal()



artist_data_cleaned <- artist_data_cleaned %>%
  mutate(
    sale_date = dmy(sale_date),  # converts '19 Nov 1992' to Date
    year = year(sale_date)  # extract year from Date object
  )

#scatterplot with legend for artists looking at the SE ratio
artist_yearly_avg_ratio <- artist_data_cleaned %>%
  mutate(year = year(sale_date)) %>%
  group_by(artist_name, year) %>%
  summarize(avg_ratio = base::mean(sale_estimate_ratio, na.rm = TRUE), .groups = 'drop') %>%
  filter(n() >= 5)  # Optionally, filter artists with at least 5 artworks sold per year for stability in averages


print(artist_yearly_avg_ratio)

# Scatter plot of yearly average sale/estimate ratio per artist
ggplot(artist_yearly_avg_ratio, aes(x = year, y = avg_ratio, color = artist_name)) +
  geom_point() +  # Points for each year and artist
  geom_line() +  # Connect points with lines for each artist
  scale_color_viridis_d() +  # Use a discrete color scale
  labs(
    title = "Yearly Average Sale/Estimate Ratio by Artist Over Time",
    x = "Year",
    y = "Average Sale/Estimate Ratio",
    color = "Artist"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#visualize art work sale/estimate ratio
ggplot(artist_data_cleaned, aes(x = year(as.Date(sale_date, "%d %b %Y")), y= sale_estimate_ratio)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Artwork Sale/Estimate Ratio Over Time",
       x = "Years",
       y = "Sale/Estimate Price Ratio") +
  theme_minimal()

#notice that outliers might be affecting the data


#look at SE ratio based on medium
artist_data_cleaned_med <- artist_data_cleaned %>%
  mutate(
    medium_clean = tolower(medium),  # Convert to lower case for uniformity
    medium_clean = str_replace_all(medium_clean, " on ", "/"),  # Standardize 'on' to '/'
    medium_clean = str_replace_all(medium_clean, ", ", "/"),  # Replace commas with slashes for consistency
    medium_clean = str_replace_all(medium_clean, " and ", "/"),  # Replace 'and' with '/'
    medium_clean = str_replace_all(medium_clean, " with ", "/")  # Replace 'with' with '/'
  )

artist_data_cleaned_med <- artist_data_cleaned_med %>%
  mutate(
    medium_clean = str_extract(medium_clean, "^(oil|acrylic|watercolor|gouache|ink|pastel|charcoal|sculpture|bronze|mixed media|photograph|print|drawing|construction|installation|c print|gelatin silver print|chromogenic print)"),
    medium_clean = if_else(str_detect(medium_clean, "canvas|panel|paper|wood|metal|plastic"), "mixed media", medium_clean)
  )

artist_data_cleaned_med <- artist_data_cleaned_med %>%
  mutate(
    medium_clean = if_else(is.na(medium_clean), "other", medium_clean)
  )

# Create the box plot
ggplot(artist_data_cleaned_med, aes(x = medium_clean, y = sale_estimate_ratio)) +
  geom_boxplot() +
  labs(title = "Box Plot of Sale/Estimate Ratio by Medium",
       x = "Medium",
       y = "Sale/Estimate Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


# Create the box plot
ggplot(artist_data_cleaned, aes(x = Medium_Clean, y = sale_estimate_ratio)) +
  geom_boxplot() +
  labs(title = "Sale/Estimate Ratio by Art Medium",
       x = "Art Medium",
       y = "Sale/Estimate Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for readability




#creating regression to look at correlation between sales/estimate ratio and economic factors

#calculate yearly mean or median sale/estimate ratios
yearly_ratios <- artist_data_cleaned %>%
  group_by(year) %>%
  summarize(
    Avg_Sale_Estimate_Ratio = base::mean(sale_estimate_ratio, na.rm = TRUE),
    .groups = 'drop'
  )

View(artist_data_cleaned)

#look at the countries included
unique_country_names <- unique(global_economic_clean$`Country Name`)
print(unique_country_names)

#North America

# Filter for GDP per capita for North America
global_economic_yearly <- global_economic_clean %>%
  filter(`Series Name` == "GDP per capita (current US$)", `Country Name` == "North America") %>%
  select(Year, Value) %>%
  rename(GDP_per_capita = Value)

global_economic_interest <- global_economic_clean %>%
  filter(`Series Name` == "Real interest rate (%)", `Country Name` == "United States") %>%
  select(Year, Value) %>%
  rename(Real_Interest_Rate = Value)

artist_data_cleaned$year <- as.integer(artist_data_cleaned$year)
global_economic_yearly$Year <- as.integer(global_economic_yearly$Year)
global_economic_interest$Year <- as.integer(global_economic_interest$Year)

View(global_economic_interest)

# Merge the datasets on the year column
merged_data_gdp <- merge(artist_data_cleaned, global_economic_yearly, by.x = "year", by.y = "Year", all.x = TRUE)

View(merged_data)

filtered_data <- merged_data %>%
  filter(!is.na(GDP_per_capita))

View(filtered_data)

merged_data_interest <- merge(artist_data_cleaned, global_economic_interest, by.x = "year", by.y = "Year", all.x = TRUE)
View(merged_data_interest)
filtered_data_interest <- merged_data_interest %>%
  filter(!is.na(Real_Interest_Rate))

# linear regression for interest rates and SE ratio
model <- lm(sale_estimate_ratio ~ Real_Interest_Rate, data = filtered_data_interest)
summary(model)

ggplot() +
  geom_line(data = filtered_data_interest, aes(x = year, y = Real_Interest_Rate, group = 1), color = "blue") +
  geom_line(data = filtered_data_interest, aes(x = year, y = sale_estimate_ratio, group = 1), color = "red", sec.axis = sec_axis(~./max(merged_data$sale_estimate_ratio) * max(merged_data$GDP_per_capita), name = "Sale/Estimate Ratio")) +
  labs(title = "Economic Indicators vs. Art Market Behavior Over Time",
       x = "Year",
       y = "US Interest Rate (Real)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Assuming you have merged your datasets into `merged_data`
model <- lm(sale_estimate_ratio ~ Real_Interest_Rate, data = filtered_data)
summary(model)

ggplot() +
  geom_line(data = filtered_data, aes(x = year, y = GDP_per_capita, group = 1), color = "blue") +
  geom_line(data = filtered_data, aes(x = year, y = sale_estimate_ratio, group = 1), color = "red", sec.axis = sec_axis(~./max(merged_data$sale_estimate_ratio) * max(merged_data$GDP_per_capita), name = "Sale/Estimate Ratio")) +
  labs(title = "Economic Indicators vs. Art Market Behavior Over Time",
       x = "Year",
       y = "GDP per Capita (current US$)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

