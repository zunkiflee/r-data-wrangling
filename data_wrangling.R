library(tidyverse)
library(gapminder)

# review data
gapminder
view(gapminder)
glimpse(gapminder)

nrow(gapminder)
ncol(gapminder)

#check missing value
sum(is.na(gapminder))

gapminder %>%
  count(continent)

# select()
df_select <- gapminder %>%
  select(country, continent, 
         year, gdpPercap)

#filter
df_filter <- gapminder %>%
  filter(pop >= 5000000)

df_filter2 <- gapminder %>%
  select(continent, pop) %>%
  filter(continent == "Americas" & pop > mean(pop))

df_filter3 <- gapminder %>%
  select(country, year, lifeExp) %>%
  filter(grepl("Canada", gapminder$country))

#mutate
df_mutate <- gapminder %>%
  select(country, pop) %>%
  mutate(pop_level = if_else(
    pop >= 10000000, "Hight", "Low"))

df_mutate2 <- gapminder %>%
  mutate(location=paste(country,continent,sep=",")) %>%
  select(location, lifeExp)
  
#arrange
df_arrange <- gapminder %>%
  select(country, year, lifeExp, gdpPercap) %>%
  arrange(desc(lifeExp))
  
#summarise
df_summarise <- gapminder %>%
  summarise(min_gdpPercap = min(gdpPercap),
            mix_gdpPercap = max(gdpPercap),
            mean_gdpPercap = mean(gdpPercap),
            sd_gdpPercap = sd(gdpPercap),
            n = n())
  
# group_by
df_groupby <- gapminder %>%
  group_by(continent) %>%
  summarise(min_gdpPercap = min(gdpPercap),
            mix_gdpPercap = max(gdpPercap),
            mean_gdpPercap = mean(gdpPercap),
            sd_gdpPercap = sd(gdpPercap),
            n = n())
  
#count(
df_count <- gapminder %>%
   count(continent) %>%
  rename(total_country = n)
  
#rename  
df_count <- gapminder %>%
  count(continent) %>%
  rename(total_country = n)

#clean data
data <- read_csv("sample-store.csv")
glimpse(data)

library(janitor)
data <- data %>%
  clean_names()
names(data)

#check missing vlaue
sum(is.na(data))

#delete missing value
data <- na.omit(data)

data %>%
  count(region)

#mini project 
df <- data %>%
  select(order_date, segment, city, 
         region, category,
         sub_category,
         sales) %>%
  filter(region == 'South' | region == 'Central') %>%
  arrange(desc(sales))


df_2 <- data %>%
  group_by(category) %>%
  summarise(mean_sales = mean(sales),
            sum_sales = sum(sales),
            sd_sales = sd(sales),
            min_sales = min(sales),
            max_sales = max(sales),
            total = n())