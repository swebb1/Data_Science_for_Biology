## load libraries
library(tidyverse)

t <- read_csv("data/covid_19_clean_complete.csv",col_names = T)

## Check for missing values
is.na(t) |> colSums()

## Confirm confirmed cases is equal to the sum of deaths and recoveries
t |> filter(Confirmed != Deaths + Recovered + Active) |> 
  head()

## The dataset includes both country-level and province-level data. Let's merge the provinces into a single country.
## We can group by country and date, and then sum the confirmed cases, deaths, recoveries and active for each country on each date.
## We will lose some granularity, e.g. combining all the french colonies into France, but it will make it easier to compare countries.
tm <- t |> 
  group_by(`Country/Region`, Date) |> 
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths),
            Recovered = sum(Recovered),
            Active = sum(Active)) |> 
  ungroup()

## Is confirmed a running total? Let's plot confirmed cases over time for a few countries to check.
tm |> filter(`Country/Region` %in% c("Brazil", "Italy", "China", "New Zealand")) |> 
  ggplot(aes(x = Date, y = Confirmed, color = `Country/Region`)) +
  geom_line() +
  labs(title = "Confirmed COVID-19 Cases Over Time",
       x = "Date",
       y = "Confirmed Cases") +
  theme_minimal()

## Wow, there are a lot of cases in the Brazil. We could add a log scale to the y-axis to better visualize the trends.
tm |> filter(`Country/Region` %in% c("Brazil", "Italy", "China", "New Zealand")) |> 
  ggplot(aes(x = Date, y = Confirmed, color = `Country/Region`)) +
  geom_line() +
  labs(title = "Confirmed COVID-19 Cases Over Time (Log Scale)",
       x = "Date",
       y = "Confirmed Cases (log scale)") +
  scale_y_log10() +
  theme_minimal()

## Brazil has a lot of cases. What factors other than the virus are driving this:  
## - Population size
## - Widely available testing and reporting

## We could normalise cases to population size to get a better sense of the relative impact of the virus in different countries.
## We can use the WDI package to get population data from the World Bank, and then merge that with our COVID-19 dataset.
install.packages("WDI")
library(WDI)
population_data <- WDI(country = "all", indicator = "SP.POP.TOTL", start = 2020, end = 2020, extra = TRUE)
population_data <- population_data |> select(iso3c, country, SP.POP.TOTL)

tm_pop <- tm |> inner_join(population_data, by = join_by("Country/Region" == "country"))

## We lose some countries where the names differ in the population data e.g "US" vs "United States". 
## We could try to clean them up, but for now let's just look at the countries that match. 

## calculate cases per million population
tm_pop <- tm_pop |> mutate(Cases_per_Million = (Confirmed / SP.POP.TOTL) * 1000000)

tm_pop |> filter(`Country/Region` %in% c("Brazil", "Italy", "China", "New Zealand")) |> 
  ggplot(aes(x = Date, y = Cases_per_Million, color = `Country/Region`)) +
  geom_line() +
  labs(title = "Confirmed COVID-19 Cases Over Time",
       x = "Date",
       y = "Confirmed Cases per million population") +
  theme_minimal()

## Let's look at the top 10 countries by cases per million population as of the most recent date in the dataset.
latest_date <- max(tm_pop$Date)
top_countries <- tm_pop |> filter(Date == latest_date) |> 
  arrange(desc(Cases_per_Million)) |> 
  head(10) |>
  pull(`Country/Region`)

## Plot these top 10 countries over time.
tm_pop |> filter(`Country/Region` %in% top_countries) |> 
  ggplot(aes(x = Date, y = Cases_per_Million, color = `Country/Region`)) +
  geom_line() +
  labs(title = "Confirmed COVID-19 Cases Over Time",
       x = "Date",
       y = "Confirmed Cases per million population") +
  theme_minimal()
  

## Let's go back to the full dataset and look at daily new cases by calculating the difference in confirmed cases from one day to the next for each country. The lag() function will help us get the previous day's confirmed cases, and we can subtract that from the current day's confirmed cases to get the new cases for that day.
tm <- tm |> group_by(`Country/Region`) |> 
  arrange(Date) |>
  mutate(New_Cases = Confirmed - lag(Confirmed, default = 0)) |>
  ungroup()

## Now we can plot new cases over time for the same countries. We'll also add US
tm |> filter(`Country/Region` %in% c("US", "Brazil", "Italy", "China", "New Zealand")) |> 
  ggplot(aes(x = Date, y = New_Cases, color = `Country/Region`)) +
  geom_line() +
  labs(title = "Daily New COVID-19 Cases Over Time",
       x = "Date",
       y = "New Cases") +
  theme_minimal()

## This seems consistent with my memory of the pandemic. The US had a large surge in cases starting around March 2020, Italy had an early surge in cases around the same time, and China had a large surge early on but then managed to control it relatively quickly.

##Let's look at the top 10 countries by total confirmed cases as of the most recent date in the dataset.
latest_date <- max(tm$Date)
top_total <- tm |> filter(Date == latest_date) |> 
  arrange(desc(Confirmed)) |> 
  select(`Country/Region`, Confirmed) |> 
  head(10) |>
  pull(`Country/Region`)

## We could see how countries performed in terms of controlling the spread of the virus by looking at
## 
tm <- tm |> mutate(Deaths_per_1000_cases = (Deaths / Confirmed) * 1000)
  
## Let's plot the deaths per 1000 cases over time for the same countries.
tm |> filter(`Country/Region` %in% top_total) |> 
  ggplot(aes(x = Date, y = Deaths_per_1000_cases, color = `Country/Region`)) +
  geom_line() +
  labs(title = "Growth Rate of New COVID-19 Cases Over Time",
       x = "Date",
       y = "Growth Rate (%)") +
  theme_minimal()

## Can we do this as a gganimate bubble plot to see Deaths per 1000 cases change over time
pp <- tm |> filter(`Country/Region` %in% top_total) |> 
  ggplot(aes(x = Date, y = Deaths_per_1000_cases, color = `Country/Region`, size = Confirmed)) +
  geom_point(alpha = 0.7) +
  labs(title = "Deaths per 1000 Cases Over Time",
       x = "Date",
       y = "Deaths per 1000 Cases") +
  theme_minimal() +
  scale_size(range = c(1, 10)) +
  guides(size = guide_legend(title = "Total Confirmed Cases"))

##
library(gganimate)
ggpp <- pp + transition_states(Date, transition_length = 1, state_length = 2) +
  ease_aes('linear')
animate(ggpp,
  width = 1400,
  height = 900,
  res = 200,
  fps = 20,
  renderer = gifski_renderer()
)



```
```{r, echo=FALSE,eval=FALSE}
## Base R only
spine_data$Mean_Spine_Density <- rowMeans(spine_data[, c("Neuron_1", "Neuron_2", "Neuron_3")])

t.test(Mean_Spine_Density ~ Treatment, data = spine_data, var.equal = T)

boxplot(spine_data$Mean_Spine_Density ~ spine_data$Treatment, 
        main = "Mean Spine Density by Treatment", 
        xlab = "Treatment", 
        ylab = "Mean Spine Density (spines/10um)")

```

