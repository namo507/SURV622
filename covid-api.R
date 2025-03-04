# Packages
if (!requireNamespace("httr", quietly = TRUE)) {
  install.packages("httr")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}

library('httr')
library('jsonlite')
library(tidyverse)

# Using info from https://gisumd.github.io/COVID-19-API-Documentation/
base_url <- 'https://covidmap.umd.edu/api/resources'
request <- GET(base_url, query = list(indicator = 'mask',
                                      type = 'daily',
                                      country = 'Finland',
                                      daterange = '20201201-20201204'))
class(request)

# Check to see if we were successful (we want 200)
request$status_code

# Look at it (not useful for us)
head(request$content)

# Extract into data frame
response <- content(request, as = "text", encoding = "UTF-8")
coviddata <- fromJSON(response, flatten = TRUE) %>% data.frame()

## Change some parameters
request <- GET(base_url, query = list(indicator = 'covid',
                                      type = 'smoothed',
                                      country = 'Germany',
                                      daterange = '20211201-20211231'))
request$status_code

# Build the data frame
germany_covid <- request %>% 
  content(as = "text", encoding = "UTF-8") %>%
  fromJSON(flatten = TRUE) %>% 
  data.frame() %>%
  mutate(date = as.Date(data.survey_date, '%Y%m%d')) %>%
  select(-status, -data.survey_date) 

# Check data frame
head(germany_covid)

# Plot over time
ggplot(germany_covid, aes(x = date, y = data.smoothed_pct_covid)) + geom_line()


# US Data
# Looking at PG County specifically 
base_url <- 'https://api.covidcast.cmu.edu/epidata/covidcast/'
request <- GET(base_url, query = list(data_source = 'fb-survey',
                                      signal = 'smoothed_wcli',
                                      time_type = 'day',
                                      geo_type = 'county',
                                      time_values = '20211101-20211231',
                                      geo_value = '24033'))
request$status_code

# Turn into data frame
response <- content(request, as = "text", encoding = "UTF-8")
pgcounty_cli <- fromJSON(response, flatten = TRUE) %>% 
  data.frame() %>% 
  transmute(cli_percent = epidata.value,
             date = as.Date(as.character(epidata.time_value), '%Y%m%d'))

ggplot(pgcounty_cli, aes(x = date, y = cli_percent)) + geom_line()

base_url <- 'https://api.covidcast.cmu.edu/epidata/covidcast/'
request <- GET(base_url, query = list(data_source = 'doctor-visits',
                                      signal = 'smoothed_cli',
                                      time_type = 'day',
                                      geo_type = 'county',
                                      time_values = '20211101-20221231',
                                      geo_value = '24033'))
request$status_code

# Turn into data frame
response <- content(request, as = "text", encoding = "UTF-8")
pgcounty_visits <- fromJSON(response, flatten = TRUE) %>% 
  data.frame() %>% 
  transmute(visits_percent = epidata.value,
            date = as.Date(as.character(epidata.time_value), '%Y%m%d'))

pgcounty_visits_cli <- pgcounty_cli %>% 
  join(pgcounty_visits, by='date') %>%
  pivot_longer(-date)

ggplot(pgcounty_visits_cli, aes(x = date, y = value)) + geom_line(aes(color = name))

