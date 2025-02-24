library(xml2)
library(rvest)
library(jsonlite)
library(robotstxt)
library(RSocrata)
library(tidyverse)


paths_allowed("https://www.niche.com/colleges/search/best-colleges/")

url <- read_html("https://www.niche.com/colleges/search/best-colleges/")
str(url)


nds <- html_elements(url, xpath = '//*[contains(concat( " ", @class, " " ), 
                     concat( " ", "nss-w5w7xf", " " ))]')

str(nds)

names <- html_text(nds)
head(names)

nds2 <- html_elements(url, xpath = '//*[contains(concat( " ", @class, " " ), 
                      concat( " ", "search-result-fact-list", " " ))]')

stats <- html_text(nds2)
head(stats)
  
stats_dat <- as_tibble(stats)
head(stats_dat)

stats_dat %>%
  separate(value, c("n_grade", "rest"), "Overall Niche Grade")



stats_dat2 <- stats_dat %>%
  separate(value, c("n_grade", "rest"), "Overall Niche GradeAcceptance rate ") %>%
  separate(rest, c("acc_rate", "rest"), "%Net price \$") %>%
  separate(rest, c('net_price', "SAT"), 'SAT range ') %>%
  separate(SAT, c('SAT_lower', 'SAT_upper'), '-')

stats_dat2

# Clean up n_grade
stats_dat2$n_grade <- gsub("grade", "", stats_dat2$n_grade) %>% str_trim()

# remove " Acceptance rate" from "rest" string
# stats_dat2$acc_rate <- gsub("Acceptance+\\s+rate+\\s", 
#                             "", stats_dat2$acc_rate) %>%
#   as.numeric()

# Remove "Net price $"
# stats_dat2$net_price <- gsub("\\$", "", stats_dat2$net_price)

# Remove "," in dollars
stats_dat2$net_price <- gsub(",", "", stats_dat2$net_price)

stats_dat2 <- stats_dat2 %>% mutate(acc_rate = as.numeric(acc_rate) / 100,
                                    net_price = as.numeric(net_price),
                                    SAT_lower = as.numeric(SAT_lower),
                                    SAT_upper = as.numeric(SAT_upper))
stats_dat2 %>% head()

### We could do more tidying, e.g. create lower SAT range and upper SAT range bounds variables
### For now, however, we stopp here and just merge this data with the college names we scraped earlier
### and create a (somewhat) final data set.

top_colleges <- data.frame(names, stats_dat2)
str(top_colleges)
head(top_colleges)

### Add another variable
## Adding city

city_data <- html_elements(url, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "card__inner", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "search-result-tagline__item", " " )) and (((count(preceding-sibling::*) + 1) = 2) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "nss-1l1273x", " " ))]')
city_state <- html_text(city_data) %>% as_tibble() %>%
  separate(value, c('City', 'State'), ', ')


top_colleges_data <- top_colleges %>% mutate(city = city_state$City,
                        state = city_state$State %>% str_sub(1,2))
top_colleges_data %>% head()

top_colleges_data %>% 
  group_by(state) %>% 
  summarize(mean_price = mean(net_price), 
            num_schools = n())

### Looping over multiple pages

# When searching for colleges, the full list of results is spread over multiple pages. 
# To scrape the same type of information from multiple pages we can embed the previous steps in a loop.
# We first specify the number of pages to loop over (e.g., 10) and define an empty `data.frame` to store the results.

npages <- 10
colleges <- data.frame()

# Now we run all previous steps within a for-loop. The key component here is that since 
# we need a different url for each page, we use a counter and modify the url 
# by pasting in a new page number (from one to ten) in every iteration of the loop.
# The other components of the loop are essentially the same as before, minus most 
# of the data cleaning to keep things readable.

for(i in 2:npages) {
  url <- paste0("https://www.niche.com/colleges/search/best-colleges/?page=",i, sep = "")
  src <- read_html(url)
  print(url)
  
  nds <- html_elements(src, xpath = '//*[contains(concat( " ", @class, " " ), 
                     concat( " ", "nss-w5w7xf", " " ))]')
  
  str(nds)
  
  names <- html_text(nds)
  
  nds2 <- html_elements(src, xpath = '//*[contains(concat( " ", @class, " " ), 
                      concat( " ", "search-result-fact-list", " " ))]')
  stats <- html_text(nds2)
  stats_dat <- as_tibble(stats)
  stats_dat2 <- stats_dat %>%
    separate(value, c("n_grade", "rest"), "Overall Niche GradeAcceptance rate ") %>%
    separate(rest, c("acc_rate", "rest"), "%Net price \\$") %>%
    separate(rest, c('net_price', "SAT"), 'SAT range ') %>%
    separate(SAT, c('SAT_lower', 'SAT_upper'), '-')
  
  # Clean up n_grade
  stats_dat2$n_grade <- gsub("grade", "", stats_dat2$n_grade) %>% str_trim()
  
  stats_dat2$net_price <- gsub(",", "", stats_dat2$net_price)
  
  stats_dat2 <- stats_dat2 %>% mutate(acc_rate = as.numeric(acc_rate) / 100,
                                      net_price = as.numeric(net_price),
                                      SAT_lower = as.numeric(SAT_lower),
                                      SAT_upper = as.numeric(SAT_upper))
  part <- data.frame(names, stats_dat2)
  
  
  city_data <- html_elements(src, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "card__inner", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "search-result-tagline__item", " " )) and (((count(preceding-sibling::*) + 1) = 2) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "nss-1l1273x", " " ))]')
  city_state <- html_text(city_data) %>% as_tibble() %>%
    separate(value, c('City', 'State'), ', ')
  
  top_colleges_data <- part %>% mutate(city = city_state$City,
                                               state = city_state$State %>% str_sub(1,2))
  colleges <- rbind(colleges, top_colleges_data) 
}


str(colleges)

colleges %>% group_by(n_grade) %>% summarize(mean_acc_rate = mean(acc_rate))

## APIs

# A more convenient way to gather information from the web is using APIs. 
# In this example, we focus on reported crime incidents that are recorded by the 
# City of Chicago and published via an API. Documentation on the dataset and on 
# how to communicate with the API can be found here:
  
# https://dev.socrata.com/foundry/data.cityofchicago.org/6zsd-86xi

# A query consists of an unique URL that requests a certain data piece. 
# You can try out the following query with your web browser:
  
# https://data.cityofchicago.org/resource/6zsd-86xi.json?case_number=01G050460

# You might notice that the response provides data in the JSON file format.
# We can also run the query from within R and store the result as an object.


cc_exmpl1 <- fromJSON('https://data.cityofchicago.org/resource/6zsd-86xi.json?case_number=01G050460')


# The function `fromJSON` converts the JSON file into a `data.frame`, which is easier to work with in R. 


cc_exmpl1
str(cc_exmpl1)


# Whereas the previous approach should work with any API, the `RSocrata` package provides
# functions that were particularly built to communicate with the Socrata Open Data API from within R. 


cc_exmpl2 <- read.socrata('https://data.cityofchicago.org/resource/6zsd-86xi.json?case_number=01G050460')


# This approach retrieves the same information, with only some minor differences in terms of how the resulting `data.frame` is organized.


str(cc_exmpl2)


# Now lets modify the query and collect information about reported crimes for a specific date.

cc_exmpl3 <- read.socrata('https://data.cityofchicago.org/resource/6zsd-86xi.json?date=2018-05-01')


# The resulting object should now have more than one observation.


head(cc_exmpl3)


# We can also collect data for a specific time span, e.g. one year. (This may take some time to complete.)


cc_2017 <- read.socrata("https://data.cityofchicago.org/resource/6zsd-86xi.json?$where=date between '2017-01-01' and '2017-12-31'")


# How many observations do we have?
  

nrow(cc_2017)


## References

# https://www.opendatanetwork.com/
# https://dev.socrata.com/


