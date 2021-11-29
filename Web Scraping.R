# ==================================================
# | Scraping jobs in https://id.indeed.com/        |
# | Note: requires captcha (if caught as a robot!) |
# ==================================================

# Import library
library(tidyverse)
library(rvest)

# Every pages require a start argument, except for the first page.
# start argument needed for the second page and so on.
# For the 2nd page start=10 and its increment is 10 for every next page.
# Ex: page 2 -> start=10, page 3 -> start=20, ..., page n -> (n-1)*10

# Extracting total pages
main_url <- "https://id.indeed.com/lowongan-kerja?q=data+analyst&l=Indonesia"
search_count_pages <- read_html(main_url) %>% 
  html_node('#searchCountPages') %>% 
  html_text() %>% 
  trimws() %>% 
  str_split(" ") %>% 
  unlist()
n_pages <- search_count_pages[length(search_count_pages) - 1] %>% 
  as.integer()

# Creating a list of all the pages that has been sorted by date
pages <- rep(0, n_pages)
for(i in 1:n_pages){
  if(i == 1) pages[i] <- main_url
  else pages[i] <- paste0(main_url, "&start=", ((i - 1) * 10))
}
print(head(pages))
length(pages)

# ================ Test scraping from the first page ==================
# webpage <- read_html(main_url)
# 
# # Extracting job ids
# job_id <- webpage %>% 
#   html_nodes('a') %>% 
#   html_attr('id') %>% 
#   na.omit()
# job_id <- job_id[grep('job_', job_id)]
# print(job_id)
# 
# # Extracting reference link for each jobs
# href <- rep(0, length(job_id))
# 
# for(i in 1:length(job_id)){
#   href[i] <- webpage %>% 
#     html_nodes('#mosaic-provider-jobcards') %>% 
#     html_nodes(paste0("#", job_id[i])) %>% 
#     html_attr('href') %>% 
#     paste0('https://id.indeed.com', .)
# }
# print(href)
# 
# # Extracting the job title
# job_title <- webpage %>% 
#   html_nodes('.job_seen_beacon') %>% 
#   html_nodes('h2') %>% 
#   html_nodes('span') %>% 
#   html_attr('title') %>% 
#   na.omit()
# job_title
# 
# # Extracting the company name
# company_name <- webpage %>% 
#   html_nodes('.companyName') %>% 
#   html_text()
# company_name
# 
# # Extracting the company rating; not all available
# # Unfilled = 90
# # stars = (filled / unfilled) * 5
# job_page <- read_html(href[1])
# company_rating <- job_page %>% 
#   html_node('.icl-Ratings-starsCountWrapper') %>% 
#   html_attr('aria-label') %>% 
#   gsub("[^0-9.-]", " ", .) %>% 
#   strsplit(" ") %>% 
#   unlist()
# company_rating <- as.numeric(company_rating[1])
# company_rating
# 
# # Extracting number of people reviewed the company; not all available
# number_review <- job_page %>% 
#   html_node('.icl-Ratings-count') %>% 
#   html_text()
# number_review
# 
# # Extracting the location
# location <- webpage %>% 
#   html_nodes('.companyLocation') %>% 
#   html_text()
# location
# 
# # Extracting job ratings; not all available
# rating <- webpage %>% 
#   html_nodes('.withRatingLink') %>% 
#   html_text()
# rating
# 
# # Extracting salary; not all available
# salary <- webpage %>% 
#   html_nodes('.salary-snippet') %>% 
#   html_text()
# salary
# 
# # Extracting the job description text
# job_page <- read_html(href[1])
# job_page %>% 
#   html_node('#jobDescriptionText') %>% 
#   html_text()
# =====================================================================

# ======================== Scraping The Website =======================
# Information to scrape:
# 1. Job title
# 2. Company name
# 3. Location
# 4. Salary (if any)
# 5. Company rating (if any)
# 6. Job description
# 7. Job posting date
# 8. Link for further information

# Hyperlink reference to the job details
href_scraper <- function(webpage){
  # Extracting job ids
  job_id <- webpage %>% 
    html_nodes('a') %>% 
    html_attr('id') %>% 
    na.omit()
  job_id <- job_id[grep('job_', job_id)]
  
  # Extracting reference link for each jobs
  href <- rep(NaN, length(job_id))
  
  for(i in 1:length(job_id)){
    href[i] <- webpage %>% 
      html_nodes('#mosaic-provider-jobcards') %>% 
      html_nodes(paste0("#", job_id[i])) %>% 
      html_attr('href') %>% 
      paste0('https://id.indeed.com', .)
  }
  return(href)
}

scraper_all <- function(list_pages){
  n <- length(list_pages) # Number of pages
  all_job_title <- NULL
  all_company_name <- NULL
  all_job_location <- NULL
  all_salary <- NULL
  all_company_rating <- NULL
  all_descriptions <- NULL
  all_posting_date <- NULL
  all_link <- NULL
  
  # Iterate through each page
  for(page in list_pages){
    webpage <- read_html(page)
    link_jobs <- href_scraper(webpage)
    container <- webpage %>% html_nodes('.job_seen_beacon')
    n_job <- length(container)  # Number of job in a page
    
    job_title <- rep(NaN, n_job)
    company_name <- rep(NaN, n_job)
    job_location <- rep(NaN, n_job)
    salary <- rep(NaN, n_job)
    company_rating <- rep(NaN, n_job)
    descriptions <- rep(NaN, n_job)
    posting_date <- rep(NaN, n_job)
    
    for(i in 1:n_job){
      # Extract each component
      job_title[i] <- container[i] %>% 
        html_nodes('h2') %>% 
        html_nodes('span') %>% 
        html_attr('title') %>% 
        na.omit()
      company_name[i] <- container[i] %>% 
        html_nodes('.companyName') %>% 
        html_text()
      job_location[i] <- container[i] %>% 
        html_nodes('.companyLocation') %>% 
        html_text()
      posting_date[i] <- container[i] %>% 
        html_nodes('.date') %>% 
        html_text()
      
      # For not all available
      salary_check <- container[i] %>% 
        html_nodes('.salary-snippet') %>% 
        html_text()
      if(length(salary_check) != 0) salary[i] <- salary_check
      
      rating_check <- container[i] %>% 
        html_nodes('.withRatingLink') %>% 
        html_text() %>% 
        gsub(",", ".", .)
      if(length(rating_check) != 0) company_rating[i] <- as.numeric(rating_check)
      
      # Job description
      job_page <- read_html(link_jobs[i])
      descriptions[i] <- job_page %>%
        html_node('#jobDescriptionText') %>%
        html_text()
    }
    all_job_title <- c(all_job_title, job_title)
    all_company_name <- c(all_company_name, company_name)
    all_job_location <- c(all_job_location, job_location)
    all_salary <- c(all_salary, salary)
    all_company_rating <- c(all_company_rating, company_rating)
    all_descriptions <- c(all_descriptions, descriptions)
    all_posting_date <- c(all_posting_date, posting_date)
    all_link <- c(all_link, link_jobs)
  }
  
  hasil_scraping <- data.frame(job_title = all_job_title,
                               company = all_company_name,
                               location = all_job_location,
                               salary = all_salary,
                               company_rating = all_company_rating,
                               description = all_descriptions,
                               posting_date = all_posting_date,
                               url_info = all_link)
  return(hasil_scraping)
}

# Scraping the first 5 pages to prevent captcha
dataall5 <- scraper_all(pages[1:5])
View(dataall5)

write_delim(dataall5, "5pages_27Nov2021-16.10.csv", delim = ";")

# Using Sys.sleep function for specified time interval
# to prevent captcha from the website
# lets_scrape <- function(x, pages, divider){
#   n <- length(pages)
#   
#   # Creating subset
#   length_subset <- ceiling(n / divider)
#   list_subset <- vector(mode = "list", length = length_subset)
#   start_index <- 1
#   stop_index <- divider
#   for(i in 1:length_subset){
#     list_subset[[i]] <- seq(start_index, stop_index)
#     start_index <- stop_index + 1
#     if(i == (length_subset - 1)) stop_index <- n
#     else stop_index <- stop_index + divider
#   }
#   
#   scraped_data <- NULL
#   # Scrape with time interval
#   for(i in list_subset){
#     scraped <- scraper_all(c(pages[i]))
#     scraped_data <- rbind(scraped_data, scraped)
#     Sys.sleep(x)
#   }
#   
#   return(scraped_data)
# }

# Run
# data_all <- lets_scrape(10, pages[1:10], 5)
