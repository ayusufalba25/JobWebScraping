# Import library
library(shiny)
library(tidyverse)
library(tm)
library(wordcloud2)

# Data preparation
dataall5 <- read_delim("5pages_27Nov2021-16.10.csv", delim = ";")
dataall5$salary <- unlist(map(dataall5$salary, 
                              function(x) if(x == "NaN") return(NA) else return(x)))
colnames(dataall5) <- c("role", "company", "location", "salary", 
                        "company rating", "description", "posting date", "url")

location <- dataall5 %>% 
  group_by(location) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))

company <- dataall5 %>% 
  group_by(company) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# Word cloud function
wordcloud_df <- function(data, company_name, spec_role = FALSE, role_name = NULL){
  # Merge all the description from the specified role
  if(spec_role == TRUE){
    data <- data[data$company == company_name & data$role == role_name, "description"]
    description <- data
  }
  else{
    data <- data[data$company == company_name, "description"]
    description <- paste(data, collapse = " ")
  }
  
  description <- iconv(description, to= "UTF-8")
  text_corpus <- Corpus(VectorSource(description))
  text_corpus <- text_corpus %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  text_corpus <- tm_map(text_corpus, content_transformer(tolower))
  text_corpus <- tm_map(text_corpus, removeWords, stopwords())
  
  # Create required data type for word cloud
  dtm <- TermDocumentMatrix(text_corpus)
  text_matrix <- as.matrix(dtm)
  words <- sort(rowSums(text_matrix), decreasing = T)
  df <- data.frame(word = names(words), freq = words)
  
  return(df)
}

# Define the ui
ui <- fluidPage(
  h1("Data Analyst Job"),
  h4(tags$a(href = "https://www.linkedin.com/in/ahmad-yusuf-a-ab5696130/", "Ahmad Yusuf Albadri")),
  tabsetPanel(
    tabPanel(
      "Visualization",
      sidebarLayout(
        sidebarPanel(
          h4(em(strong("Bar Chart Parameter"))),
          sliderInput("toploc", "Top n for location", value = 5,
                       min = 1, max = 10, step = 1),
          sliderInput("topcomp", "Top n for company", value = 5,
                      min = 1, max = 10, step = 1),
          hr(),
          h4(em(strong("Word Cloud and Table Parameter"))),
          selectInput("comp", "Company", company$company),
          checkboxInput("role", "Choose specific role?*"),
          conditionalPanel(
            condition = "input.role == 1",
            uiOutput("specrole")
          ),
          p("*if not specified, all the available job descriptions will be shown in the word cloud")
        ),
        mainPanel(
          fluidRow(
            column(6, plotOutput("location", width = "100%")),
            column(6, plotOutput("company", width = "100%"))
          ),
          hr(),
          wordcloud2Output("wordcloud", width = "100%"),
          hr(),
          tableOutput("job_table")
        )
      )
    ),
    tabPanel(
      "About the App",
      br(),
      p('I created this web apps based on my project called', strong("Job Web Scraping for Data Analyst Role in Indonesia from",
                                                               a(href = "https://id.indeed.com/lowongan-kerja?q=data+analyst&l=Indonesia", "id.indeed.com")),
        "(accessed on Nov 27, 2021 at 16.10 JKT Time for the first five pages). This particular web apps aims to visualize the data that has been collected using web scraping technique.
        There are three different sections in the visualization panel that you can explore:"),
      tags$ol(tags$li("Bar chart for the top n location and company based on the number of occurence in the data."),
              tags$li("Word cloud of job description."),
              tags$li("Table that contains information about the role, location, salary (if available), company rating (if available), and url for further information.")),
      p("As you can see from the bar chart that", strong("Jakarta and GO-JEK"), "has the highest number of occurence in the data for location and job respectively.")
      
      
    )
  )
)

# Define the server
server <- function(input, output){
  output$specrole <- renderUI(selectInput("specroledone", "Role to show", 
                                          dataall5[dataall5$company == input$comp, 1] %>% pull()))
  output$location <- renderPlot({
    location %>% 
      mutate(name = fct_reorder(location, n)) %>% 
      top_n(input$toploc, n) %>% 
      ggplot(aes(x = name, y = n)) + 
      geom_bar(stat = "identity") +
      coord_flip() + 
      xlab("") +
      ylab("count") +
      ggtitle(paste("Top", input$toploc, "Job Location")) +
      theme_minimal()
  })
  output$company <- renderPlot({
    company %>% 
      mutate(name = fct_reorder(company, n)) %>% 
      top_n(input$topcomp, n) %>% 
      ggplot(aes(x = name, y = n)) + 
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("") +
      ylab("count") +
      ggtitle(paste("Top", input$topcomp, "Company")) +
      theme_minimal()
  })
  output$job_table <- renderTable({
    dataall5[dataall5$company == input$comp,] %>% 
      select(role, location, salary, `company rating`, url) %>% 
      distinct()
  })
  output$wordcloud <- renderWordcloud2({
    wordcloud2(wordcloud_df(dataall5, input$comp, input$role, input$specroledone))
  })
}

shinyApp(ui, server)