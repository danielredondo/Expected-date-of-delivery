library(shiny)
library(shinythemes)
library(dplyr)
library(timevis)
library(lubridate)

# If there are problems with the timezone...
#options(tz="Europe/Madrid")
#
# And, if that´s not enough:
#Sys.getenv("TZ")
#Sys.setenv(TZ="Europe/Madrid")
#Sys.getenv("TZ")

url <- "https://twitter.com/intent/tweet?text=Expected%20date%20of%20delivery&url=http://watzilei.com/shiny/EDL/&via=WATZILEI"
ui <- fluidPage(theme = shinytheme ("flatly"),
  titlePanel(tags$h1(tags$b("Expected date of delivery")), windowTitle = "Expected date of delivery"),
  sidebarPanel(width = 3,
    dateInput(inputId = "date",  
              label = "Date of the first day of your last menstrual period (YYYY-MM-DD)",
              value = Sys.Date() - 60,
              min = Sys.Date() - 365,
              max = Sys.Date(),
              startview = "year"
    ),
    
    sliderInput(inputId = "cycle",
                label = "Normal length of your menstrual cycle in days",
                min = 28,
                max = 41,
                value = 28,
                step = 1
    ),
  
    h6("By:", br(),
       "MA Luque-Fernandez", br(),
       "Daniel Redondo Sánchez"),
    
    actionButton("twitter",
                 label = "Share on Twitter",
                 icon = icon("twitter"),
                 style="color: #fff; background-color: #00ACED; border-color: #00ACED",
                 onclick = sprintf("window.open('%s')", url)
                 )
  ),
  
  mainPanel(
    h3("Congratulations! You are due to give birth on: ", tags$b(textOutput("dateofbirth"))),
    h4(htmlOutput("wiki_link")),
    h3(textOutput("gestation")),
    timevisOutput("timeline")
  , style = "text-align: center;")
)

server <- function(input, output) { 
  
  date_birth <- reactive({
    req(input$date)
    input$date + 280 + (input$cycle - 28)
  })
  
  output$dateofbirth <- renderText({   
    req(input$date)
    paste0(date_birth(), ", ", weekdays(date_birth()))
  }) 
  
  output$wiki_link <- renderUI({   
    req(input$date)
    link_base = "https://en.wikipedia.org/wiki/Wikipedia:Selected_anniversaries/"
    link_day <- day(date_birth())
    link_month <- switch (Sys.info()[[1]],
      "Windows" = month(date_birth(), label = TRUE, abbr = FALSE, locale = "English_United States.1252"),
      month(date_birth(), label = TRUE, abbr = FALSE)
    )
    link <- paste0(link_base, link_month, "_", link_day)
    HTML("<a href='", link, "'> Selected anniversaries of that day (Wikipedia) </a>")
  }) 

  output$gestation <- renderText({  
    req(input$date)
    today <- Sys.Date()
    weeks_gestation <- ((1 / 7) * ((today - input$date) - (input$cycle - 28))) %>% as.numeric %>% round()
    paste("You are",  weeks_gestation, "weeks gestation.")
  })
  
  output$timeline <- renderTimevis({
    req(input$date)
    data <- data.frame(
      content = c("Beginning of last menstrual cycle", "End of last menstrual cycle", "Estimated date of birth", "Today"),
      start   = c(input$date, input$date + input$cycle, input$date + 280 + (input$cycle - 28), Sys.Date()),
      end     = NA
    )
    timevis(data, options = list(showCurrentTime = FALSE, orientation = "top"))
  })
  
}

shinyApp(ui = ui, server = server)