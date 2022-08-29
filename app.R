
# loading the supporting libraries

library(shiny)
library(vroom)
library(tidyverse)
library(shinythemes)
library(rsconnect)

dir.create("neiss")
#> Warning in dir.create("neiss"): 'neiss' already exists
download <- function(name) {
  url <- "https://github.com/hadley/mastering-shiny/raw/master/neiss/"
  download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
}
#download("injuries.tsv.gz")
#download("population.tsv")
#download("products.tsv")
if (!exists("injuries")) {
  injuries <- vroom::vroom("injuries.tsv.gz")
  products <- vroom::vroom("products.tsv")
  population <- vroom::vroom("population.tsv")
}


  
#===========Shinny prototype================
prod_codes<-setNames(products$prod_code,products$title)
# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme("superhero"),
                navbarPage(
                  "My App",
                  tabPanel("Navbar",
  fluidRow(
    column(8,
           selectInput("code","product",
                       choices =setNames(products$prod_code,products$title),
                        width = "100%"
                       )
           ),
    column(2, selectInput("y","Y axis",c("rate","count")))
    
  ),
  fluidRow(
    column(4,tableOutput("diag")),
    column(4,tableOutput("body_part")),
    column(4,tableOutput("location"))
  ),
  
  fluidRow(
    column(12,plotOutput("age_sex"))
  ),
  fluidRow(
    column(2,actionButton("story","Tell me a story")),
    column(10, textOutput("narrative"))
  )
  )
 )
)
#<< count_top
count_top<-function(df,var,n=5){
  df %>%
    mutate({{var}}:=fct_lump(fct_infreq({{var}}),n=n)) %>%
    group_by({{var}}) %>%
    summarise(n=as.integer(sum(weight)))
}

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  selected<-reactive(injuries %>% filter(prod_code == input$code))
  #<< Tables
  output$diag<-renderTable(count_top(selected(),diag),width = "100%")
  output$body_part<-renderTable(count_top(selected(),body_part),width = "100%")
  output$location<-renderTable(count_top(selected(),location),width = "100%")
  
  #>> 
  
  
  summary<-reactive({
    selected() %>%
      count(age,sex,wt=weight) %>%
      left_join(population,by=c("age","sex")) %>%
      mutate(rate=n/population * 1e4)
  })
  output$age_sex<-renderPlot({
    if(input$y=="count"){
    summary() %>%
      ggplot(aes(age,n,colour=sex))+
      geom_line()+
      labs(y="Estimated number of injuries")
    }else{
      summary() %>%
        ggplot(aes(age,rate,colour=sex))+
        geom_line(na.rm = TRUE)+
        labs(y="Injuries per 10,000 people")
    }
  },res=96)
  
  # narrative-server
  narrative_sample<-eventReactive(
    list(input$story, selected()),
    selected() %>% pull(narrative) %>% sample(1)
  )
  output$narrative<-renderText(narrative_sample())
}

# Run the application 
shinyApp(ui,server)
