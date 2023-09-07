#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title = "commerces de Paris"),# titre de mon tableau de bord
    dashboardSidebar(sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),# declaration de menu 
      menuItem("Widgets", tabName = "Widgets", icon = icon("th"))
    )
    )
,
    dashboardBody(
     # mon premier box pour choisr arrondissement
        
        box(
          width = 4
        (selectInput("arrondissement", 
                    "Choisir l'arrondissement ", 
                    choices = sort(unique(commerces$ARRONDISSEMENT)),
                  plotOutput("plot")))
                    
                      
                    
      
      
    )
     
      
))

server <- function(input, output) {
          output$plot=renderPlot({
            commerces %>% group_by(ARRONDISSEMENT) %>% summarise(n=n())
          })
  
              
  
}





shinyApp(ui, server)


