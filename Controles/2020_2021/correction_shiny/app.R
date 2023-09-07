
library(shiny)
library(shinydashboard)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(scales)
library(forcats)
library(DT)
library(shinyWidgets)

# préparation des données


  
  
  
  # Define UI for application that draws a histogram
  ui = dashboardPage(
    dashboardHeader(
      title = h3("Informations sur les diamants"),
      titleWidth = 230
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Informations générales", 
                 tabName  = "information"
        ),
        menuItem("Détails",
                 tabName = "detail"
        ),
        menuItem("Données",
                 tabName = "donnees"
        )
      )
    ),
      dashboardBody(
        tabItems( 
          tabItem("information",
                  
                  box(
                    title = "Distribution des coupes",
                    plotOutput("diagrammecut"),
                    width = 5
                  ),
                  box(
                    title = "Distribution des couleurs",
                    plotOutput("diagrammecolor"),
                    width = 5
                  ),
                  infoBox(
                    title = "Les données",
                    value = textOutput("donnees"),
                    icon = icon("diamond"),
                    color = "blue",
                    width = 5
                  ),
                  tabBox(
                    title = "Résumé des diamants par type de coupe",
                    width = 5,
                    tabPanel(title = "Prix du diamant", 
                             dataTableOutput("resume_price")
                    ),
                    tabPanel(title = "Profondeur du diamant", 
                             dataTableOutput("resume_depth")
                    ))
          ),
          tabItem("detail",
                  box(
                    title = "Distribution",
                    plotOutput("histo"),
                    width = 4
                  ),
                  box(
                    title = "Histogramme suivant les coupes",
                    plotOutput("histo_cut"),
                    width = 4
                  ),
                  box(
                    title = "répartition selon les carats",
                    plotOutput("plot"),
                    width = 4
                  ),
                  box(
                    selectInput("choix",
                                "Variable à visualiser",
                                list("Choisir une modalité" = 1,
                                     "Price" = 2,
                                     "Depth" = 3
                                )
                    )
                  ),
                  box(
                    sliderInput("nb_classe",
                                label = "Nombre de classes pour les histogrammes :",
                                min = 1,
                                max = 50,
                                value = 30)
                  )
                  
          ),
          tabItem("donnees",
                  tabBox(
                    title = "Résumé des diamants",
                    width = 6,
                    tabPanel(title = "carat", 
                             dataTableOutput("carat")
                    ),
                    tabPanel(title = "prix", 
                             dataTableOutput("price")
                    ),
                    tabPanel(title = "profondeur", 
                             dataTableOutput("depth")
                    ),
                    tabPanel(title = "clareté", 
                             dataTableOutput("clarity")
                    ),
                    tabPanel(title = "coupe", 
                             dataTableOutput("cut")
                    ),
                    tabPanel(title = "table", 
                             dataTableOutput("table")
                    ),
                    tabPanel(title = "couleur", 
                             dataTableOutput("color")
                    ))))),
      title = "Informations sur les diamants",
      skin = "blue"
    )
  
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    
    
    output$donnees = renderText({
      paste(c(nrow(diamonds)," lignes et  ",ncol(diamonds),"colonnes, dont :", colnames(diamonds))
      )
    })
    
    output$diagrammecut = renderPlot({
      ggplot(diamonds, aes("", fill=cut))+
        geom_bar(
          aes(y=..count../sum(..count..)),
          width=1
        )+
        scale_y_continuous(labels=percent)+
        coord_polar(theta="y")+
        scale_fill_brewer(palette = "Set2")+
        theme_minimal()+
        theme(axis.title = element_blank())+
        labs(fill="coupe")
    })
    
    output$diagrammecolor = renderPlot({
      ggplot(diamonds, aes("", fill=color))+
        geom_bar(
          aes(y=..count../sum(..count..)),
          width=1
        )+
        scale_y_continuous(labels=percent)+
        coord_polar(theta="y")+
        scale_fill_brewer(palette = "Set2")+
        theme_minimal()+
        theme(axis.title = element_blank())+
        labs(fill="couleur")
    })
    
    output$resume_price = renderTable({
      data.frame(
        Statistique = c("Moyenne", "Ecart-type"),
        Valeur = c(
          round(mean(diamonds$price, na.rm = T),2),
          round(sd(diamonds$price, na.rm = T), 2)
        )
      )
    })
    
    output$resume_depth = renderTable({
      data.frame(
        Statistique = c("Moyenne", "Ecart-type"),
        Valeur = c(
          round(mean(diamonds$depth, na.rm = T),2),
          round(sd(diamonds$depth, na.rm = T), 2)
        )
      )
    })
    
    #deuxième onglet
    
    donnees_react = reactive({
      if (input$choix == "Price") {
        diamonds = diamonds %>%
          select(price)
      } else {
        diamonds = diamonds %>%
          select(depth)
      }
      diamonds
    })
    
    output$histo = renderPlot({
      if(input$choix>1){
        ggplot(donnees_react(), aes(donnees_react())) + 
          geom_histogram(aes(y=..density..), bins = input$nb_classe) + 
          labs(x="", 
               y="Densite")}
    })
    
    output$histo_cut = renderPlot({
      if(input$choix>1){
        ggplot(diamonds, aes(donnees_react(), color = cut, fill = cut)) + 
          geom_histogram(aes(y=..density..), bins = input$nb_classe) + 
          labs(x="", 
               y="Densite")}
    })
    
    output$plot = renderPlot({
      if(input$choix>1){
        ggplot(diamonds, aes("donnees_react()")) + 
          geom_point(aes(y=carat)) + 
          labs(x="", 
               y="carat")}
    })
    
    #troisième onglet
    
    output$price = renderDataTable({
      datatable(
        data.frame(
          diamonds %>% select(price)
        )
      ) 
      
    })
    output$depth = renderDataTable({
      datatable(
        data.frame(
          diamonds %>% select(depth)
        )
      ) 
    })
    output$table = renderDataTable({
      datatable(
        data.frame(
          diamonds %>% select(table)
        )
      ) 
      
    })
    output$clarity = renderDataTable({
      datatable(
        data.frame(
          diamonds %>%select(clarity)
        )
      ) 
      
    })
    output$color = renderDataTable({
      datatable(
        data.frame(
          diamonds %>% select(color)
        )
      ) 
      
    })
    output$cut = renderDataTable({
      datatable(
        data.frame(
          diamonds %>%
            select(cut)
        )
      ) 
      
    })
    output$carat = renderDataTable({
      datatable(
        data.frame(
          diamonds %>% 
            select(carat)
        )
      ) 
      
    })
    
    
    
    
    
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)

