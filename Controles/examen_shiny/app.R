library(shiny)
library(shinydashboard)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(scales)
library(forcats)
library(DT)

### Compute external values ----
dataset <- mtcars


### Define UI ----
ui = dashboardPage(
  dashboardHeader(
    title = h3("Données mtcars "),
    titleWidth = 230
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Résumé des informations", 
               tabName  = "resume"
      ),
      menuItem("Description des données",
               tabName = "description"
      )
    )
  ),
  dashboardBody(
    tabItems( 
      tabItem("resume",
              valueBox(
                value = "Mesures sur Sépale et Pétale",
                subtitle = "Les variables",
                icon = icon("leaf"),
                #href = "https://fontawesome.com/icons/",
                color = "yellow",
                width = 8
              ),
              infoBox(
                title = "Les données",
                value = textOutput("donnees"),
                icon = icon("leaf"),
                #href = "https://fontawesome.com/icons/",
                color = "yellow",
                width = 4
              ),
              box(
                title = "Distribution des espèces",
                plotOutput("diagramme"),
                width = 6
              ),
              tabBox(
                title = "Résumé des mesures",
                width = 6,
                tabPanel(title = "Largeur de Sépale", 
                         dataTableOutput("sepal_width")
                ),
                tabPanel(title = "Longueur de Sépale", 
                         dataTableOutput("sepal_length")
                ),
                tabPanel(title = "Largeur de Pétale", 
                         dataTableOutput("petal_width")
                ),
                tabPanel(title = "Longueur de Pétale", 
                         dataTableOutput("petal_length")
                ))
      ),
      tabItem("description",
              box(
                title = "Distribution",
                plotOutput("histo"),
                width = 4
              ),
              box(
                title = "Histogramme suivant les espèces",
                plotOutput("histo_esp"),
                width = 4
              ),
              box(
                title = "Distribution par espèce",
                plotOutput("boxplot"),
                width = 4
              ),
              box(
                selectInput("choix",
                            "Variable à visualiser",
                            list("Choisir une variable" = 1,
                                 "Largeur de Sepale" = 2,
                                 "Longueur de Sepale" = 3,
                                 "Largeur de Petale" = 4,
                                 "Longueur de Petale" = 5
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
              
              
      )        
      
    )),
  title = "Informations sur les iris de Fisher",
  skin = "purple"
)

### Define server logic ----
server = function(input, output) {
  
  variable_a_afficher = reactive({
    if(input$choix == 2) var = "Sepal.Width"
    if(input$choix == 3) var = "Sepal.Length"
    if(input$choix == 4) var = "Petal.Width"
    if(input$choix == 5) var = "Petal.Length"
    var
  })
  
  
  output$diagramme = renderPlot({
    ggplot(iris, aes("", fill=Species))+
      geom_bar(
        aes(y=..count../sum(..count..)),
        width=1
      )+
      scale_y_continuous(labels=percent)+
      coord_polar(theta="y")+
      scale_fill_brewer(palette = "Set2")+
      theme_minimal()+
      theme(axis.title = element_blank())+
      labs(fill="Espèce")
  })
  
  output$sepal_width = renderDataTable({
    datatable(
      data.frame(
        df %>% 
          filter(var == "Sepal.Width") %>% 
          select(-var)
      ),
      colnames = c(
        'Espece'= 'Species' ,
        'Ecart type' = 'Ecart_type'
      ),
      options = list(pageLength = 3, dom = 'tip')
    ) 
    
  })
  
  output$sepal_length = renderDataTable({
    datatable(
      data.frame(
        df %>% 
          filter(var == "Sepal.Length") %>% 
          select(-var)
      ),
      colnames = c(
        'Espece'= 'Species' ,
        'Ecart type' = 'Ecart_type'
      ),
      options = list(pageLength = 3, dom = 'tip')
    ) 
  })
  
  output$petal_width = renderDataTable({
    datatable(
      data.frame(
        df %>% 
          filter(var == "Petal.Width") %>% 
          select(-var)
      ),
      colnames = c(
        'Espece'= 'Species' ,
        'Ecart type' = 'Ecart_type'
      ),
      options = list(pageLength = 3, dom = 'tip')
    )
  })
  
  output$petal_length = renderDataTable({
    datatable(
      data.frame(
        df %>% 
          filter(var == "Petal.Length") %>% 
          select(-var)
      ),
      rownames = FALSE,
      colnames = c(
        'Espece'= 'Species' ,
        'Ecart type' = 'Ecart_type'
      ),
      options = list(pageLength = 3, dom = 'tip')
    ) 
  })
  
  output$histo = renderPlot({
    if(input$choix>1){
      ggplot(iris, aes_string(variable_a_afficher()))+
        geom_histogram(aes(y=..density..),
                       bins=input$nb_classe,
                       colour = "red",
                       fill = "steelblue"
        )+
        labs(x = "",
             y = "Densite")+
        theme_light()} 
  })    
  
  output$histo_esp = renderPlot({
    if(input$choix>1){
      ggplot(iris, aes_string(variable_a_afficher(), fill = "Species")) + 
        geom_histogram(aes(y=..density..), bins = input$nb_classe) + 
        labs(x="", 
             y="Densite")}
  })
  
  output$boxplot = renderPlot({
    if(input$choix>1){
      ggplot(iris, aes_string("Species", variable_a_afficher(), color = "Species")) +
        geom_boxplot(show.legend = FALSE) +
        labs (x = "Species", y = "Mesure") + 
        theme_light()}
    
    
  })
  
  
}

### Run the app ----
shinyApp(ui = ui, server = server)