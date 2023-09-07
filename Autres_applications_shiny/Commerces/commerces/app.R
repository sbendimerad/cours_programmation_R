library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(forcats)

load("commercesParis.RData")

###############################
# Changement du type ARONDISSEMENT pour traitement et affichage
commerces$ARRONDISSEMENT = as.integer(commerces$ARRONDISSEMENT)

###############################
# Calcul de la table résumé
table_complete = commerces %>% 
  group_by(ARRONDISSEMENT, `LIBELLE ACTIVITE`) %>% 
  summarise(
    n = n()
  )

resume = data.frame(
  table_complete %>% 
  group_by(ARRONDISSEMENT) %>% 
  summarise(
    Nombre_de_commerces = sum(n),
    Commerce_le_plus_present = `LIBELLE ACTIVITE`[n==max(n)]
  )
)

ui = dashboardPage(
  ################################
  # Titre
  dashboardHeader(
    title = "Les commerces à Paris",
    titleWidth = 235
  ),
  
  ################################
  # Menu
  dashboardSidebar(
    sidebarMenu(
      # Résumé des données
      menuItem("Table résumé",
               tabName = "resume",
               icon = icon("city")
      ),
      
      # Top 10 et Bottom 5
      menuItem("Types de commerces",
               tabName = "topbottom",
               icon = icon("signal"))
    )
  ),
  
  ################################
  # Page principale
  dashboardBody(
    tabItems(
      # Page résumé
      tabItem("resume",
              fluidRow(
                tabBox(
                  tabPanel(
                  title = "Résumé des commerces par arrondissement",
                  tableOutput("resume")
                  ),
                  width = 8
                )
              )
      ),
      
      # Page top/bottom
      tabItem("topbottom",
              fluidRow(
                box(
                  selectInput("arrondissement",
                              "Choisir un arrondissement",
                              choices = c(
                                "Tous les arrondissements",
                                sort(unique(commerces$ARRONDISSEMENT))
                              )
                  ),
                  width = 10
                ),
                box(
                  title = "Les 10 types de commerce les plus présents",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("top10")
                ),
                box(
                  title = "Les 10 types de commerce les moins présents",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("bottom10")
                )
              )
      )
    )
  ),
  title = "Commerces sur Paris",
  skin = "red"
)


server = function(input, output) {
   output$resume = renderTable({resume})
   
   donnees = reactive({
     if(input$arrondissement == "Tous les arrondissements"){
       resum = commerces %>% 
         group_by(`LIBELLE ACTIVITE`) %>% 
         summarise(nombre = n())
     } else {
       resum = commerces %>% 
         filter(ARRONDISSEMENT == input$arrondissement) %>% 
         group_by(`LIBELLE ACTIVITE`) %>% 
         summarise(nombre = n())
     }
     resum
   })
   
   output$top10 = renderPlot({
     top10_activites = donnees() %>% 
       arrange(desc(nombre)) %>% 
       slice(1:10)
     
     ggplot(
       top10_activites, 
       aes(
         fct_reorder(`LIBELLE ACTIVITE`, nombre), 
         nombre
       )
     )+
       geom_col()+
       labs(
         title = " ",
         x = " ",
         y = "Effectif"
       )+
       coord_flip()+
       theme_light()
   })
   
   output$bottom10 = renderPlot({
     bottom10_activites = donnees() %>% 
       arrange(nombre) %>% 
       slice(1:10)
     
     ggplot(
       bottom10_activites, 
       aes(
         fct_reorder(`LIBELLE ACTIVITE`, nombre, .desc = TRUE), 
         nombre
       )
     )+
       geom_col()+
       labs(
         title = " ",
         x = " ",
         y = "Effectif"
       )+
       coord_flip()+
       theme_light()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
