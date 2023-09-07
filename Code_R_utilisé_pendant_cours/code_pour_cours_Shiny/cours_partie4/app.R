library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)



# Interaction avec l'utilisateur (2) --------------------------------------

# 
# evol_globale = txhousing %>%
#     group_by(year) %>%
#     summarise(volume = sum(volume, na.rm = T))
# 
# ui = dashboardPage(
#     dashboardHeader(
#         title = "Ventes immobilières au Texas",
#         titleWidth = 300
#     ),
#     dashboardSidebar(),
#     dashboardBody(
#         box(
#             title = "Evolution du volume des ventes",
#             footer = "en US$",
#             status = "info",
#             solidHeader = TRUE,
#             width = 8,
#             plotOutput("evolution")
#         ),
#         box(
#             width = 4,
#             selectInput("ville",
#                         "Ville choisie",
#                         choices = c(
#                             "Toutes les villes",
#                             unique(txhousing$city)
#                         )
#             )
#         ),
#         infoBox(
#             title = "Progression",
#             value = textOutput("progression"),
#             subtitle = "Entre 2000 et 2015",
#             icon = icon("line-chart"),
#             fill = TRUE,
#             color = "light-blue",
#             width = 4
#         ),
#         valueBox(
#             value = textOutput("volume"),
#             subtitle = "Volume total des ventes (en milliards)",
#             icon = icon("usd"),
#             color = "green",
#             width = 4
#         ),
#         tabBox(title = "Informations",
#                width = 4,
#                tabPanel(title = "Prix médian",
#                         tableOutput("info_prix")
#                ),
#                tabPanel(title = "Nombre",
#                         tableOutput("info_nombre")
#                )
#         )
#     ),
#     title = "Texas Housing",
#     skin = "yellow"
# )
# 
# server = function(input, output) {
# 
#     output$evolution = renderPlot({
#         ggplot(evol_globale, aes(year, volume)) +
#             geom_line() +
#             theme_minimal() +
#             labs(x = "Annee", y = "Volume des ventes")
#     })
# 
#     output$progression = renderText({
#         paste(round(
#             tail(evol_globale$volume, 1) /
#                 head(evol_globale$volume, 1) *
#                 100
#         ),
#         "%")
#     })
# 
#     output$volume = renderText({
#         round(
#             sum(evol_globale$volume, na.rm = T) / 1e+9, 1
#         )
#     })
# 
#     output$info_prix = renderTable({
#         data.frame(
#             Statistique = c("Minimum", "Médiane", "Maximum"),
#             Valeur = c(
#                 min(txhousing$median, na.rm = T),
#                 median(txhousing$median, na.rm = T),
#                 max(txhousing$median, na.rm = T)
#             )
#         )
#     })
# 
#     output$info_nombre = renderTable({
#         data.frame(
#             Statistique = c("Minimum", "Médiane", "Maximum"),
#             Valeur = c(
#                 min(txhousing$sales, na.rm = T),
#                 median(txhousing$sales, na.rm = T),
#                 max(txhousing$sales, na.rm = T)
#             )
#         )
#     })
# 
# }
# 
# shinyApp(ui = ui, server = server)



#  Interaction avec l'utilisateur (3), (4) et (5) -------------------------


evol_globale = txhousing %>%
  group_by(year) %>%
  summarise(volume = sum(volume, na.rm = T))



ui = dashboardPage(
  dashboardHeader(
    title = "Ventes immobilières au Texas",
    titleWidth = 300
  ),
  dashboardSidebar(),
  dashboardBody(
    box(
      title = "Evolution du volume des ventes",
      footer = "en US$",
      status = "info",
      solidHeader = TRUE,
      width = 8,
      plotOutput("evolution")
    ),
    box(
      width = 4,
      selectInput("ville",
                  "Ville choisie",
                  choices = c(
                    "Toutes les villes",
                    unique(txhousing$city)
                  )
      )
    ),
    infoBox(
      title = "Progression",
      value = textOutput("progression"),
      subtitle = "Entre 2000 et 2015",
      icon = icon("line-chart"),
      fill = TRUE,
      color = "light-blue",
      width = 4
    ),
    valueBox(
      value = textOutput("volume"),
      subtitle = "Volume total des ventes (en milliards)",
      icon = icon("usd"),
      color = "green",
      width = 4
    ),
    tabBox(title = "Informations",
           width = 4,
           tabPanel(title = "Prix médian",
                    tableOutput("info_prix")
           ),
           tabPanel(title = "Nombre",
                    tableOutput("info_nombre")
           )
    )
  ),
  title = "Texas Housing",
  skin = "yellow"
)

server = function(input, output) {

  output$evolution = renderPlot({
    
    if (input$ville == "Toutes les villes") {
      evol = evol_globale
    } else {
      evol = txhousing %>%
        filter(city == input$ville) %>%
        group_by(year) %>%
        summarise(volume = sum(volume, na.rm = T))
    }
    
    
    ggplot(evol, aes(year, volume)) +
      geom_line() +
      theme_minimal() +
      labs(x = "", y = "Volume des ventes")
  })

  output$progression = renderText({
    
    if (input$ville == "Toutes les villes") {
      evol = evol_globale
    } else {
      evol = txhousing %>%
        filter(city == input$ville) %>%
        group_by(year) %>%
        summarise(volume = sum(volume, na.rm = T))
    }
    
    paste(round(tail(evol$volume, 1) /
                  head(evol$volume, 1) * 100),
          "%")
  })

  output$volume = renderText({
    
    if (input$ville == "Toutes les villes") {
      evol = evol_globale
    } else {
      evol = txhousing %>%
        filter(city == input$ville) %>%
        group_by(year) %>%
        summarise(volume = sum(volume, na.rm = T))
    }
    
    round(
      sum(evol_globale$volume, na.rm = T) / 1e+9, 1
    )
  })

  output$info_prix = renderTable({
    
    data.frame(
      Statistique = c("Minimum", "Médiane", "Maximum"),
      Valeur = c(
        min(txhousing$median, na.rm = T),
        median(txhousing$median, na.rm = T),
        max(txhousing$median, na.rm = T)
      )
    )
  })

  output$info_nombre = renderTable({
    data.frame(
      Statistique = c("Minimum", "Médiane", "Maximum"),
      Valeur = c(
        min(txhousing$sales, na.rm = T),
        median(txhousing$sales, na.rm = T),
        max(txhousing$sales, na.rm = T)
      )
    )
  })

}

shinyApp(ui = ui, server = server)





