library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)


evol_globale = txhousing %>%
    group_by(year) %>%
    summarise(volume = sum(volume, na.rm = T))

#  Boites -------------------------------------------------------

# 
# ui = dashboardPage(
# 
#     dashboardHeader(),
#     dashboardSidebar(),
#     dashboardBody(
#         box(),
# #        infoBox(),
# #        valueBox(),
#         tabBox()
#     ),
#     skin = "blue"
# )
# 
# server = function(input, output) {}
# 
# shinyApp(ui = ui, server = server)


# Boite générique -------------------------------------------------------


# ui = dashboardPage(
# 
#     dashboardHeader(),
#     dashboardSidebar(),
#     dashboardBody(
#         box(
#             title = "Evolution du volume des ventes",
#             footer = "en US$",
#             status = "info",
#             solidHeader = TRUE,
#             width = 8,
#             plotOutput("evolution")
#         )
#     ),
#     skin = "blue"
# )
# 
# server = function(input, output) {}
# 
# shinyApp(ui = ui, server = server)


# Boite générique (2) ----------------------------------------------------------------


# library(dplyr)
# evol_globale = txhousing %>%
#     group_by(year) %>%
#     summarise(volume = sum(volume, na.rm = T))
# 
# 
# 
# ui = dashboardPage(
# 
#     dashboardHeader(),
#     dashboardSidebar(),
#     dashboardBody(
#         box(
#             title = "Evolution du volume des ventes",
#             footer = "en US$",
#             status = "info",
#             solidHeader = TRUE,
#             width = 8,
#             plotOutput("evolution")
#         )
#     ),
#     skin = "blue"
# )
# 
# server = function(input, output) {
#     output$evolution = renderPlot({
#         ggplot(evol_globale, aes(year, volume)) +
#             geom_line() +
#             theme_minimal() +
#             labs(x = "Annee", y = "Volume des ventes")
#     })
# }
# 
# shinyApp(ui = ui, server = server)



# Boite information -------------------------------------------------------

# ui = dashboardPage(
# 
#     dashboardHeader(),
#     dashboardSidebar(),
#     dashboardBody(
#         infoBox(
#             title = "La progression est de ",
#             value = textOutput("progression"),
#             subtitle = "Entre 2000 et 2015",
#             icon = icon("line-chart"),
#             fill = TRUE,
#             color = "light-blue",
#             width = 4
#         )
#     ),
#     skin = "blue"
# )
# 
# server = function(input, output) {
#     output$progression = renderText({
#         paste(round(
#             tail(evol_globale$volume, 1) /
#                 head(evol_globale$volume, 1) *
#                 100
#         ),
#         "%")
#     })
# }
# 
# shinyApp(ui = ui, server = server)




# Boite valeur et Boite valeur(2) ------------------------------------------------------------

# ui = dashboardPage(
# 
#     dashboardHeader(),
#     dashboardSidebar(),
#     dashboardBody(
#         valueBox(
#             value = textOutput("volume"),
#             subtitle = "Volume totale des ventes (en milliards)",
#             icon = icon("usd"),
#             color = "green",
#             width = 4
#         )
#     ),
#     skin = "blue"
# )
# 
# server = function(input, output) {
#     output$volume = renderText({
#         round(
#             sum(evol_globale$volume, na.rm = T) / 1e+9, 1
#         )
#     })
# 
# }
# 
# shinyApp(ui = ui, server = server)
# 


# Boite tableau (2) et Boite tableau (3)-----------------------------------------------------------
ui = dashboardPage(

    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody(
        tabBox(title = "Informations",
               width = 8,
               tabPanel(title = "Prix médian",
                        tableOutput("info_prix")
               ),
               tabPanel(title = "Nombre",
                        tableOutput("info_nombre")
               )
        )
    ),
    skin = "blue"
)

server = function(input, output) {
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
