library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)



# Tableaux : Présentation améliorée (1) et (2)  --------------------------------------


# Tableaux : Présentation améliorée (3) et (4)  --------------------------------------

ui = dashboardPage(
  dashboardHeader(
    title = "Ventes immobilières au Texas",
    titleWidth = 300
  ),
  dashboardSidebar(
    menuItem("Tableau", tabName = "donnees",
             icon = icon("table")
    )
  ),
  dashboardBody(
    tabItem(
      "donnees",
      dataTableOutput("tableau")
    )
  ),
  title = "Texas Housing",
  skin = "yellow"
)

server = function(input, output) {

  output$tableau <- renderDataTable({
    datatable(txhousing,
              rownames = FALSE,
              colnames = c('Ville' = 'city',
                           'Année' = 'year',
                           'Mois' = 'month'),
              caption = "Données concernant
les ventes immobilières au Texas
- 2000-2015",
              filter = 'top')
  })
}

shinyApp(ui = ui, server = server)

# Tableaux : Présentation améliorée (5) et (6)  --------------------------------------

# ui = dashboardPage(
#   dashboardHeader(
#     title = "Ventes immobilières au Texas",
#     titleWidth = 300
#   ),
#   dashboardSidebar(
#     menuItem("Tableau", tabName = "donnees",
#              icon = icon("table")
#     )
#   ),
#   dashboardBody(
#     tabItem(
#       "donnees",
#       dataTableOutput("tableau")
#     )
#   ),
#   title = "Texas Housing",
#   skin = "yellow"
# )
# 
# server = function(input, output) {
# 
#   output$tableau <- renderDataTable({
#     datatable(txhousing %>%
#                 mutate(date = date_decimal(date))
#     ) %>%
#       formatCurrency(c("volume", "median")) %>%
#       formatDate("date", "toLocaleDateString") %>%
#       formatStyle('city',
#                   color = 'white',
#                   backgroundColor = 'slategrey',
#                   fontWeight = 'bold') %>%
#       formatStyle('median',
#                   background = styleColorBar(
#                     range(txhousing$median, na.rm = TRUE),
#                     'lightblue')
#       )
#   })
# }
# 
# shinyApp(ui = ui, server = server)
# 


# # Tableaux : Présentation améliorée (7)  --------------------------------------
#
# ui = dashboardPage(
#   dashboardHeader(
#     title = "Ventes immobilières au Texas",
#     titleWidth = 300
#   ),
#   dashboardSidebar(
#     menuItem("Tableau", tabName = "donnees",
#              icon = icon("table")
#     )
#   ),
#   dashboardBody(
#     tabItem(
#       "donnees",
#       dataTableOutput("tableau")
#     )
#   ),
#   title = "Texas Housing",
#   skin = "yellow"
# )
# 
# server = function(input, output) {
# 
#   output$tableau <- renderDataTable({
#     datatable(
#       txhousing,
#       rownames = FALSE,
#       extensions = 'FixedColumns',
#       options = list(
#         dom = 't',
#         scrollX = TRUE,
#         fixedColumns = list(leftColumns = 1, rightColumns = 1)
#       )
#     )
# 
#   })
# }

 shinyApp(ui = ui, server = server)

