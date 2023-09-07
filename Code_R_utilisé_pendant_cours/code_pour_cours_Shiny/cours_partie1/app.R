library(shiny)
library(shinydashboard)
library(ggplot2)

#

# Exemple 1  --------------------------------------------------------------


# ui =
# server = function(input, output) {}
# shinyApp(ui = ui, server = server)


# Exemple 2  --------------------------------------------------------------


# ui = dashboardPage(
#     dashboardHeader(),
#     dashboardSidebar(),
#     dashboardBody(),
#     title = "Titre dans le navigateur",
#     skin = "yellow"
# )
# server = function(input, output) {}
# shinyApp(ui = ui, server = server)


# Exemple 3 ---------------------------------------------------------------


ui = dashboardPage(
    dashboardHeader(
        title = "Ventes immobilières au Texas",
        titleWidth = 300
    ),
    dashboardSidebar(),
    dashboardBody(),
    title = "Titre dans le navigateur",
    skin = "yellow"
)
server = function(input, output) {}
shinyApp(ui = ui, server = server)


# Exemple 4  --------------------------------------------------------------

# 
# ui = dashboardPage(
#     dashboardHeader(
#         title = "Ventes immobilières au Texas",
#         titleWidth = 300
#     ),
#     dashboardSidebar(
#         # Input : nombre de classes ----
#         sliderInput("bins",
#                     label = "Nombre de classes :",
#                     min = 1,
#                     max = 50,
#                     value = 30)
#     ),
#     dashboardBody(),
#     title = "Titre dans le navigateur",
#     skin = "yellow"
# )
# server = function(input, output) {}
# shinyApp(ui = ui, server = server)


# Exemple 5 ---------------------------------------------------------------


ui = dashboardPage(
    dashboardHeader(
        title = "Ventes immobilières au Texas",
        titleWidth = 300
    ),
    dashboardSidebar(
        # Input : nombre de classes ----
        sliderInput("bins",
                    label = "Nombre de classes :",
                    min = 1,
                    max = 50,
                    value = 30)
    ),
    dashboardBody(plotOutput("distPlot")),
    title = "Titre dans le navigateur",
    skin = "yellow"
)
server = function(input, output) {
    output$distPlot = renderPlot({
        ggplot(txhousing, aes(sales))+
            geom_histogram(aes(y=..density..),
                           bins=input$bins,
                           colour = "white",
                           fill = "steelblue"
            )+
            labs(x = "Nombre de ventes",
                 y = "Densite",
                 title = "Histogramme du nombre de ventes"
            )+
            theme_light()
    })
}

shinyApp(ui = ui, server = server)

