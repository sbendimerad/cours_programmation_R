library(shiny)
library(shinydashboard)
library(ggplot2)


# TP 1 -------------------------------------------------------


ui = dashboardPage(
    
    dashboardHeader(
        title = "Tableau de bord pour IRIS",
        titleWidth = 300
    ),
    dashboardSidebar(
        sliderInput("bins",
                    label = "Nombre de classes :",
                    min = 1,
                    max = 50,
                    value = 30)
    ),
    dashboardBody(
        plotOutput("distPlot")
    ),
    skin = "blue"
)


server = function(input, output) {
    output$distPlot = renderPlot({
        ggplot(iris, aes(Sepal.Length))+
            geom_histogram(aes(y=..density..),
                           bins=input$bins,
                           colour = "red",
                           fill = "steelblue"
            )+
            labs(x = "longueur Sepal",
                 y = "Densite",
                 title = "Histogramme longueur Sepal"
            )+
            theme_light()
    })
}
shinyApp(ui = ui, server = server)