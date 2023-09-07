library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)

load("Football_2009_2016.RData")

championnat = Football %>% filter(Championship == "Italy") %>% select(-Championship)

ui = dashboardPage(
    dashboardHeader(
        title = "Championnat de Football Italien (2009-2016)",
        titleWidth =  450
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Statistiques des joueurs",
                     tabName = "joueurs",
                     icon = icon("futbol")
            ),
            menuItem("Analyse agrégée",
                     tabName = "analyse",
                     icon = icon("copy")
            )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem("joueurs",
                    fluidRow(
                        box(
                            selectInput("club",
                                        "Choisir un club",
                                        choices = sort(unique(championnat$Club))
                                            )
                        ),
                        box(
                            selectInput("saison",
                                        "Choisir une saison",
                                        choices = sort(unique(championnat$year))
                            )
                        ),
                        tabBox(
                            tabPanel(title = "Statistiques des joueurs",
                                     dataTableOutput("joueurs")
                                     ),
                            width = 11
                        )
                    )
            ),
            tabItem("analyse",
                    fluidRow(
                        box(
                            selectInput("club2",
                                        "Choisir un club",
                                        choices = sort(unique(championnat$Club))
                            )
                        ),
                        box(
                            selectInput("variable",
                                        "Choisir une statistique",
                                        choices = c(
                                            "Nombre moyen de passes décisives",
                                            "Nombre moyen de cartons jaunes", 
                                            "Nombre moyen de cartons rouges",
                                            "Nombre moyen de tirs par match",
                                            "Nombre moyen d'homme du match",
                                            "Note moyenne"
                                        )
                            )
                        ),
                       box(
                           title = "Evolution moyenne entre 2009 et 2016",
                           plotOutput("evolsaison")
                       ),
                       box(
                           title = "Nombre moyen de buts sur la saison en fonction de la statistique",
                           plotOutput("evolbuts")
                       ),
                       box(
                           selectInput("position",
                                       "Choisir une position de jeu",
                                       choices = c(
                                           "Tous les joueurs",
                                           "Attaquant",
                                           "Milieu",
                                           "Défenseur",
                                           "Gardien de but"
                                       )
                           )
                       ),
                       infoBox(
                           title = "Progression",
                           value = textOutput("progression"),
                           subtitle = "Entre 2009 et 2016",
                           icon = icon("line-chart"),
                           fill = TRUE,
                           color = "aqua"
                       )
                    )
            )
        )
    ),
    title = "Données de foot",
    skin = "green"
)

# Define server logic required to draw a histogram
server = function(input, output) {
    
    output$joueurs = renderDataTable({
        table = championnat %>% filter(Club == input$club & year == input$saison) %>%
            select(-c(year, Club, `Position 2`, `Position 3`))
        table$R = as.integer(table$R)
        table$CM = as.integer(table$CM)
        table$KG = as.integer(table$KG)
        table$Mins = as.integer(table$Mins)
        table$Goals = as.integer(table$Goals)
        table$Assists = as.integer(table$Assists)
        table$Yel = as.integer(table$Yel)
        table$Red = as.integer(table$Red)
        table$MotM = as.integer(table$MotM)
        datatable(
        data.frame(
            table
        ),
        rownames = FALSE,
        extensions = 'FixedColumns',
        options = list(
            dom = 'tip',
            scrollX = TRUE,
            fixedColumns = list(leftColumn = 1, rightColumns = 1)
        )
        ) %>% 
            formatStyle('Player',
                        color = 'white',
                        backgroundColor = 'steelblue',
                        fontWeight = 'bold')
    })
    
    position = reactive({
        if(input$position == "Tous les joueurs"){
            donnees = championnat %>% 
                filter(Club == input$club2)
        }
        if(input$position == "Attaquant"){
            donnees = championnat %>% 
                filter(Club == input$club2 & Position == "F")
        }
        if(input$position == "Milieu"){
            donnees = championnat %>% 
                filter(Club == input$club2 & Position == "M")
        }
        if(input$position == "Défenseur"){
            donnees = championnat %>% 
                filter(Club == input$club2 & Position == "D")
        }
        if(input$position == "Gardien de but"){
            donnees = championnat %>% 
                filter(Club == input$club2 & Position == "GK")
        }
        donnees
    })

    analyse = reactive({
        if(input$variable == "Nombre moyen de passes décisives"){
            buts = position() %>% group_by(year) %>% 
                summarize(Buts=mean(Goals, na.rm = TRUE), Variable = mean(Assists, na.rm = TRUE))
        }
        if(input$variable == "Nombre moyen de cartons jaunes"){
            buts = position() %>% group_by(year) %>% 
                summarize(Buts=mean(Goals, na.rm = TRUE), Variable = mean(Yel, na.rm = TRUE))
        }
        if(input$variable == "Nombre moyen de cartons rouges"){
            buts = position() %>% group_by(year) %>% 
                summarize(Buts=mean(Goals, na.rm = TRUE), Variable = mean(Red, na.rm = TRUE))
        }
        if(input$variable == "Nombre moyen de tirs par match"){
            buts = position() %>% group_by(year) %>% 
                summarize(Buts=mean(Goals, na.rm = TRUE), Variable = mean(SpG, na.rm = TRUE))
        }
        if(input$variable == "Nombre moyen d'homme du match"){
            buts = position() %>% group_by(year) %>% 
                summarize(Buts=mean(Goals, na.rm = TRUE), Variable = mean(MotM, na.rm = TRUE))
        }
        if(input$variable == "Note moyenne"){
            buts = position() %>% group_by(year) %>% 
                summarize(Buts=mean(Goals, na.rm = TRUE), Variable = mean(Rating, na.rm = TRUE))
        }
        buts
    })
    
    output$evolsaison = renderPlot({
       ggplot(analyse(), aes(year, Variable))+
           geom_line(color = "steelblue")+
           labs(title = "",
                x = "Saison",
                y = "Statistique")+
           theme_light()
    })
    
    output$evolbuts = renderPlot({
        ggplot(analyse(), aes(Variable, Buts))+
            geom_point(color = "red")+
            geom_text(aes(x = Variable, y = Buts+0.1, label = year))+
            labs(title = "",
                 x = "Statistique par saison",
                 y = "Nombre moyen de buts par saison")+
            theme_light()
    })
    
    output$progression = renderText({
        paste(round(
            tail(analyse()$Variable,1)/head(analyse()$Variable,1)*100
        ),
         "%"   
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
