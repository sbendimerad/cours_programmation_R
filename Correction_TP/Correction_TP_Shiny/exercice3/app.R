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
# Univariate description
iris_longue = iris %>% gather(-Species, key = "var", value = "Valeur")
df = iris_longue %>% group_by(Species, var) %>% 
    summarise(
        Moyenne = round(mean(Valeur, na.rm = T),2),
        Mediane = round(median(Valeur, na.rm=T),2),
        Ecart_type = round(sd(Valeur, na.rm = T), 2)
    )

df1 = iris %>% group_by(Species) %>% 
    summarise(
        Moyenne=mean(Sepal.Length, na.rm = T),
        Ecart_type = sd(Sepal.Length, na.rm = T)
    )

### Define UI ----
ui = dashboardPage(
    dashboardHeader(
        title = h3("Les iris de Fisher"),
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
                color = "yellow",
                width = 8
            ),
            infoBox(
                title = "Les données",
                value = textOutput("donnees"),
                icon = icon("leaf"),
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
                )
                
                
                )        
        
        )),
    title = "Informations sur les iris de Fisher",
    skin = "purple"
)

### Define server logic ----
server = function(input, output) {
    
    output$donnees = renderText({
        paste(c(nrow(iris)," iris, dont ",
                iris %>% filter(Species=="setosa") %>% nrow,
                "Setosa, ",
                iris %>% filter(Species=="versicolor") %>% nrow,
                "Versicolor et ",
                iris %>% filter(Species=="virginica") %>% nrow,
                "Virginica"
        )
        )
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
        ggplot(iris, aes(Sepal.Length))+
            geom_histogram(aes(y=..density..),
                           bins=8,
                           colour = "red",
                           fill = "steelblue"
            )+
            labs(x = "longueur Sepal",
                 y = "Densite",
                 title = "Histogramme longueur Sepal"
            )+
            theme_light()    
    })    
    
    output$histo_esp = renderPlot({
        ggplot(iris, aes(Sepal.Length, color = Species, fill = Species)) + 
            geom_histogram(aes(y=..density..), bins = 8) + 
            labs(x="", 
                 y="Densite")
    })
    
    
    output$boxplot = renderPlot({
        
        ggplot(iris, aes(Species, Sepal.Length, color = Species)) +
            scale_color_brewer(palette = "Set2") +
            scale_fill_brewer(palette = "Set2") +
            geom_boxplot(show.legend = FALSE) +
            geom_errorbar(data = df1,
                          mapping = aes(y = Moyenne,
                                        ymin = Moyenne - Ecart_type,
                                        ymax = Moyenne + Ecart_type),
                          col = "gray70", width = .4, size = 1) +
            geom_point(data = df1,
                       mapping = aes(y = Moyenne),
                       col = "steelblue", size = 2) +
            theme_light()
        
        
    })
    
    
}

### Run the app ----
shinyApp(ui = ui, server = server)