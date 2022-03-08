library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinycssloaders)




liste <-lapply(list.files("C:/Users/laure/Documents/M2 TIDE/STAGE/STAGE R/PROJET", pattern = "*.csv"), function(x) read.csv(x))

df <- do.call("rbind", liste) %>% 
  mutate(genre = gsub("Shounen Ai", "shounen Ai", genre)) %>% 
  mutate(genre = gsub("Shoujo Ai", "shoujo Ai", genre)) %>% distinct()

vect_genre <- df %>% separate(genre, into = letters[1:13] ,sep=", ", fill = "right") %>%
  select(a:m) %>% gather() %>% drop_na() %>% count(value) %>% select(value)


vect_type <- df %>% count(type) %>% select(type)


#UI--------------
ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "ANIME GUIDE"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Avant-Propos", icon = icon("exclamation"), tabName = "apropo"), #liste des nom des icones : https://fontawesome.com/v6.0/icons?s=solid%2Cbrands
                        menuItem("Application", icon = icon("desktop"), tabName = "app") # autre liste icon (rajoutÃ© lib = "glyphicon") https://getbootstrap.com/docs/3.4/components/#glyphicons
                        
                        )
                    ),
                    dashboardBody(
                      tabItems(
                        
                        tabItem(tabName = "apropo", h1("Bonjour!"),
                                h3("Cette application a pour but de recommander a l'utilisateur des animes en fonction de ses gouts. Il est libre de choisir le genre et le type d'anime qu'il souhaite voir, ainsi que la quantite d'anime qui lui sera recommande.  Il peut decider de classer les animes selon leur popularite (nombre de membres) ou par rapport a leur note generale (rating)."),
                                ),
                        
                        
                        tabItem(tabName = "app",
                                
                                h1("Application"),
                                
                                
                                fluidRow(
                                  column(3,
                                         
                                         selectInput("genre",
                                                     h3("Selectionnez votre genre :"),
                                                     choices = vect_genre,
                                                     selected = "Action" )
                                
                                          
                                         ),
                                  column(3,
                                         
                                         selectInput("type",
                                                     h3("Choix du type :"),
                                                     choices = vect_type,
                                                     selected = "Movie" )
                                
                                          
                                         ),
                                  column(3,selectInput("type_class",
                                                       h3("Type de Classement:"),
                                                       choices = c("Popularité" = "members",
                                                                   "Note sur 10" = "rating_10" ,
                                                                   "Note sur 150" = "rating_100",
                                                                   "Note sur 1000" = "rating_1000"),
                                                       selected = "members" ))
                                  ),
                                
                                radioButtons(inputId = "top",
                                             label = "Top:",
                                             choices = c("Top 5" = 5, "Top 10" = 10, "Top 50" = 50, "Top 100" = 100),
                                             selected = 5, inline = T),
                               
                                withSpinner(dataTableOutput(outputId = "table")),
                                
                                h3("Repartition"),
                                
                                withSpinner(plotOutput(outputId = "graph"))
                                
                                )
                        
                      )
                    )                    
)

#Server--------------

server <- function(input, output){
  
df_filter <-   reactive({ df %>% mutate(Valeurs = df[input$type_class]) %>% filter(grepl(input$genre, genre))  %>%
        filter(type == input$type)  %>% arrange(desc(Valeurs)) %>%
         head(as.numeric(input$top))
        })


df_filter_2 <- reactive({df_filter() %>% mutate(Rang = 1:nrow(df_filter())) %>% select(Rang,name,genre,input$type_class,type)})


prop_genre <- function(genre_ = "Shounen"){
  
  count_type <- df %>% count(type)
  
  df %>% mutate(x = ifelse(grepl(genre_,genre), 1, 0)) %>%
    select(type, genre, x) %>% group_by(type) %>%
    summarise(genre_ = sum(x)) %>%
    mutate(sous_total = count_type[[2]], proportion = scales::percent(genre_/sous_total)) %>%
    as.data.frame()
}

prop <- reactive({prop_genre(input$genre)})
                   
  output$table <- renderDataTable({

    df_filter_2()
    
  })
  
  output$graph <- renderPlot({
    ggplot(prop(), aes(x = type, y = genre_, fill = type)) + geom_col() +
      labs(x = "Type", y = "Effectifs",title = paste("Effectif par type du genre:", input$genre),
           subtitle = paste("Proportion du genre",input$genre, "pour chaque type (en rouge)")) +
      geom_text(aes(label=genre_), position=position_dodge(width=0.9), vjust=-0.5) + geom_text(aes(label=proportion), position=position_dodge(width=0.9), vjust=-0.5, hjust=-1.0, color="red")
  
    
    })
}

shinyApp(ui,server)