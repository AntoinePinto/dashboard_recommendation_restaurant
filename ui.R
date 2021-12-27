library(bslib)
df <- read.table( 
    text = readLines("df2.txt"), 
    header = TRUE,  
    sep = ";" 
)
review <- read.table( 
    text = readLines("review2.txt"), 
    header = TRUE,  
    sep = ";" 
)
ui <- fluidPage(
    tags$head(tags$style(HTML("
        .selectize-input, .selectize-dropdown {
          font-size: 75%;
        }
        "))),  
    theme = bs_theme(
        bg = "white", fg = "black",
        base_font = font_google("Righteous")),
    titlePanel("Trouvez le restaurant qui vous correspond !"),
    sidebarLayout(
        sidebarPanel(
            title = "Main parameters", status = "warning", solidHeader = TRUE, width = 4,
            textInput("keyword", label = h3("Mots-clés"), value = ""),
            fixedRow(
                column(12,
                       fixedRow(
                           column(4,
                                  selectInput(inputId = "jour", label = "Choisir une journée ?",
                                              choices = c("Monday", "Tuesday", "Wednesday",
                                                          "Thursday", "Friday", "Saturday", "Sunday"))
                           ),
                           column(8,
                                  sliderInput("heure",  "Heure",
                                              min = 0, max = 23, value = 21)
                           )
                       )
                )
            ),
            fixedRow(
                column(12,
                       fixedRow(
                           column(4,
                                  selectInput(inputId = "city", label = "Choisir une ville:",
                                              choices = unique(df$city))
                           ),
                           column(8,
                                  sliderInput("importance",  "Ville - importance",
                                              min = 0, max = 100, value = 30)
                           )
                       )
                )
            ),
            
            sliderInput("popularity",  "Importance des avis",
                        min = 0, max = 10, value = 5),
            fixedRow(
                column(12,
                       fixedRow(
                           column(4,
                                  sliderInput("prix",  "Niveau des prix",
                                              min = 1, max = 4, value = c(2, 3))
                           ),
                           column(8,
                                  sliderInput("v_prix",  "Importance du prix",
                                              min = 0, max = 100, value = 30)
                           )
                       )
                )
            ),
            fixedRow(
                column(12,
                       fixedRow(
                           column(4,
                                  selectInput(inputId = "service", label = "Quel service ?",
                                              choices = c("lunch/dinner", "breakfast/brunch"),
                                              selected = "lunch/dinner")
                           ),
                           column(8,
                                  sliderInput("v_service",  "Importance du service",
                                              min = 0, max = 100, value = 30)
                           )
                       )
                )
            ),
            fixedRow(
                column(12,
                       fixedRow(
                           column(4,
                                  selectInput(inputId = "ambiance", label = "Quelle ambiance ?",
                                              choices = unique(df$ambiance),
                                              selected = "casual")
                           ),
                           column(8,
                                  sliderInput("v_ambiance",  "Importance de l'ambiance",
                                              min = 0, max = 100, value = 30)
                           )
                       )
                )
            ),
            fixedRow(
                column(12,
                       fixedRow(
                           column(4,
                                  sliderInput("bruit",  "Niveau du bruit",
                                              min = 1, max = 4, value = c(2, 3))
                           ),
                           column(8,
                                  sliderInput("v_bruit",  "Importance du critère",
                                              min = 0, max = 100, value = 30)
                           )
                       )
                )
            ),
            checkboxGroupInput(
                inputId = "autres",
                label = "Autres attributs", 
                choices = colnames(df)[7:15],
                inline = TRUE
            )
        ),
        mainPanel(
            h1("Le Top 5 des restaurants faits pour vous :"),
            tableOutput("summary"),
            fluidRow(
                column(width = 6,
                       h3("TOP 1 Restaurant - Pureté d'ajustement aux préferences:"),
                       plotOutput("distPlot2")
                ),
                column(width = 6,
                       h3("TOP 1 Restaurant - Nuage de mot:"),
                       plotOutput("distPlot",width = "500px", height="375px")
                )
            ),
        )
    )
)