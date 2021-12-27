library(stringr)
library(ggplot2)
library(dplyr)
library(wordcloud)
library(shiny)
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
server <- function(input, output) {
    output$summary <- renderTable({
        v_wgt <- rep(0, 15)
        names(v_wgt) <- c(colnames(df)[c(7:15)], "city_", "popularity", "price", "ambiance_", "service_", "NoiseLevel_")
        keyword <- input$keyword
        day <- input$jour
        hours <- input$heure
        keyword <- ifelse(sum(str_detect(str_to_lower(df$categories), str_to_lower(keyword))) > 0, keyword, "")
        df <- df[str_detect(str_to_lower(df$categories), str_to_lower(keyword)) & (df[, paste(day, "_open", sep = "")] <= hours) & (df[, paste(day, "_close", sep = "")] > hours),] %>% filter(!is.na(business_id))
        df$city_ <- df$city == input$city
        df$price <- df$RestaurantsPriceRange2 >= input$prix[1] & df$RestaurantsPriceRange2 <= input$prix[2]
        df$ambiance_ <- df$ambiance == input$ambiance
        df$service_ <- df$service == input$service
        df$NoiseLevel_ <- df$NoiseLevel %in% input$bruit
        v_wgt[names(v_wgt) == "city_"] <- input$importance
        v_wgt[names(v_wgt) == "popularity"] <- input$popularity
        v_wgt[names(v_wgt) == "price"] <- input$v_prix
        v_wgt[names(v_wgt) == "ambiance_"] <- input$v_ambiance
        v_wgt[names(v_wgt) == "service_"] <- input$v_service
        v_wgt[names(v_wgt) == input$autres] <- input$v_bruit
        contri <- sweep(df[, names(v_wgt)], MARGIN = 2, v_wgt, '*')
        contri$score <- rowSums(contri, na.rm = T)
        df$score <- contri$score
        contri <- arrange(contri, desc(score))
        df[df == FALSE] <- "Non"
        df[df == TRUE] <- "Oui"
        df[is.na(df)] <- "Inconnu"
        df %>% 
            arrange(desc(score)) %>%
            slice(1:5) %>%
            mutate("Top Restaurant" = 1:5) %>%
            select("Top Restaurant", "name", "city", "stars", "review_count", "RestaurantsPriceRange2",
                   "ambiance", "service", input$autres, "score") %>%
            rename("Nom du restaurant" = "name",
                   "Ville" = "city",
                   "Étoiles" = "stars",
                   "Nombre d'avis" = "review_count",
                   "Échelle de prix" = "RestaurantsPriceRange2",
                   "Ambiance" = "ambiance",
                   "Service" = "service",
                   "Score" = "score")
    })
    output$distPlot <- renderPlot({
        v_wgt <- rep(0, 15)
        names(v_wgt) <- c(colnames(df)[c(7:15)], "city_", "popularity", "price", "ambiance_", "service_", "NoiseLevel_")
        keyword <- input$keyword
        day <- input$jour
        hours <- input$heure
        keyword <- ifelse(sum(str_detect(str_to_lower(df$categories), str_to_lower(keyword))) > 0, keyword, "")
        df <- df[str_detect(str_to_lower(df$categories), str_to_lower(keyword)) & (df[, paste(day, "_open", sep = "")] <= hours) & (df[, paste(day, "_close", sep = "")] > hours),] %>% filter(!is.na(business_id))
        df$city_ <- df$city == input$city
        df$price <- df$RestaurantsPriceRange2 >= input$prix[1] & df$RestaurantsPriceRange2 <= input$prix[2]
        df$ambiance_ <- df$ambiance == input$ambiance
        df$service_ <- df$service == input$service
        df$NoiseLevel_ <- df$NoiseLevel %in% input$bruit
        v_wgt[names(v_wgt) == "city_"] <- input$importance
        v_wgt[names(v_wgt) == "popularity"] <- input$popularity
        v_wgt[names(v_wgt) == "price"] <- input$v_prix
        v_wgt[names(v_wgt) == "ambiance_"] <- input$v_ambiance
        v_wgt[names(v_wgt) == "service_"] <- input$v_service
        v_wgt[names(v_wgt) == input$autres] <- input$v_bruit
        contri <- sweep(df[, names(v_wgt)], MARGIN = 2, v_wgt, '*')
        contri$score <- rowSums(contri, na.rm = T)
        df$score <- contri$score
        contri <- arrange(contri, desc(score))
        tab <- str_split(review$text[review$business_id == df$business_id[1]], " ") %>%
            unlist() %>%
            table() %>%
            sort(decreasing = T)
        wordcloud(words = names(tab), freq = tab, min.freq = 1,
                  max.words=50, random.order=FALSE, rot.per=0.35,
                  colors=brewer.pal(8, "Dark2"), scale=c(4,0.5))
    })
    output$distPlot2 <- renderPlot({
        v_wgt <- rep(0, 15)
        names(v_wgt) <- c(colnames(df)[c(7:15)], "city_", "popularity", "price", "ambiance_", "service_", "NoiseLevel_")
        keyword <- input$keyword
        day <- input$jour
        hours <- input$heure
        keyword <- ifelse(sum(str_detect(str_to_lower(df$categories), str_to_lower(keyword))) > 0, keyword, "")
        df <- df[str_detect(str_to_lower(df$categories), str_to_lower(keyword)) & (df[, paste(day, "_open", sep = "")] <= hours) & (df[, paste(day, "_close", sep = "")] > hours),] %>% filter(!is.na(business_id))
        df$city_ <- df$city == input$city
        df$price <- df$RestaurantsPriceRange2 >= input$prix[1] & df$RestaurantsPriceRange2 <= input$prix[2]
        df$ambiance_ <- df$ambiance == input$ambiance
        df$service_ <- df$service == input$service
        df$NoiseLevel_ <- df$NoiseLevel %in% input$bruit
        v_wgt[names(v_wgt) == "city_"] <- input$importance
        v_wgt[names(v_wgt) == "popularity"] <- input$popularity
        v_wgt[names(v_wgt) == "price"] <- input$v_prix
        v_wgt[names(v_wgt) == "ambiance_"] <- input$v_ambiance
        v_wgt[names(v_wgt) == "service_"] <- input$v_service
        v_wgt[names(v_wgt) == input$autres] <- input$v_bruit
        contri <- sweep(df[, names(v_wgt)], MARGIN = 2, v_wgt, '*')
        contri$score <- rowSums(contri, na.rm = T)
        df$score <- contri$score
        contri <- arrange(contri, desc(score))
        purete <- max(df$score) / (input$importance + input$popularity + input$v_prix + input$v_ambiance + input$v_service + input$v_bruit)
        ggplot() +
            geom_text(aes(x = 5, y = 5, label = paste(round(100*purete, 1), "%", sep = "")), size = 50, col = "blue") +
            theme_classic() +
            theme(axis.line=element_blank(),axis.text.x=element_blank(),
                  axis.text.y=element_blank(),axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),legend.position="none",
                  panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),plot.background=element_blank())+
            ylim(0, 6)
    })
}