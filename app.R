library(shiny)
library(shinythemes)
library(tidyverse)
library(readxl)
library(rvest)
library(janitor)
library(readr)
library(chilemapas)
library(wesanderson)#Zissou1, o heat de base
library(plotly)
#wes_palette("Zissou1")

#https://datos.gob.cl/dataset/7161

url <- 'https://www.sernameg.gob.cl/?page_id=27084'
html <- read_html(url) %>% 
  html_table()

tabla <- html[[1]]

fem_consumados <- tabla[1:19,1:13]
fem_consumados <- fem_consumados[-c(1),]
fem_consumados <- fem_consumados[-c(18),]
fem_consumados[is.na(fem_consumados)] <- 0

fem_consumados <- fem_consumados %>%
  mutate(across(everything(), as.character))

fem_consumados <- row_to_names(fem_consumados,1)
fem_consumados <- fem_consumados |> 
  rename_with(~ paste0("anio_", .)) |> 
  rename("Región" = "anio_Región")

i <- c(2:13) 

fem_consumados[ , i] <- apply(fem_consumados[ , i], 2,            # Specify own function within apply
                              function(x) as.numeric(as.character(x)))

fem_consumados$Región <- sub(".*? ", "", fem_consumados$Región)

fem_frustrados <- tabla[21:38,1:13]
fem_frustrados <- fem_frustrados[-c(18),]
fem_frustrados[is.na(fem_frustrados)] <- 0

fem_frustrados <- fem_frustrados %>%
  mutate(across(everything(), as.character))

fem_frustrados <- row_to_names(fem_frustrados,1)
fem_frustrados <- fem_frustrados |> 
  rename_with(~ paste0("anio_", .)) |> 
  rename("Región" = "anio_Región")

i <- c(2:13) 

fem_frustrados[ , i] <- apply(fem_frustrados[ , i], 2,            # Specify own function within apply
                              function(x) as.numeric(as.character(x)))

fem_frustrados$Región <- sub(".*? ", "", fem_frustrados$Región)

fem_consumados$Región <- gsub("á","a",
                              gsub("é","e",
                                   gsub("í","i",
                                        gsub("ó","o",
                                             gsub("ö","o",
                                                  gsub("ú","u",
                                                       gsub("ü","u",
                                                            fem_consumados$Región)))))))
fem_consumados$Región[fem_consumados$Región == "Magallanes"] <- "Magallanes y de la Antartica Chilena"
fem_consumados$Región[fem_consumados$Región == "Aysen"] <- "Aysen del General Carlos Ibanez del Campo"
fem_consumados$Región[fem_consumados$Región == "Ñuble"] <- "Nuble"
fem_consumados$Región[fem_consumados$Región == "O’Higgins"] <- "Libertador General Bernardo OHiggins"
fem_consumados$Región[fem_consumados$Región == "Araucania"] <- "La Araucania"
fem_consumados$Región[fem_consumados$Región == "Bio Bio"] <- "Biobio"
fem_consumados$Región[fem_consumados$Región == "Metropolitana"] <- "Metropolitana de Santiago"

fem_frustrados$Región <- gsub("á","a",
                              gsub("é","e",
                                   gsub("í","i",
                                        gsub("ó","o",
                                             gsub("ö","o",
                                                  gsub("ú","u",
                                                       gsub("ü","u",
                                                            fem_frustrados$Región)))))))
fem_frustrados$Región[fem_frustrados$Región == "Magallanes"] <- "Magallanes y de la Antartica Chilena"
fem_frustrados$Región[fem_frustrados$Región == "Aysen"] <- "Aysen del General Carlos Ibanez del Campo"
fem_frustrados$Región[fem_frustrados$Región == "Ñuble"] <- "Nuble"
fem_frustrados$Región[fem_frustrados$Región == "O’Higgins"] <- "Libertador General Bernardo OHiggins"
fem_frustrados$Región[fem_frustrados$Región == "Araucania"] <- "La Araucania"
fem_frustrados$Región[fem_frustrados$Región == "Bio Bio"] <- "Biobio"
fem_frustrados$Región[fem_frustrados$Región == "Metropolitana"] <- "Metropolitana de Santiago"


#agregado

fem_consumados$Tipo <- "Consumados"
fem_frustrados$Tipo <- "Frustrados"

agregado <- rbind(fem_consumados,fem_frustrados)

#fusión geometry y datos

c <- codigos_territoriales |> 
  select(codigo_region,nombre_region) |> 
  distinct() |> 
  full_join(agregado, by = join_by(nombre_region == Región))

datos_agregados <- c |> 
  left_join(generar_regiones(mapa = chilemapas::mapa_comunas)) 

plot_agregados <- ggplot(datos_agregados, aes(fill=`2013`, geometry = geometry))+
  geom_sf()+
  scale_fill_gradientn(colours=(wes_palette("Zissou1")),
                       name="Frecuencia",
                       na.value = "grey50")+ theme_classic()
ggplotly(plot_agregados)

plot_agregados_sn <- ggplot(datos_agregados |> 
                              filter(nombre_region != "Metropolitana de Santiago"), aes(fill=`2013`, geometry = geometry))+
  geom_sf()+
  scale_fill_gradientn(colours=(wes_palette("Zissou1")),
                       name="Frecuencia",
                       na.value = "grey50")+ theme_classic()
ggplotly(plot_agregados_sn)


# Define UI for application that draws a histogram
ui <- fluidPage(
  #tema shinythemes
    theme = shinytheme("simplex"),
    # Application title
    titlePanel("Femicidios en Chile 2013 - 2024"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("tipo",
                        "Tipo:",
                      unique(datos_agregados$Tipo)),
          
      checkboxInput("rm_incl", "¿Región Metropolitana inclusive?", TRUE),
      verbatimTextOutput("Sí"),
      
      selectInput("anio", "Año:",
                  c("2013" = "2013",
                    "2014" = "2014"))
    ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("coropletico")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output){
  
  output$coropletico <- renderPlot({
    if(input$rm_incl){
      ggplot(datos_agregados |> 
               filter(Tipo == input$tipo), 
             aes(fill = input$anio, geometry = geometry))+
        geom_sf()+
        scale_fill_gradientn(colours=(wes_palette("Zissou1")),
                             name="Frecuencia",
                             na.value = "grey50")+ theme_classic()
    } else {
      ggplot(datos_agregados |> 
               filter(Tipo == input$tipo) |> 
               filter(nombre_region != "Metropolitana de Santiago"), 
             aes(fill = input$anio, geometry = geometry))+
        geom_sf()+
        scale_fill_gradientn(colours=(wes_palette("Zissou1")),
                             name="Frecuencia",
                             na.value = "grey50")+ theme_classic()
      
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
