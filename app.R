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
# fem_consumados <- fem_consumados |> 
#   rename_with(~ paste0("anio_", .)) |> 
#   rename("Región" = "anio_Región")

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
# fem_frustrados <- fem_frustrados |> 
#   rename_with(~ paste0("anio_", .)) |> 
#   rename("Región" = "anio_Región")

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
# ggplotly(plot_agregados)

plot_agregados_sn <- ggplot(datos_agregados |> 
                              filter(nombre_region != "Metropolitana de Santiago"), aes(fill=`2013`, geometry = geometry))+
  geom_sf()+
  scale_fill_gradientn(colours=(wes_palette("Zissou1")),
                       name="Frecuencia",
                       na.value = "grey50")+ theme_classic()
# ggplotly(plot_agregados_sn)

tabla_bonita <- datos_agregados |> 
  select(!c(geometry, codigo_region)) |> 
  select(nombre_region, Tipo, everything()) |> 
  rename("Región" = nombre_region) |> 
  rename("Tipo femicidio" = "Tipo")

datos_agregados <- datos_agregados |> 
  pivot_longer(!c(codigo_region,nombre_region,Tipo,geometry),
               names_to = "Año",
               values_to = "Cantidad")


ui <- fluidPage(
  #tema shinythemes
  theme = shinytheme("simplex"),

  titlePanel("Femicidios en Chile 2013 - 2024"),
  tabsetPanel( 
  tabPanel("Introducción"),
  tabPanel("Absoluto",
  fluidRow(
    column(6,
      selectInput("tipo",
                  "Tipo:",
                  unique(datos_agregados$Tipo),
                  selected = "Consumados")),
      
      column(6,
             selectInput("anio", "Año:",
                  unique(datos_agregados$Año),
                  selected = "2024"))
    ),
  fluidRow(
    column(6,
      checkboxInput("rm_incl", "Región Metropolitana inclusive", TRUE),
      verbatimTextOutput("Sí"))
    ),
      plotOutput("coropletico"),
  fluidRow(
    column(6,
           checkboxInput(inputId = "mostrar_tabla",
                         label = "Mostrar tabla de datos",
                         value = FALSE)),
      DT::DTOutput(outputId = "tabla_bonita")
  )
  ), #tabPannel comma
  tabPanel("Proporcional")
))

server <- function(input, output){
  
  output$coropletico <- renderPlot({
    if(input$rm_incl){
      ggplot(datos_agregados |> 
               filter(Año == input$anio & Tipo == input$tipo), 
             aes(fill = Cantidad, geometry = geometry))+
        geom_sf()+
        scale_fill_gradientn(colours = (wes_palette("Zissou1")),
                             breaks = c(datos_agregados %>%
                                          filter(Año == input$anio & Tipo == input$tipo) %>%
                                          summarize(max_value = max(Cantidad)) %>%
                                          pull(max_value),
                                        datos_agregados %>%
                                          filter(Año == input$anio & Tipo == input$tipo) %>%
                                          summarize(max_value = max(Cantidad)) %>%
                                          pull(max_value)/2,
                                        datos_agregados %>%
                                          filter(Año == input$anio & Tipo == input$tipo) %>%
                                          summarize(min_value = min(Cantidad)) %>%
                                          pull(min_value)
                             ),
                             labels = c(datos_agregados %>%
                                          filter(Año == input$anio & Tipo == input$tipo) %>%
                                          summarize(max_value = max(Cantidad)) %>%
                                          pull(max_value),
                                        datos_agregados %>%
                                          filter(Año == input$anio & Tipo == input$tipo) %>%
                                          summarize(max_value = max(Cantidad)) %>%
                                          pull(max_value)/2,
                                        datos_agregados %>%
                                          filter(Año == input$anio & Tipo == input$tipo) %>%
                                          summarize(min_value = min(Cantidad)) %>%
                                          pull(min_value)
                             ),
                             name="Frecuencia",
                             na.value = "grey50")+ theme_classic() + 
        theme(axis.line=element_blank(),axis.text.x=element_blank(),
              axis.text.y=element_blank(),axis.ticks=element_blank())
      
    } else {
      ggplot(datos_agregados |> 
               filter(Año == input$anio & Tipo == input$tipo) |> 
               filter(nombre_region != "Metropolitana de Santiago"), 
             aes(fill = Cantidad, geometry = geometry))+
        geom_sf()+
        scale_fill_gradientn(colours = (wes_palette("Zissou1")),
                             breaks = c(datos_agregados %>%
                                          filter(nombre_region != "Metropolitana de Santiago") |> 
                                          filter(Año == input$anio & Tipo == input$tipo) %>%
                                          summarize(max_value = max(Cantidad)) %>%
                                          pull(max_value),
                                        datos_agregados %>%
                                          filter(nombre_region != "Metropolitana de Santiago") |> 
                                          filter(Año == input$anio & Tipo == input$tipo) %>%
                                          summarize(max_value = max(Cantidad)) %>%
                                          pull(max_value)/2,
                                        datos_agregados %>%
                                          filter(nombre_region != "Metropolitana de Santiago") |> 
                                          filter(Año == input$anio & Tipo == input$tipo) %>%
                                          summarize(min_value = min(Cantidad)) %>%
                                          pull(min_value)
                             ),
                             labels = c(datos_agregados %>%
                                          filter(nombre_region != "Metropolitana de Santiago") |> 
                                          filter(Año == input$anio & Tipo == input$tipo) %>%
                                          summarize(max_value = max(Cantidad)) %>%
                                          pull(max_value),
                                        datos_agregados %>%
                                          filter(nombre_region != "Metropolitana de Santiago") |>
                                          filter(Año == input$anio & Tipo == input$tipo) %>%
                                          summarize(max_value = max(Cantidad)) %>%
                                          pull(max_value)/2,
                                        datos_agregados %>%
                                          filter(nombre_region != "Metropolitana de Santiago") |>
                                          filter(Año == input$anio & Tipo == input$tipo) %>%
                                          summarize(min_value = min(Cantidad)) %>%
                                          pull(min_value)
                             ),
                             name="Frecuencia",
                             na.value = "grey50")+ theme_classic() + 
        theme(axis.line=element_blank(),axis.text.x=element_blank(),
              axis.text.y=element_blank(),axis.ticks=element_blank())
    }
  })
  
  output$tabla_bonita <- DT::renderDT({
    if(input$mostrar_tabla){
      DT::datatable(tabla_bonita %>%
                      filter(`Tipo femicidio` == input$tipo),
                    options = list(dom = "ft",
                                   pageLength = 10000))
    }
  })
}

shinyApp(ui = ui, server = server)

