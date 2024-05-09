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

url_pob <- 'https://es.wikipedia.org/wiki/Anexo:Regiones_de_Chile_por_poblaci%C3%B3n'
html_pob <- read_html(url_pob) %>% 
  html_table()

tabla_pob <- html_pob[[1]]
tabla_pob <- row_to_names(tabla_pob,1)
tabla_pob <- tabla_pob |> 
  select(Región, Población)
tabla_pob <- tabla_pob[-17,]
tabla_pob <- tabla_pob %>%
  mutate(across(everything(), as.character))

tabla_pob$Población <- str_squish(tabla_pob$Población)
tabla_pob$Población <- str_remove(tabla_pob$Población, " ")
tabla_pob$Población <- str_remove(tabla_pob$Población, " ")
tabla_pob$Población <- as.numeric(tabla_pob$Población)

tabla_pob$Región <- gsub("á","a",
                              gsub("é","e",
                                   gsub("í","i",
                                        gsub("ó","o",
                                             gsub("ö","o",
                                                  gsub("ú","u",
                                                       gsub("ü","u",
                                                            tabla_pob$Región)))))))
tabla_pob$Región[tabla_pob$Región == "Magallanes y la Antartica Chilena"] <- "Magallanes y de la Antartica Chilena"
tabla_pob$Región[tabla_pob$Región == "Aysen"] <- "Aysen del General Carlos Ibanez del Campo"
tabla_pob$Región[tabla_pob$Región == "Ñuble"] <- "Nuble"
tabla_pob$Región[tabla_pob$Región == "O'Higgins"] <- "Libertador General Bernardo OHiggins"
tabla_pob$Región[tabla_pob$Región == "Araucania"] <- "La Araucania"
tabla_pob$Región[tabla_pob$Región == "Metropolitana"] <- "Metropolitana de Santiago"

tabla_pob_datos <- full_join(tabla_pob, datos_agregados, by = join_by("Región" == "nombre_region")) 
i <- c(4:15) 
tabla_pob_datos[ , i] <- apply(tabla_pob_datos[ , i], 2,            # Specify own function within apply
                              function(x) as.numeric(as.character(x)))
tabla_pob_datos_percapita <- tabla_pob_datos %>%
  mutate(
    across(
      all_of(i),
      ~ format(.x / Población, digits = 2, scientific = FALSE),
      .names = "percapita_{.col}"  
    )
  ) |> 
  select(c(Región, Población, Tipo, geometry,18:29))

datos_percapita <- tabla_pob_datos_percapita

tabla_pob_datos_percapita$Población <- format(round(as.numeric(tabla_pob_datos_percapita$Población), 1), big.mark=".")

tabla_pob_datos_percapita <- tabla_pob_datos_percapita |> 
  select(-geometry)

clean_column_names <- function(df, string_to_remove) {
  # Use gsub to replace the unwanted string with an empty string
  new_colnames <- gsub(string_to_remove, "", colnames(df))
  # Assign new column names back to the dataframe
  colnames(df) <- new_colnames
  return(df)
}

# Apply the function to the dataframe
tabla_pob_datos_percapita <- clean_column_names(tabla_pob_datos_percapita, "percapita_")

tabla_pob_datos_percapita[4:15] <- lapply(tabla_pob_datos_percapita[4:15], function(col) {
  str_replace_all(col, "\\.", "\\,")
}) #tabla bonita percapita

i <- c(5:16)
datos_percapita[ , i] <- apply(datos_percapita[ , i], 2,            # Specify own function within apply
                               function(x) as.numeric(as.character(x)))
tabla_bonita <- datos_agregados |> 
  select(!c(geometry, codigo_region)) |> 
  select(nombre_region, Tipo, everything()) |> 
  rename("Región" = nombre_region) |> 
  rename("Tipo femicidio" = "Tipo")

datos_agregados <- datos_agregados |> 
  pivot_longer(!c(codigo_region,nombre_region,Tipo,geometry),
               names_to = "Año",
               values_to = "Cantidad")

datos_percapita <- datos_percapita |> 
  pivot_longer(!c(Región,Tipo,geometry,Población),
               names_to = "Año",
               values_to = "Cantidad")

datos_percapita$Cantidad_etiqueta <- datos_percapita$Cantidad
datos_percapita$Cantidad_etiqueta <- format(datos_percapita$Cantidad_etiqueta, scientific = FALSE, digits = 10)
datos_percapita$Año <- str_remove(datos_percapita$Año, "percapita_")

ui <- fluidPage(
  #tema shinythemes
  theme = shinytheme("simplex"),

  titlePanel("Femicidios en Chile 2013 - 2024"),
  p("La siguiente información es rescatada desde",
    a("https://www.sernameg.gob.cl/?page_id=27084",
      href = "https://www.sernameg.gob.cl/?page_id=27084",
      target="_blank", rel="noopener noreferrer" )
  ,", página web oficial del Ministerio de la Mujer y la 
  Equidad de Género del Gobierno de Chile."),
  p("Los datos fueron rescatados desde la misma página web,
    mientras que la construcción de la aplicación fue hecha
    con Shiny. El código está ",
    a("aquí.",
      href = "https://github.com/viflorenz/tablero_femicidios",
      target="_blank", rel="noopener noreferrer")),
  tabsetPanel( 
  tabPanel("Introducción",
           p(""),
           p("Según la legislación chilena (",
             a("Ley 21.212",
               href = "https://www.bcn.cl/leychile/navegar?idNorma=1143040&tipoVersion=0",
               target="_blank", rel="noopener noreferrer"),
           "), el femicidio 
           es el asesinato de una mujer ejecutado por quien es o ha 
           sido su cónyuge o conviviente, o con quien tiene o ha 
           tenido un hijo en común, en razón de tener o haber tenido 
           con ella una relación de pareja de carácter sentimental 
           o sexual sin convivencia."),
           p("También se considera femicidio el asesinato de una 
           mujer en razón de su género cuando la muerte se produzca 
           en alguna de las siguientes circunstancias:"),
           p("1.-Ser consecuencia de la negativa a establecer con el 
           autor una relación de carácter sentimental o sexual."),
           p("2.-Ser consecuencia de que la víctima ejerza o haya
           ejercido la prostitución, u otra ocupación u oficio de
           carácter sexual."),
           p("3.-Haberse cometido el delito tras haber ejercido 
           contra la víctima cualquier forma de violencia sexual, 
           sin perjuicio de lo dispuesto en el artículo 372bis."),
           p("4.-Haberse realizado con motivo de la orientación 
           sexual, identidad de género o expresión de género de 
           la víctima."),
           p("5.-Haberse cometido en cualquier tipo de situación 
             en la que se den circunstancias de manifiesta 
             subordinación por las relaciones desiguales de 
             poder entre el agresor y la víctima, o motivada por 
             una evidente intención de discriminación.")
           ),
  tabPanel("Cantidades",
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
  tabPanel("Cantidades per cápita",
           fluidRow(
             column(6,
                    selectInput("tipo_pc",
                                "Tipo:",
                                unique(datos_percapita$Tipo),
                                selected = "Consumados")),
             
             column(6,
                    selectInput("anio_pc", "Año:",
                                unique(datos_percapita$Año),
                                selected = "2024"))
             ),
           plotOutput("coropletico_pc"),
           fluidRow(
             column(6,
                    checkboxInput(inputId = "mostrar_tabla_pc",
                                  label = "Mostrar tabla de datos",
                                  value = FALSE)),
             DT::DTOutput(outputId = "tabla_bonita_pc")
           ))
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
                             name="Frecuencia\n",
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
 output$coropletico_pc <- renderPlot({

      ggplot(datos_percapita |> 
               filter(Año == input$anio_pc & Tipo == input$tipo_pc), 
             aes(fill = Cantidad, geometry = geometry))+
        geom_sf()+
        scale_fill_gradientn(colours = (wes_palette("Zissou1")),
                             breaks = c(datos_percapita %>%
                                          filter(Año == input$anio_pc & Tipo == input$tipo_pc) %>%
                                          summarize(max_value = max(Cantidad)) %>%
                                          pull(max_value),
                                        datos_percapita %>%
                                          filter(Año == input$anio_pc & Tipo == input$tipo_pc) %>%
                                          summarize(min_value = min(Cantidad)) %>%
                                          pull(min_value)
                             ),
                             labels = c(datos_percapita %>%
                                          filter(Año == input$anio_pc & Tipo == input$tipo_pc) %>%
                                          filter(Cantidad == max(Cantidad))%>%
                                          summarize(Cantidad_etiqueta = first(Cantidad_etiqueta)) %>%
                                          pull(Cantidad_etiqueta),
                                        datos_percapita %>%
                                          filter(Año == input$anio_pc & Tipo == input$tipo_pc) %>%
                                          filter(Cantidad == min(Cantidad))%>%
                                          summarize(Cantidad_etiqueta = first(Cantidad_etiqueta)) %>%
                                          pull(Cantidad_etiqueta)
                             ),
                             name="Frecuencia\nper cápita\n",
                             na.value = "grey50")+ theme_classic() + 
        theme(axis.line=element_blank(),axis.text.x=element_blank(),
              axis.text.y=element_blank(),axis.ticks=element_blank())
      
  })
  
  output$tabla_bonita_pc <- DT::renderDT({
    if(input$mostrar_tabla_pc){
      DT::datatable(tabla_pob_datos_percapita %>%
                      filter(Tipo == input$tipo_pc),
                    options = list(dom = "ft",
                                   pageLength = 10000))
    }
  })
}

shinyApp(ui = ui, server = server)

