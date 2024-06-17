install.packages("ggplot2")
install.packages("tidyverse")
install.packages("countrycode")
install.packages("plotly")
install.packages("DT")

library(countrycode)
library(ggplot2)
library(dplyr)
library(shiny)
library(plotly)
library(DT)

setwd("C:/Users/Evelio/Documents/Rstudio Worrking Directory")

votes <- readRDS("C:/Users/Evelio/Documents/Rstudio Worrking Directory/votes.rds")
descriptions <- readRDS("C:/Users/Evelio/Documents/Rstudio Worrking Directory/descriptions.rds")

#Parte 1

votes_filtradosnecesarios <- votes %>%
  filter(vote %in% c(1, 2, 3))

#Parte 2

votes_filtradosnecesarios <- votes_filtradosnecesarios %>%
  mutate(year = session + 1945)

#Parte 3 

countrycode(votes_filtradosnecesarios$ccode,origin ="cown",destination = "country.name" )
votes_filtradosnecesarios$country <- countrycode(votes_filtradosnecesarios$ccode, origin = "cown", destination = "country.name", warn = FALSE)

#Parte 4

votes_filtradosnecesarios$country[votes_filtradosnecesarios$country == "United States of America"] <- "United States"
votes_filtradosnecesarios$country[votes_filtradosnecesarios$country == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"

#Parte 5

resultados <- votes %>%
  group_by(vote) %>%
  summarise(
    total = n(),
    porcentaje = (n() / nrow(votes)) * 100
  )

yes <- resultados %>%
  filter(vote == 1)
no <- resultados %>%
  filter(vote == 3)

print("Votos 'Yes':")
print(yes)
print("Votos 'No':")
print(no)

#Parte 6

agrupacion_año <- votes_filtradosnecesarios %>%
  group_by(votes_filtradosnecesarios$year,vote) %>%
  summarise(total = n(), porcentaje = (n()/nrow(votes)*100)) %>%
  filter(vote == 1)
print(agrupacion_año)

#Parte 7

agrupacion_pais <- votes_filtradosnecesarios %>%
  group_by(votes_filtradosnecesarios$country,vote) %>%
  summarise(total = n(), porcentaje = (n()/nrow(votes)*100)) %>%
  filter(vote == 1)
print(agrupacion_pais)

#Parte 8

mayor_menor <- agrupacion_pais %>%
  arrange(desc(porcentaje))
print(mayor_menor)

#Parte 9

grafico9 <- ggplot(agrupacion_año,
                   aes(x = agrupacion_año$`votes_filtradosnecesarios$year`,
                       y = agrupacion_año$porcentaje)) +
  geom_line(col = "blue") +
  ggtitle("Porcentaje total para los que votaron Sí, según año") +
  xlab("Año") +
  ylab("Porcentaje total") +
  scale_x_continuous(limits = c(1947, 2013), breaks = seq(1947, 2013, 6)) +
  scale_y_continuous(limits = c(0,4), breaks = seq(0,4,0.5))
print(grafico9)

# Aproximadamente entre los años 1977 y 1989 se dio un periodo en donde 
# hubo un mayor porcentaje de votos Sí, además, desde el año 1947 hasta 
# el año 2013 ha existido una tendencia al aumento en el porcentaje de los
# que votaron Sí.

#Parte 10

paises_seleccionados <- subset(votes_filtradosnecesarios,
                               votes_filtradosnecesarios$country %in% c("Mexico",
                                                                        "Egypt",
                                                                        "Philippines", 
                                                                        "Pakistan",
                                                                        "Venezuela",
                                                                        "Thailand"))

paises_seleccionados_ultimo <- paises_seleccionados %>%
  group_by(paises_seleccionados$vote, country, year) %>%
  summarise(total = n(), porcentaje = (n()/nrow(votes)*100))


grafico10 <- ggplot(paises_seleccionados_ultimo, aes(
  x = paises_seleccionados_ultimo$year, y = paises_seleccionados_ultimo$total,
  color = paises_seleccionados_ultimo$country)) +
  geom_line() +
  ggtitle("Votos Sí según año de los 6 países con mas votos Sí (totales)") +
  xlab("Año") +
  ylab("Cantidad total") +
  theme(legend.position = "right") +
  scale_color_discrete(name = "País") +
  scale_x_continuous(limits = c(1947, 2013), breaks = seq(1947, 2013, 6))

print(grafico10)

# parte 11

grafico11 <- ggplot(paises_seleccionados_ultimo, aes(
  x = paises_seleccionados_ultimo$year, y = paises_seleccionados_ultimo$total,
  color = paises_seleccionados_ultimo$country)) +
  geom_line() +
  facet_wrap(~paises_seleccionados_ultimo$country) +
  ggtitle("Votos Sí según año de los 6 países con mas votos Sí (totales)") +
  xlab("Año") +
  ylab("Cantidad total") +
  theme(legend.position = "right") +
  scale_color_discrete(name = "País") +
  scale_x_continuous(limits = c(1947, 2013), breaks = seq(1947, 2013, 20))

print(grafico11)

# se puede observar que tanto conjunta como individualmente los 6 países
# a lo largo de los años han presentado una tendencia al aumento en la cantidad
# de votos Sí, e igualmente se observa un periodo a inicios y finales de los 
# años 80 en el cual la cantidad de votos Sí aumento considerablemente.

######APLICACION#####

ui <- fluidPage(
  titlePanel("Votos hechos por los países miembros de las naciones unidas durante los años 1947 al 2013"),
  
  tabsetPanel(
    tabPanel("Tabla Votos Filtrados", 
             DTOutput("tabla_votos")),
    tabPanel("Tabla de Resultados", 
             dataTableOutput("tabla_resultados")),
    tabPanel("Tabla Agrupación por Año",
             dataTableOutput("tabla_resumen")),
    tabPanel("Tabla Agrupacion por Pais",
             dataTableOutput("tabla_resultados_pais")),
    tabPanel("Gráfico 1",
             fluidPage(
               
               # Application title
               titlePanel("Porcentaje de votos Sí según año por los países de las Naciones Unidas"),
               
               # Sidebar with a slider input for number of bins 
               sidebarLayout(
                 sidebarPanel(
                   # slider para seleccionar el rango de años
                   
                   sliderInput("yearRange",
                               "Seleccione el rango de años:",
                               min = 1947,
                               max = 2013,
                               value = c(1977,1989)),
                   
                   # boton para sumar el porcentaje del rango
                   actionButton("sumButton", "Calcular porcentaje total")
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                   plotlyOutput("lineplot"),
                   textOutput("sumText"))
               )
             )
    ),
    tabPanel("Gráfico 2",
             fluidPage(
               
               # Application title
               titlePanel("Cantidad total de votos Sí de los 6 países con más votos Sí entre los años 1947-2013"),
               
               
               checkboxGroupInput("selected_countries", "Seleccione los países:", 
                                  choices = c("Egypt", "Mexico", "Pakistan", "Philippines", "Thailand", "Venezuela"),
                                  selected = "Egypt",
                                  inline = T),
               
               ),
             
             mainPanel(
               plotOutput("linePlot2"),

               )
             
    )
  )
)

                   
             
server <- function(input, output) {
  
  resultados <- reactive({
    votes %>%
      group_by(vote) %>%
      summarise(
        total = n(),
        porcentaje = (n() / nrow(votes)) * 100
      )
  })
  
  output$tabla_resultados <- renderDataTable({
    datatable(
      resultados(),
      options = list(
        searching = FALSE, 
        paging = FALSE,    
        info = FALSE       
      )
    )
  })
  
  
  output$tabla_votos <- renderDT({
    datatable(votes_filtradosnecesarios, filter = 'none', options = list(
      columnDefs = list(
        list(targets = c(1, 2), searchable = FALSE) 
      ),
      initComplete = JS(
        "function(settings, json) {",
        "$('.dataTables_filter input').attr('placeholder', 'Buscar por country');",  
        "}"
      )
    ))
  })
  
  output$tabla_resumen <- renderDataTable({
    datatable(agrupacion_año, 
              options = list(
                paging = FALSE, 
                searching = FALSE 
              ))
  })
  
  output$tabla_resultados_pais <- renderDataTable({
    datatable(agrupacion_pais, 
              options = list(
                paging = TRUE,
                searching = TRUE,
                pageLength = 10
              ))
  })
  
  #grafico 1
  #crear datos reactivos
  
  datos_reactivos <- reactive({paste(sum_values,input$yearRange)})
  
  output$lineplot <- renderPlotly({
    
    # dibuja el histograma
    
    p <- ggplot(agrupacion_año,
                aes(x = agrupacion_año$`votes_filtradosnecesarios$year`,
                    y = agrupacion_año$porcentaje)) +
      geom_line(col = "blue") +
      geom_point(col = "red") +
      ggtitle(NULL) +
      xlab("Año") +
      ylab("Porcentaje total") +
      scale_x_continuous(limits = c(1947, 2013), breaks = seq(1947, 2013, 3)) +
      scale_y_continuous(limits = c(0,4), breaks = seq(0,4,0.5))
    ggplotly(p)
  })
  
  #calcular la suma del promedio del rango seleccionado
  
  observeEvent(input$sumButton, {
    
    selected_data <- subset(agrupacion_año, agrupacion_año$`votes_filtradosnecesarios$year` >= input$yearRange[1] & agrupacion_año$`votes_filtradosnecesarios$year` <= input$yearRange[2])
    sum_values <- sum(selected_data$porcentaje)
    
    output$sumText <- renderText({
      paste("El porcentaje total para el rango de años seleccionado es:", round(sum_values, 2))
    })
  })
  
  #grafico 2
  
    paises_seleccionados <- subset(votes_filtradosnecesarios,
                                   votes_filtradosnecesarios$country %in% c("Mexico",
                                                                            "Egypt",
                                                                            "Philippines", 
                                                                            "Pakistan",
                                                                            "Venezuela",
                                                                            "Thailand"))
    
    paises_seleccionados_ultimo <- paises_seleccionados %>%
      group_by(paises_seleccionados$vote, country, year) %>%
      summarise(total = n(), porcentaje = (n()/nrow(votes)*100))
    
    output$linePlot2 <- renderPlot({
      req(input$selected_countries)
      filtered_data <- subset(paises_seleccionados_ultimo, country %in% input$selected_countries)
    
    ggplot(filtered_data, aes(
      x = year, y = total,
      color = country, group = country)) +
      geom_line() +
      theme_linedraw() +
      ggtitle(NULL) +
      xlab("Año") +
      ylab("Cantidad total") +
      theme(legend.position = "right") +
      scale_color_discrete(name = "País") +
      scale_x_continuous(limits = c(1947, 2013), breaks = seq(1947, 2013, 6))}
  
  )
}
  
shinyApp(ui = ui, server = server)
