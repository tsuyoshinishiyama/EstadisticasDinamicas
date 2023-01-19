#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)
library(rgdal)
library(png)
library("formattable")

dataProv <- read.csv("data/dataProvincia.csv")
dataEstadioClinico <- read.csv("data/dataEstadioClinico.csv")
dataLocalizacion <- read.csv("data/dataLocalizacion.csv")
dataHombreMujer <- read.csv("data/dataHombreMujer.csv")
dataPediatrico <- read.csv("data/dataPediatrico.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Estadísticas Dinámicas"),
    
    tabsetPanel(type = "tabs",
                
                ######################################################
                #  provinciales
                ######################################################
                tabPanel(
                  title = "Estadísticas provinciales",
                  icon = icon("map-location-dot"),
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("in_map_year", "Año:", multiple = TRUE,
                                  list("2017", "2018", "2019", "2020", "2021"),
                                  list("2017", "2018", "2019", "2020", "2021")
                      ),
                      selectInput("in_map_sexo", "Sexo(Aplicar sólo por provincia):", multiple = TRUE,
                                  list("hombre", "mujer"),
                                  list("hombre", "mujer")
                      ),
                      checkboxGroupInput("in_map_prov",
                                         "Provincia:",
                                         choices = unique(dataEstadioClinico$TOPONIMIA),
                                         selected = unique(dataEstadioClinico$TOPONIMIA),
                                         inline = TRUE),
                      
                      downloadButton("downloadDataProvincia", "Download:Cuadro1"),
                      downloadButton("downloadDataEstadio", "Download:Cuadro2")
                    ),
                    mainPanel(
                      fluidRow(
                        column(12,
                               h4("Total de casos por provincia"),
                               leafletOutput("mymap",height = 300)
                        ),
                        fluidRow(
                          column(12,
                                 h4("Total de casos según provincia y estadio"),
                                 plotOutput("out_EstadioClinico"),
                          )
                        ),
                        fluidRow(
                          column(6,
                                 h4("Cuadro1:Total de casos por provincia"),
                                 tableOutput("out_table_Provincia")
                          ),
                          column(6,
                                 h4("Cuadro2:Total de casos según provincia y estadio"),
                                 tableOutput("out_table_Estadio")
                          ),
                        )
                      )
                    )
                  )
                ),
                ######################################################
                #  Sexo y Grupo Etario
                ######################################################
                tabPanel(
                  title = "Estadísticas según Sexo y Grupo Etario",
                  icon = icon("chart-simple"),
                  # Sidebar with a slider input for number of bins 
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("in_hm_year", "Año:", multiple = TRUE,
                                  list("2017", "2018", "2019", "2020", "2021"),
                                  list("2017", "2018", "2019", "2020", "2021")
                      ),
                      selectInput("in_hm_sexo", "Sexo:", multiple = TRUE,
                                  list("hombre", "mujer"),
                                  list("hombre", "mujer")
                      ),
                      selectInput("in_hm_etario", "Etario:", multiple = TRUE,
                                  list("80 <", "70-79", "60-69", "50-59", "40-49", "30-39", "20-29", "10-19", "0-9"),
                                  list("80 <", "70-79", "60-69", "50-59", "40-49", "30-39", "20-29", "10-19", "0-9")
                      ),
                      downloadButton("downloadDataHombreMujer", "Download")
                    ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                      #fluidRow(
                      #  column(12,div(style = "height:30px;"))
                      #),
                      fluidRow(
                        column(width=6,
                               align="center",
                        ),
                        column(width=6,
                               align="center",
                        )
                      ),
                      fluidRow(
                        column(width=12,
                               h4("Total de casos por sexo"),
                               plotOutput("distPlot")
                        )
                      ),
                      fluidRow(
                        column(width=12,
                               h4("Cuadro:Total de casos por sexo"),
                               tableOutput("disttable")
                        )
                      )
                    )
                  )
                ),
                ######################################################
                #  Topográficas
                ######################################################
                tabPanel(
                  title = "Estadísticas Topográficas",
                  icon = icon("person"),
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("in_local_year", "Año:", multiple = TRUE,
                                         list("2019", "2020", "2021"),
                                         list("2019", "2020", "2021")
                             ),
                             selectInput("in_local_sexo", "Sexo:", multiple = TRUE,
                                         list("hombre", "mujer"),
                                         list("hombre", "mujer")
                             ),
                             selectInput("in_local_localizacion", "Localización:", multiple = TRUE,
                                         choices = unique(dataLocalizacion$LOCALIZACION),
                                         selected = unique(dataLocalizacion$LOCALIZACION)
                              ),
                             downloadButton("downloadDataLocalizacion", "Download:Cuadro"),
                           ),
                           mainPanel(
                             fluidRow(
                               column(12,
                                      h4("Total de casos por sexo ")
                                      )
                             ),
                             fluidRow(
                               column(width=6,
                                      align="center",
                                      img(src = "male.png",
                                         alt = "male",
                                         width = 100,
                                         height = 100
                                      ),
                                      h2(textOutput("out_text_hombre")),
                                      h4("Hombres"),
                               ),
                               column(width=6,
                                      align="center",
                                      img(src = "female.png",
                                           alt = "female",
                                           width = 100,
                                           height = 100
                                      ),
                                      h2(textOutput("out_text_mujer")),
                                      h4("Mujeres")
                               )
                             ),
                              fluidRow(
                                column(12,
                                        align="top",
                                        h4("Top 10"),
                                        plotOutput("out_plot_localizacion")
                                 )
                               ),
                             fluidRow(
                                 column(12,
                                        h4("Cuadro"),
                                        tableOutput("out_table_localizacion")
                                 )
                               )
                           )
                         )
                ),
                ######################################################
                #  Pediatricos
                ######################################################
                tabPanel(
                  title = "Estadísticas Pediátricas",
                  icon = icon("child"),
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("in_pediatrico_year", "Año:", multiple = TRUE,
                                  list("2019", "2020", "2021"),
                                  list("2019", "2020", "2021")
                      ),
                      selectInput("in_pediatrico_sexo", "Sexo:", multiple = TRUE,
                                  list("hombre", "mujer"),
                                  list("hombre", "mujer")
                      ),
                      selectInput("in_pediatrico_diagnostico_solido", "Diagnóstico de solido:", multiple = TRUE,
                                  choices = unique(filter(dataPediatrico, TIPO == "solido") %>% select(DIAGNOSTICO)),
                                  selected = unique(dataPediatrico$DIAGNOSTICO)
                      ),
                      selectInput("in_pediatrico_diagnostico_liquido", "Diagnóstico de liquido:", multiple = TRUE,
                                  choices = unique(filter(dataPediatrico, TIPO == "liquido") %>% select(DIAGNOSTICO)),
                                  selected = unique(dataPediatrico$DIAGNOSTICO)
                      ),
                      downloadButton("downloadDataPediatricoSolido", "Download:Cuadro1"),
                      downloadButton("downloadDataPediatricoLiquido", "Download:Cuadro2")
                    ),
                    mainPanel(
                      fluidRow(
                        column(12,
                               h4("Total de casos por sexo ")
                        )
                      ),
                      fluidRow(
                        column(width=4,
                               align="center",
                               img(src = "boy.png",
                                   alt = "boy",
                                   width = 60,
                                   height = 100
                               ),
                               h2(textOutput("out_text_boy")),
                               h4("Niños"),
                        ),
                        column(width=4,
                               align="center",
                               img(src = "girl.png",
                                   alt = "girl",
                                   width = 60,
                                   height = 100
                               ),
                               h2(textOutput("out_text_girl")),
                               h4("Niñas")
                        ),
                        column(width=4,
                               align="center",
                               plotOutput("out_plot_pediatrico_boygirl", height="120px"),
                               h2(textOutput("out_text_donut"))
                        )
                      ),
                      fluidRow(
                        column(6,
                               align="center",
                               h4("Total de casos de diagnosticos solidos"),
                               plotOutput("out_plot_pediatrico_solido")
                        ),
                        column(6,
                               align="center",
                               h4("Total de casos de diagnosticos liquidos"),
                               plotOutput("out_plot_pediatrico_liquido")
                        )
                      ),
                      fluidRow(
                        column(6,
                               h4("Cuadro1:Total de casos de diagnosticos solidos"),
                               tableOutput("out_table_pediatrico_solido")
                        ),
                        column(6,
                               h4("Cuadro2:Total de casos de diagnosticos liquidos"),
                               tableOutput("out_table_pediatrico_liquido")
                        )
                      )
                    )
                  )
                )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  ######################################################
  #  Pediatricos
  ######################################################
  output$out_plot_pediatrico_solido <- renderPlot({
    
    dataPediatrico <- dataPediatrico %>%
      filter(YEAR %in% input$in_pediatrico_year) %>%
      filter(SEXO %in% input$in_pediatrico_sexo) %>%
      filter(DIAGNOSTICO %in% input$in_pediatrico_diagnostico_solido)
      
    output$out_table_pediatrico_solido <- renderTable(dataPediatrico)
    
    diagnostico <- dataPediatrico$DIAGNOSTICO
    tipo <- dataPediatrico$SEXO
    val <- dataPediatrico$CASOS
    df <- data.frame(x = diagnostico, y = val, sexo = tipo)
    
    ggplot(df, aes(x = x, y = y, fill = sexo)) +
      geom_bar(position = "stack", stat = "identity") + 
      xlab("Diagnóstico de solido") +
      ylab("Casos") +
      theme_bw(base_size = 13) +
      scale_fill_manual("", values = c("hombre" = "#5B9BD5", "mujer" = "#ED7D31")) + 
      theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
      theme(legend.position = "none")
  })
  
  output$out_plot_pediatrico_liquido <- renderPlot({
    
    dataPediatrico <- dataPediatrico %>%
      filter(YEAR %in% input$in_pediatrico_year) %>%
      filter(SEXO %in% input$in_pediatrico_sexo) %>%
      filter(DIAGNOSTICO %in% input$in_pediatrico_diagnostico_liquido)
    
    output$out_table_pediatrico_liquido <- renderTable(dataPediatrico)
    
    diagnostico <- dataPediatrico$DIAGNOSTICO
    tipo <- dataPediatrico$SEXO
    val <- dataPediatrico$CASOS
    df <- data.frame(x = diagnostico, y = val, sexo = tipo)
    
    ggplot(df, aes(x = x, y = y, fill = sexo)) +
      geom_bar(position = "stack", stat = "identity") + 
      xlab("Diagnóstico de liquido") +
      ylab("Casos") +
      theme_bw(base_size = 13) +
      scale_fill_manual("", values = c("hombre" = "#5B9BD5", "mujer" = "#ED7D31")) + 
      theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
      theme(legend.position = "none")
  })
  
  dataPediatricoTotal <- reactive({
    dataPediatricoTotal <- dataPediatrico %>%
      filter(YEAR %in% input$in_pediatrico_year) %>%
      filter(SEXO %in% input$in_pediatrico_sexo) %>%
      filter(DIAGNOSTICO %in% input$in_pediatrico_diagnostico_solido | DIAGNOSTICO %in% input$in_pediatrico_diagnostico_liquido) %>%
      group_by(SEXO) %>%
      summarise(total_val = sum(CASOS))
      #mutate(total_val = ifelse(length(total_val) == 0, 0, total_val))
  })
  
  
  output$out_plot_pediatrico_boygirl <- renderPlot({
    
    #dataPediatricoTotal <- dataPediatricoTotal()
    
    dataPediatricoTotalBoy <- dataPediatricoTotal() %>%
      filter(SEXO == "hombre")
    
    output$out_text_boy <- renderText(format(dataPediatricoTotalBoy$total_val, big.mark = ","))
    
    
    dataPediatricoTotalGirl <- dataPediatricoTotal() %>%
      filter(SEXO == "mujer")
    
    output$out_text_girl <- renderText(format(dataPediatricoTotalGirl$total_val, big.mark = ","))
    
    
    # Create data.
    data <- data.frame(
      category=c("mujer", "hombre"),
      count=c(ifelse(length(dataPediatricoTotalGirl$total_val) == 0, 0, dataPediatricoTotalGirl$total_val), 
              ifelse(length(dataPediatricoTotalBoy$total_val) == 0, 0, dataPediatricoTotalBoy$total_val))
    )
    
    #output$out_text_donut <- renderText(ifelse(length(dataPediatricoTotalBoy$total_val) == 0, 1, 2))
    
    # Compute percentages
    data$fraction <- data$count / sum(data$count)
    
    # Compute the cumulative percentages (top of each rectangle)
    data$ymax <- cumsum(data$fraction)
    
    # Compute the bottom of each rectangle
    data$ymin <- c(0, head(data$ymax, n=-1))
    
    # Compute label position
    data$labelPosition <- (data$ymax + data$ymin) / 2
    
    # Compute a good label
    #data$label <- paste0(data$category, "\n value: ", data$count)
    data$label <- paste0(percent(data$fraction))
    
    data$label <- ifelse(data$label == "0.00%", "", data$label)
    
    # Make the plot
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
      geom_rect() +
      scale_fill_manual("", values = c("hombre" = "#5B9BD5", "mujer" = "#ED7D31")) + 
      #geom_text( x=5, aes(y=labelPosition, label=label, color=c("#17202A","#17202A")), size=4) + # x here controls label position (inner / outer)
      geom_text( x=3.5, aes(y=labelPosition, label=label), size=4) + # x here controls label position (inner / outer)
      #scale_fill_brewer(palette=3) +
      #scale_color_brewer(palette=3) +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none")
  })
  
  dataPediatricoSolido <- reactive({
    dataPediatricoSolido <- dataPediatrico %>%
      filter(YEAR %in% input$in_pediatrico_year) %>%
      filter(SEXO %in% input$in_pediatrico_sexo) %>%
      filter(DIAGNOSTICO %in% input$in_pediatrico_diagnostico_solido)
  })
  
  output$downloadDataPediatricoSolido <- downloadHandler(
    filename = function() {
      "Cuadro1.csv"
    },
    content = function(file) {
      write.csv(dataPediatricoSolido(), file, row.names = FALSE)
    }
  )
  
  dataPediatricoLiquido <- reactive({
    dataPediatricoLiquido <- dataPediatrico %>%
      filter(YEAR %in% input$in_pediatrico_year) %>%
      filter(SEXO %in% input$in_pediatrico_sexo) %>%
      filter(DIAGNOSTICO %in% input$in_pediatrico_diagnostico_liquido)
  })
  
  output$downloadDataPediatricoLiquido <- downloadHandler(
    filename = function() {
      "Cuadro2.csv"
    },
    content = function(file) {
      write.csv(dataPediatricoLiquido(), file, row.names = FALSE)
    }
  )
  
  ######################################################
  #  Topográficas
  ######################################################
  dataLocalizacionNew <- reactive({
    dataLocalizacionNew <- dataLocalizacion %>%
      filter(LOCALIZACION %in% input$in_local_localizacion) %>%
      filter(YEAR %in% input$in_local_year) %>%
      filter(SEXO %in% input$in_local_sexo) %>%
      arrange(YEAR, SEXO, LOCALIZACION)
  })
  
  output$out_table_localizacion <- renderTable({
    dataLocalizacionNew()
  })
  
  
  output$downloadDataLocalizacion <- downloadHandler(
    filename = function() {
      "Cuadro.csv"
    },
    content = function(file) {
      write.csv(dataLocalizacionNew(), file, row.names = FALSE)
    }
  )
  
  output$out_plot_localizacion <- renderPlot({
    
    dataLocalizacionNew <- dataLocalizacionNew()

    dataLocalizacionTotal <- dataLocalizacionNew %>%
      group_by(SEXO) %>%
      summarise(total_val = sum(CASOS)) %>%
      mutate(total_val = ifelse(is.na(total_val), 0, total_val))
    
    totalHombre <- dataLocalizacionTotal %>%
      filter(SEXO == "hombre")
    
    totalMujer <- dataLocalizacionTotal %>%
      filter(SEXO == "mujer")
    
    output$out_text_hombre <- renderText(ifelse(is.null(totalHombre$total_val), 0, format(totalHombre$total_val, big.mark = ",")))
    output$out_text_mujer <- renderText(ifelse(is.null(totalMujer$total_val), 0, format(totalMujer$total_val, big.mark = ",")))

    
    dataLocalizacionTop10 <- dataLocalizacionNew %>%
      group_by(LOCALIZACION)  %>%
      summarise(total_val = sum(CASOS)) %>%
      top_n(10, total_val) %>%
      arrange(desc(total_val))
    
    dataLocalizacionNew <- dataLocalizacionNew %>%
      group_by(LOCALIZACION, SEXO)  %>%
      summarise(total_val = sum(CASOS)) %>%
      filter(LOCALIZACION %in% dataLocalizacionTop10$LOCALIZACION)
      #arrange(total_val)
    

    localizacion <- dataLocalizacionNew$LOCALIZACION
    localizacion <- factor(localizacion, levels = dataLocalizacionTop10$LOCALIZACION)
    tipo <- dataLocalizacionNew$SEXO
    val <- dataLocalizacionNew$total_val
    df <- data.frame(x = localizacion, y = val, sexo = tipo)
    
    
    ggplot(df, aes(x = x, y = y, fill = sexo)) +
      geom_bar(position = "stack", stat = "identity") + 
      xlab("Localización") +
      ylab("Casos") +
      theme_bw(base_size = 13) +
      scale_fill_manual("", values = c("hombre" = "#5B9BD5", "mujer" = "#ED7D31")) + 
      theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
      theme(legend.position = "none")
  })
  
  
  ######################################################
  #  provinciales
  ######################################################
  output$mymap <- renderLeaflet({
    
    map_region <- readOGR(dsn = "data/PROVCenso2010.shp")
    
    map_region <- spTransform(map_region, CRS("+proj=longlat +datum=WGS84"))
    

    dataProv <- dataProv %>%
      filter(TOPONIMIA %in% input$in_map_prov) %>%
      filter(YEAR %in% input$in_map_year) %>%
      filter(SEXO %in% input$in_map_sexo) %>%
      group_by(TOPONIMIA)  %>%
      summarise(TOTAL_CASOS = sum(CASOS)) %>%
      mutate(TOPONIMIA = toupper(TOPONIMIA)) %>%
      select(TOPONIMIA, TOTAL_CASOS)
    
    map_region@data <- map_region@data %>%
      left_join(dataProv) %>%
      mutate(TOTAL_CASOS = ifelse(is.na(TOTAL_CASOS), 0, TOTAL_CASOS))
    
    paleta_de_color <- colorNumeric("Blues", NULL, n = nrow(map_region@data))
    pal <- colorNumeric(palette="Blues", domain=map_region$TOTAL_CASOS)
    
    region_popup <- paste0(
      "<strong>Provincia: </strong>",
      map_region$TOPONIMIA,
      "<br><strong>Casos: </strong>",
      format(map_region$TOTAL_CASOS, big.mark = ",")
    )
    
    leaflet(data = map_region) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~paleta_de_color(TOTAL_CASOS),
        fillOpacity = 0.7,
        color = "#808080",
        weight = 1
        , popup = region_popup
      ) %>%
      addLegend(position='topright', pal=pal, values=~TOTAL_CASOS)
  })
  
  output$out_EstadioClinico <- renderPlot({
    
    dataEstadioClinicoNew <- dataEstadioClinico %>% 
      filter(TOPONIMIA %in% input$in_map_prov) %>%
      filter(YEAR %in% input$in_map_year)

    prov <- dataEstadioClinicoNew$TOPONIMIA
    tipo <- dataEstadioClinicoNew$TIPO
    val <- dataEstadioClinicoNew$VAL
    df <- data.frame(x = prov, y = val, EstadioClinico = tipo)
      
    ggplot(df, aes(x = x, y = y, fill = EstadioClinico)) +
      geom_bar(position = "stack", stat = "identity") + 
      xlab("Toponimia") +
      ylab("Casos") +
      scale_fill_grey()+ 
      theme_bw(base_size = 13) +
      theme(axis.text.x=element_text(angle = -45, hjust = 0))
  })
    
  dataProvincia <- reactive({
    dataProvincia <- dataProv %>%
      filter(TOPONIMIA %in% input$in_map_prov) %>%
      filter(YEAR %in% input$in_map_year) %>%
      filter(SEXO %in% input$in_map_sexo)
      #group_by(TOPONIMIA)  %>%
      #summarise(TOTAL_CASOS=sum(Val)) %>%
      #select(TOPONIMIA, TOTAL_CASOS)
    
    dataProvincia <- with(dataProvincia, dataProvincia[order(YEAR,TOPONIMIA,SEXO),])
  })
  
  
  output$out_table_Provincia <- renderTable({
    dataProvincia()
  })
  
  output$downloadDataProvincia <- downloadHandler(
    filename = function() {
      "Cuadro1.csv"
    },
    content = function(file) {
      write.csv(dataProvincia(), file, row.names = FALSE)
    }
  )
  
  dataEstadioClinicoNew <- reactive({
    dataEstadioClinicoNew <- dataEstadioClinico %>%
      filter(TOPONIMIA %in% input$in_map_prov) %>%
      filter(YEAR %in% input$in_map_year) %>%
      rename(CASOS = VAL) %>%
      arrange(YEAR, TOPONIMIA, TIPO) %>%
      rename("ESTADIO CLINICO" = TIPO)
  })
  
  output$out_table_Estadio <- renderTable({
    dataEstadioClinicoNew()
  })
  
  output$downloadDataEstadio <- downloadHandler(
    filename = function() {
      "Cuadro2.csv"
    },
    content = function(file) {
      write.csv(dataEstadioClinicoNew(), file, row.names = FALSE)
    }
  )
  
  ######################################################
  #  Sexo y Grupo Etario
  ######################################################
  
  dataHombreMujerNew <- reactive({
    dataHombreMujerNew <- dataHombreMujer %>% 
      filter(YEAR %in% input$in_hm_year) %>%
      filter(SEXO %in% input$in_hm_sexo) %>%
      filter(ETARIO %in% input$in_hm_etario)
    
    dataHombreMujerNew <- with(dataHombreMujerNew, dataHombreMujerNew[order(YEAR,SEXO,ETARIO),])
  })
  
  output$disttable <- renderTable({
    dataHombreMujerNew()
  })
  
  output$distPlot <- renderPlot({

    dataHombreMujerTotal  <- dataHombreMujerNew()
    
    dataHombreMujerTotal <- dataHombreMujerTotal %>%
      group_by(SEXO, ETARIO)  %>%
      summarise(TOTAL_CASOS=sum(CASOS))

    ## barplots for male populations goes to the left (thus negative sign)
    dataHombreMujerTotal$TOTAL_CASOS <- ifelse(dataHombreMujerTotal$SEXO == "hombre", -1*dataHombreMujerTotal$TOTAL_CASOS, dataHombreMujerTotal$TOTAL_CASOS)
    
    ## pyramid charts are two barcharts with axes flipped
    pyramidGH2 <- ggplot(dataHombreMujerTotal, aes(x = ETARIO, y = TOTAL_CASOS, fill = SEXO)) + 
      geom_bar(data = subset(dataHombreMujerTotal, SEXO == "mujer"), stat = "identity") +
      geom_bar(data = subset(dataHombreMujerTotal, SEXO == "hombre"), stat = "identity") + 
      scale_fill_manual("", values = c("hombre" = "#5B9BD5", "mujer" = "#ED7D31")) + 
      scale_y_continuous(limits = c(-1000, 1000),
                         breaks = seq(-1000, 1000, 100),
                         labels = as.character(c(seq(1000, 0, -100), seq(100, 1000, 100)))) +
      xlab("Etario") +
      ylab("Casos") +
      theme_bw(base_size = 13) +
      coord_flip()
    pyramidGH2
    
  })
  
  output$downloadDataHombreMujer <- downloadHandler(
    filename = function() {
      "Cuadro.csv"
    },
    content = function(file) {
      write.csv(dataHombreMujerNew(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
