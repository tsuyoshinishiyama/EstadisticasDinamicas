#
#
#
#
#
#
#
#

library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(png)
library("formattable")

dataProv <- read.csv("data/dataProvincia.csv")
dataEstadioClinico <- read.csv("data/dataEstadioClinico.csv")
dataLocalizacion <- read.csv("data/dataLocalizacion.csv")
dataHombreMujer <- read.csv("data/dataHombreMujer.csv")
dataPediatrico <- read.csv("data/dataPediatrico.csv")

ui <- fluidPage(

  # Application title
  titlePanel("Estadísticas Dinámicas"),
    
  tabsetPanel(type = "tabs",
                
    ######################################################
    #  provinciales
    ######################################################
    tabPanel(
      title = "Estadísticas Provinciales",
      icon = icon("map-location-dot"),
      sidebarLayout(
        sidebarPanel(
          #selectInput("in_map_year", "Año:", multiple = TRUE,
          #            list("2017", "2018", "2019", "2020", "2021"),
          #            list("2017", "2018", "2019", "2020", "2021")
          #),
          checkboxGroupInput("in_map_year", "Año:", inline = TRUE,
                             choices = list("2017", "2018", "2019", "2020", "2021"),
                             selected = list("2017", "2018", "2019", "2020", "2021"),
          ),
          selectInput("in_map_sexo", "Sexo(Aplicar sólo por provincia):", multiple = TRUE,
                      list("masculino", "femenino"),
                      list("masculino", "femenino")
          ),
          checkboxGroupInput("in_map_prov", "Provincia:", inline = TRUE,
                             choices = unique(dataEstadioClinico$TOPONIMIA),
                             selected = unique(dataEstadioClinico$TOPONIMIA),
          ),
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
              column(12,
                     h4("Total de casos según provincia y estadio con Desconocido"),
                     plotOutput("out_EstadioClinico_sum"),
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
      sidebarLayout(
        sidebarPanel(
          #selectInput("in_hm_year", "Año:", multiple = TRUE,
          #            list("2017", "2018", "2019", "2020", "2021"),
          #            list("2017", "2018", "2019", "2020", "2021")
          #),
          checkboxGroupInput("in_hm_year", "Año:", inline = TRUE,
                             choices = list("2017", "2018", "2019", "2020", "2021"),
                             selected = list("2017", "2018", "2019", "2020", "2021")
          ),
          selectInput("in_hm_sexo", "Sexo:", multiple = TRUE,
                      list("masculino", "femenino"),
                      list("masculino", "femenino")
          ),
          checkboxGroupInput("in_hm_etario", "Grupo Etario:", inline = FALSE,
                             choices = unique(dataHombreMujer$ETARIO),
                             selected = unique(dataHombreMujer$ETARIO)
          ),
          downloadButton("downloadDataHombreMujer", "Download")
        ),
                    
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
                   textOutput("textTmp"),
                   textOutput("textTmp2"),
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
          #selectInput("in_local_year", "Año:", multiple = TRUE,
          #            list("2019", "2020", "2021"),
          #            list("2019", "2020", "2021")
          #),
          checkboxGroupInput("in_local_year", "Año:", inline = TRUE,
                             choices = list("2019", "2020", "2021"),
                             selected = list("2019", "2020", "2021")
          ),
          selectInput("in_local_sexo", "Sexo:", multiple = TRUE,
                      list("masculino", "femenino"),
                      list("masculino", "femenino")
          ),
          uiOutput("ui_localizacion"),
          downloadButton("downloadDataLocalizacion", "Download:Cuadro")
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
                   img(src = "male.png",
                   alt = "male",
                   width = 100,
                   height = 100
                   ),
                   h2(textOutput("out_text_hombre")),
                   h4("Masculino")
            ),
            column(width=4,
                   align="center",
                   plotOutput("out_plot_donut_localizacion", height="120px"),
                   h2(textOutput("out_text_donut_localizacion")),
                   #h2(tableOutput("out_table_donut"))
            ),
            column(width=4,
                   align="center",
                   img(src = "female.png",
                   alt = "female",
                   width = 100,
                   height = 100
                   ),
                   h2(textOutput("out_text_mujer")),
                   h4("Femenino")
            )
          ),
          fluidRow(
            column(12,
                   align="top",
                   h4("Los 10 Más Frecuentes"),
                   plotOutput("out_plot_localizacion")
            )
          ),
          fluidRow(
            column(12,
                   h4("Cuadro:Total de casos según localización de tumor"),
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
          #selectInput("in_pediatrico_year", "Año:", multiple = TRUE,
          #            list("2019", "2020", "2021"),
          #            list("2019", "2020", "2021")
          #),
          checkboxGroupInput("in_pediatrico_year", "Año:", inline = TRUE,
                             choices = list("2019", "2020", "2021"),
                             selected = list("2019", "2020", "2021")
          ),
          selectInput("in_pediatrico_sexo", "Sexo:", multiple = TRUE,
                      list("masculino", "femenino"),
                      list("masculino", "femenino")
          ),
          uiOutput("ui_pediatrico_diagnostico_solido"),
          uiOutput("ui_pediatrico_diagnostico_liquido"),
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
                   plotOutput("out_plot_pediatrico_boygirl", height="120px"),
                   h2(textOutput("out_text_donut")),
                   h2(tableOutput("out_table_donut"))
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
            )
          ),
          fluidRow(
            column(6,
                   align="center",
                   h4("Total de casos de diagnósticos tumores sólidos"),
                   plotOutput("out_plot_pediatrico_solido")
            ),
            column(6,
                   align="center",
                   h4("Total de casos de diagnósticos tumores líquidos"),
                   plotOutput("out_plot_pediatrico_liquido")
            )
          ),
          fluidRow(
            column(6,
                   h4("Cuadro1:Total de casos de diagnósticos tumores sólidos"),
                   tableOutput("out_table_pediatrico_solido")
            ),
            column(6,
                   h4("Cuadro2:Total de casos de diagnósticos tumores líquidos"),
                   tableOutput("out_table_pediatrico_liquido")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  output$ui_localizacion <- renderUI({
    dataLocalizacionChoices <- dataLocalizacion %>%
      distinct(LOCALIZACION) %>%
      arrange(LOCALIZACION)
    
    as.list(dataLocalizacionChoices$LOCALIZACION)
    
    checkboxGroupInput("in_local_localizacion",
                       label = "Localización:",
                       inline = TRUE,
                       choices = dataLocalizacionChoices$LOCALIZACION,
                       selected = dataLocalizacionChoices$LOCALIZACION)
  })
  
  output$ui_pediatrico_diagnostico_solido <- renderUI({
    dataPediatricoChoices <- dataPediatrico %>%
      filter(TIPO == "solido") %>%
      distinct(DIAGNOSTICO) %>%
      arrange(DIAGNOSTICO)
    
    as.list(dataPediatricoChoices$DIAGNOSTICO)
    
    checkboxGroupInput("in_pediatrico_diagnostico_solido",
                       label = "Diagnósticos tumores sólidos:",
                       inline = TRUE,
                       choices = dataPediatricoChoices$DIAGNOSTICO,
                       selected = dataPediatricoChoices$DIAGNOSTICO)
  })
  
  output$ui_pediatrico_diagnostico_liquido <- renderUI({
    dataPediatricoChoices <- dataPediatrico %>%
      filter(TIPO == "liquido") %>%
      distinct(DIAGNOSTICO) %>%
      arrange(DIAGNOSTICO)
    
    as.list(dataPediatricoChoices$DIAGNOSTICO)
    
    checkboxGroupInput("in_pediatrico_diagnostico_liquido",
                       label = "Diagnósticos tumores líquidos:",
                       inline = TRUE,
                       choices = dataPediatricoChoices$DIAGNOSTICO,
                       selected = dataPediatricoChoices$DIAGNOSTICO)
  })
  
  
  ######################################################
  #  provinciales
  ######################################################
  output$mymap <- renderLeaflet({
    
    map_region_org <-  st_read("data/PROVCenso2010.shp")
    
    map_region <- st_transform(map_region_org, crs = "+proj=longlat +datum=WGS84")
    
    dataProv <- dataProv %>%
      filter(TOPONIMIA %in% input$in_map_prov) %>%
      filter(YEAR %in% input$in_map_year) %>%
      filter(SEXO %in% input$in_map_sexo) %>%
      group_by(TOPONIMIA)  %>%
      summarise(TOTAL_CASOS = sum(CASOS)) %>%
      mutate(TOPONIMIA = toupper(TOPONIMIA)) %>%
      select(TOPONIMIA, TOTAL_CASOS)
    
    map_region <- map_region %>%
      left_join(dataProv) %>%
      mutate(TOTAL_CASOS = ifelse(is.na(TOTAL_CASOS), 0, TOTAL_CASOS))
    
    #paleta_de_color <- colorNumeric("Blues", NULL, n = nrow(map_region))
    paleta_de_color <- colorBin("Blues", NULL, n = nrow(map_region))
    #pal <- colorNumeric(palette="Blues", domain=map_region$TOTAL_CASOS)
    pal <- colorBin(palette="Blues", domain=map_region$TOTAL_CASOS)
    
    region_popup <- paste0(
      "<strong>Provincia: </strong>",
      map_region$TOPONIMIA,
      "<br><strong>Casos: </strong>",
      format(map_region$TOTAL_CASOS, big.mark = ",")
    )
    
    
    leaflet(data = map_region) %>%
      #addTiles() %>%
      addPolygons(
        fillColor = ~paleta_de_color(TOTAL_CASOS),
        fillOpacity = 0.8,
        color = "#808080",
        weight = 1
        , popup = region_popup
      ) %>%
      addLegend(position='topright',
                pal=pal,
                values=~TOTAL_CASOS,
                labels=c("")
      )
  })
  
  output$out_EstadioClinico <- renderPlot({
    
    dataEstadioClinicoNew <- dataEstadioClinico %>% 
      filter(TOPONIMIA %in% input$in_map_prov) %>%
      filter(YEAR %in% input$in_map_year) %>%
      filter(TIPO != "9")
      #mutate(TIPO = ifelse(TIPO == "9", "Desc.", TIPO))
    
    prov <- dataEstadioClinicoNew$TOPONIMIA
    tipo <- dataEstadioClinicoNew$TIPO
    val <- dataEstadioClinicoNew$VAL
    df <- data.frame(x = prov, y = val, EstadioClinico = tipo)
    
    ggplot(df, aes(x = x, y = y, fill = EstadioClinico)) +
      geom_bar(position = "stack", stat = "identity") + 
      xlab("Toponimia") +
      ylab("Casos") +
      #scale_fill_grey() + 
      scale_fill_brewer(palette = "PuBu") +
      theme_bw(base_size = 13) +
      theme(axis.text.x=element_text(angle = -45, hjust = 0),
            panel.background = element_blank(),
            panel.grid = element_blank())
  })
  
  
  output$out_EstadioClinico_sum <- renderPlot({
    
    dataEstadioClinicoTable <- dataEstadioClinico %>%
      filter(TOPONIMIA %in% input$in_map_prov) %>%
      filter(YEAR %in% input$in_map_year) %>%
      rename(CASOS = VAL) %>%
      arrange(YEAR, TOPONIMIA, TIPO) %>%
      rename("ESTADIO CLINICO" = TIPO)
    
    dataEstadioClinicoWider <- dataEstadioClinicoTable %>%
      pivot_wider(names_from = "ESTADIO CLINICO",
                  values_from = "CASOS") %>%
      rename("DESCONOCIDO" = "9") %>%
      mutate("CONOCIDO" = I + II + III + IV) %>%
      select(YEAR, TOPONIMIA, DESCONOCIDO, CONOCIDO)
    
    dataEstadioClinicoLong <- dataEstadioClinicoWider %>%
      pivot_longer(cols = c("DESCONOCIDO", "CONOCIDO"),
                   names_to = "TIPO",
                   values_to = "VAL"
      )
      
      
    prov <- dataEstadioClinicoLong$TOPONIMIA
    tipo <- dataEstadioClinicoLong$TIPO
    val <- dataEstadioClinicoLong$VAL
    df <- data.frame(x = prov, y = val, EstadioClinico = tipo)
    
    ggplot(df, aes(x = x, y = y, fill = EstadioClinico)) +
      geom_bar(position = "stack", stat = "identity") + 
      xlab("Toponimia") +
      ylab("Casos") +
      #scale_fill_grey() + 
      scale_fill_brewer(palette = "PuBu") +
      theme_bw(base_size = 13) +
      theme(axis.text.x=element_text(angle = -45, hjust = 0),
            panel.background = element_blank(),
            panel.grid = element_blank())
  })
  
  dataProvinciaTable <- reactive({
    
    dataProvinciaTable <- dataProv %>%
      filter(TOPONIMIA %in% input$in_map_prov) %>%
      filter(YEAR %in% input$in_map_year) %>%
      filter(SEXO %in% input$in_map_sexo)

    dataProvinciaSexo <- dataProvinciaTable %>%
      distinct(SEXO)
    
    flgMasculino <- "masculino" %in% dataProvinciaSexo$SEXO
    flgFemenino <- "femenino" %in% dataProvinciaSexo$SEXO
    
    dataProvinciaWider <- dataProvinciaTable %>%
      add_column(flgMas = ifelse(flgMasculino, "TRUE", "FALSE")) %>%
      add_column(flgFem = ifelse(flgFemenino, "TRUE", "FALSE")) %>%
      pivot_wider(names_from = "SEXO",
                  values_from = "CASOS")
    
    dataProvinciaWider <- dataProvinciaWider %>%
      add_column(MASCULINO = as.numeric(ifelse(dataProvinciaWider$flgMas, dataProvinciaWider$masculino, 0))) %>%
      add_column(FEMENINO = as.numeric(ifelse(dataProvinciaWider$flgFem, dataProvinciaWider$femenino, 0)))
    
    dataProvinciaWider <- dataProvinciaWider %>%
      mutate(TOTAL_CASOS = MASCULINO + FEMENINO) %>%
      mutate(MASCULINO = ifelse(dataProvinciaWider$flgMas, format(MASCULINO, nsmall = 0), "NA")) %>%
      mutate(FEMENINO = ifelse(dataProvinciaWider$flgFem, format(FEMENINO, nsmall = 0), "NA")) %>%
      mutate(TOTAL_CASOS = format(TOTAL_CASOS, nsmall = 0)) %>%
      rename("Masc" = MASCULINO) %>%
      rename("Fem" = FEMENINO) %>%
      rename("TOTAL CASOS" = TOTAL_CASOS) %>%
      select(YEAR, REGION, TOPONIMIA, Masc, Fem, "TOTAL CASOS")
    
    dataProvinciaWider <- with(dataProvinciaWider, dataProvinciaWider[order(YEAR,REGION, TOPONIMIA),])
  })
  
  output$out_table_Provincia <- renderTable({
    
    dataProvinciaTable()
    
  })
  
  output$downloadDataProvincia <- downloadHandler(
    filename = function() {
      "Cuadro1.csv"
    },
    content = function(file) {
      write.csv(dataProvinciaTable(), file, row.names = FALSE)
    }
  )
  
  dataEstadioClinicoTable <- reactive({
    
    dataEstadioClinicoTable <- dataEstadioClinico %>%
      filter(TOPONIMIA %in% input$in_map_prov) %>%
      filter(YEAR %in% input$in_map_year) %>%
      rename(CASOS = VAL) %>%
      arrange(YEAR, TOPONIMIA, TIPO) %>%
      rename("ESTADIO CLINICO" = TIPO)
    
    dataEstadioClinicoWider <- dataEstadioClinicoTable %>%
      pivot_wider(names_from = "ESTADIO CLINICO",
                  values_from = "CASOS") %>%
      rename("DESCONOCIDO" = "9") %>%
      mutate("TOTAL CASOS" = DESCONOCIDO + I + II + III + IV) %>%
      select(YEAR, TOPONIMIA, I, II, III, IV, DESCONOCIDO, "TOTAL CASOS")
    
    dataEstadioClinicoWider <- with(dataEstadioClinicoWider, dataEstadioClinicoWider[order(YEAR,TOPONIMIA),])
  })
  
  output$out_table_Estadio <- renderTable({
    dataEstadioClinicoTable()
  })
  
  output$downloadDataEstadio <- downloadHandler(
    filename = function() {
      "Cuadro2.csv"
    },
    content = function(file) {
      write.csv(dataEstadioClinicoTable(), file, row.names = FALSE)
    }
  )
  
  ######################################################
  #  Sexo y Grupo Etario
  ######################################################
  output$distPlot <- renderPlot({
    
    dataHombreMujerTotal <- dataHombreMujer %>% 
      filter(YEAR %in% input$in_hm_year) %>%
      filter(SEXO %in% input$in_hm_sexo) %>%
      filter(ETARIO %in% input$in_hm_etario)
      
    
    dataHombreMujerTotal <- dataHombreMujerTotal %>%
      group_by(SEXO, ETARIO)  %>%
      summarise(TOTAL_CASOS=sum(CASOS))
      
      
    maxCasos <- dataHombreMujerTotal %>%
      filter(SEXO=="femenino") %>%
      filter(TOTAL_CASOS==max(TOTAL_CASOS)) %>%
      arrange(TOTAL_CASOS)
    
    byNum <- 100
    
    if (nrow(maxCasos) == 0) maxCasos[1,] <- NA
    if (is.na(maxCasos$TOTAL_CASOS)) maxCasos$TOTAL_CASOS <- 0
    if (maxCasos$TOTAL_CASOS == 0) byNum <- 100
    
    
    maxCasos$TOTAL_CASOS <- round(maxCasos$TOTAL_CASOS + 60, -2)
    
    #output$textTmp <- renderText(maxCasos$TOTAL_CASOS)
    
    #output$textTmp2 <- renderText(byNum)

    ## barplots for male populations goes to the left (thus negative sign)
    dataHombreMujerTotal$TOTAL_CASOS <- ifelse(dataHombreMujerTotal$SEXO == "masculino", -1*dataHombreMujerTotal$TOTAL_CASOS, dataHombreMujerTotal$TOTAL_CASOS)
    

    ## pyramid charts are two barcharts with axes flipped
    pyramidGH2 <- ggplot(dataHombreMujerTotal, aes(x = ETARIO, y = TOTAL_CASOS, fill = SEXO)) + 
      geom_bar(data = subset(dataHombreMujerTotal, SEXO == "femenino"), stat = "identity") +
      geom_bar(data = subset(dataHombreMujerTotal, SEXO == "masculino"), stat = "identity") + 
      
      #scale_fill_discrete(limits = c("masculino", "femenino")) +
      scale_fill_manual("", values = c("masculino" = "#5B9BD5", "femenino" = "#ED7D31")) + 
      scale_y_continuous(limits = c(maxCasos$TOTAL_CASOS*-1, maxCasos$TOTAL_CASOS),
                         breaks = seq(maxCasos$TOTAL_CASOS*-1, maxCasos$TOTAL_CASOS, byNum),
                         labels = as.character(c(seq(maxCasos$TOTAL_CASOS, 0, byNum*-1), seq(byNum, maxCasos$TOTAL_CASOS, byNum)))) +
      #scale_y_continuous(limits = c(-1000, 1000),
      #                   breaks = seq(-1000, 1000, 100),
      #                   labels = as.character(c(seq(1000, 0, -100), seq(100, 1000, 100)))) +
      xlab("Grupo Etario") +
      ylab("Casos") +
      theme_bw(base_size = 13) +
      coord_flip() +
      theme(panel.background = element_blank(),
           panel.grid = element_blank())
    pyramidGH2
    
  })
  
  dataHombreMujerTable <- reactive({
    
    dataHombreMujerTable <- dataHombreMujer %>% 
      filter(YEAR %in% input$in_hm_year) %>%
      filter(SEXO %in% input$in_hm_sexo) %>%
      filter(ETARIO %in% input$in_hm_etario)
    
    dataHombreMujerNewSexo <- dataHombreMujerTable %>%
      distinct(SEXO)
    
    flgMasculino <- "masculino" %in% dataHombreMujerNewSexo$SEXO
    flgFemenino <- "femenino" %in% dataHombreMujerNewSexo$SEXO
    
    dataHombreMujerWider <- dataHombreMujerTable %>%
      add_column(flgMas = ifelse(flgMasculino, "TRUE", "FALSE")) %>%
      add_column(flgFem = ifelse(flgFemenino, "TRUE", "FALSE")) %>%
      pivot_wider(names_from = "SEXO",
                  values_from = "CASOS")
    
    dataHombreMujerWider <- dataHombreMujerWider %>%
      add_column(MASCULINO = as.numeric(ifelse(dataHombreMujerWider$flgMas, dataHombreMujerWider$masculino, 0))) %>%
      add_column(FEMENINO = as.numeric(ifelse(dataHombreMujerWider$flgFem, dataHombreMujerWider$femenino, 0)))
    
    dataHombreMujerWider <- dataHombreMujerWider %>%
      mutate(TOTAL_CASOS = MASCULINO + FEMENINO) %>%
      mutate(MASCULINO = ifelse(dataHombreMujerWider$flgMas, format(MASCULINO, nsmall = 0), "NA")) %>%
      mutate(FEMENINO = ifelse(dataHombreMujerWider$flgFem, format(FEMENINO, nsmall = 0), "NA")) %>%
      mutate(TOTAL_CASOS = format(TOTAL_CASOS, nsmall = 0)) %>%
      rename("Masc" = MASCULINO) %>%
      rename("Fem" = FEMENINO) %>%
      rename("TOTAL CASOS" = TOTAL_CASOS) %>%
      rename(GRUPO_ETARIO = ETARIO) %>%
      select(YEAR, GRUPO_ETARIO, Masc, Fem, "TOTAL CASOS")
    
    dataHombreMujerWider <- with(dataHombreMujerWider, dataHombreMujerWider[order(YEAR,GRUPO_ETARIO),])
  })
  
  output$disttable <- renderTable({
    
    dataHombreMujerTable()
    
  })
  
  output$downloadDataHombreMujer <- downloadHandler(
    filename = function() {
      "Cuadro.csv"
    },
    content = function(file) {
      write.csv(dataHombreMujerTable(), file, row.names = FALSE)
    }
  )
  
  ######################################################
  #  Topográficas
  ######################################################
  output$out_plot_donut_localizacion <- renderPlot({
    
    dataLocalizacionNew <- dataLocalizacion %>%
      filter(LOCALIZACION %in% input$in_local_localizacion) %>%
      filter(YEAR %in% input$in_local_year) %>%
      filter(SEXO %in% input$in_local_sexo) %>%
      arrange(YEAR, SEXO, LOCALIZACION)
    
    dataLocalizacionTotal <- dataLocalizacionNew %>%
      group_by(SEXO) %>%
      summarise(total_val = sum(CASOS)) %>%
      mutate(total_val = ifelse(is.na(total_val), 0, total_val))
    
    flgMasculino <- "masculino" %in% dataLocalizacionTotal$SEXO
    flgFemenino <- "femenino" %in% dataLocalizacionTotal$SEXO
    
    totalHombre <- dataLocalizacionTotal %>%
      filter(SEXO == "masculino")
    
    totalMujer <- dataLocalizacionTotal %>%
      filter(SEXO == "femenino")
    
    #output$out_text_hombre <- renderText(ifelse(flgMasculino, format(totalHombre$total_val, big.mark = ","), "NA"))
    #output$out_text_mujer <- renderText(ifelse(flgFemenino, format(totalMujer$total_val, big.mark = ","), "NA"))
    
    
    # Create data.
    data <- data.frame(
      category=c("femenino", "masculino"),
      count=c(ifelse(length(totalMujer$total_val) == 0, 0, totalMujer$total_val), 
              ifelse(length(totalHombre$total_val) == 0, 0, totalHombre$total_val))
    )
    
    #percentages
    data$fraction <- data$count / sum(data$count)
    
    #cumulative percentages (top of each rectangle)
    data$ymax <- cumsum(data$fraction)
    
    #the bottom of each rectangle
    data$ymin <- c(0, head(data$ymax, n=-1))
    
    #label position
    data$labelPosition <- (data$ymax + data$ymin) / 2
    
    #good label
    #data$label <- paste0(data$category, "\n value: ", data$count)
    data$label <- paste0(percent(data$fraction))
    
    data$label <- ifelse(data$label == "0.00%", "", data$label)
    
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
      geom_rect() +
      scale_fill_manual("", values = c("masculino" = "#5B9BD5", "femenino" = "#ED7D31")) + 
      #geom_text( x=5, aes(y=labelPosition, label=label, color=c("#17202A","#17202A")), size=4) + # x here controls label position (inner / outer)
      geom_text( x=3.5, aes(y=labelPosition, label=label), size=4) + # x here controls label position (inner / outer)
      #scale_fill_brewer(palette=3) +
      #scale_color_brewer(palette=3) +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none")
  })
  
  output$out_plot_localizacion <- renderPlot({
    
    dataLocalizacionNew <- dataLocalizacion %>%
      filter(LOCALIZACION %in% input$in_local_localizacion) %>%
      filter(YEAR %in% input$in_local_year) %>%
      filter(SEXO %in% input$in_local_sexo) %>%
      arrange(YEAR, SEXO, LOCALIZACION)
    
    dataLocalizacionTotal <- dataLocalizacionNew %>%
      group_by(SEXO) %>%
      summarise(total_val = sum(CASOS)) %>%
      mutate(total_val = ifelse(is.na(total_val), 0, total_val))
    
    flgMasculino <- "masculino" %in% dataLocalizacionTotal$SEXO
    flgFemenino <- "femenino" %in% dataLocalizacionTotal$SEXO
    
    totalHombre <- dataLocalizacionTotal %>%
      filter(SEXO == "masculino")
    
    totalMujer <- dataLocalizacionTotal %>%
      filter(SEXO == "femenino")
    
    output$out_text_hombre <- renderText(ifelse(flgMasculino, format(totalHombre$total_val, big.mark = ","), "NA"))
    output$out_text_mujer <- renderText(ifelse(flgFemenino, format(totalMujer$total_val, big.mark = ","), "NA"))
    
    
    dataLocalizacionTop10 <- dataLocalizacionNew %>%
      group_by(LOCALIZACION)  %>%
      summarise(total_val = sum(CASOS)) %>%
      top_n(10, total_val) %>%
      arrange(desc(total_val))
    
    dataLocalizacionNew <- dataLocalizacionNew %>%
      group_by(LOCALIZACION, SEXO)  %>%
      summarise(total_val = sum(CASOS)) %>%
      filter(LOCALIZACION %in% dataLocalizacionTop10$LOCALIZACION)

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
      scale_fill_manual("", values = c("masculino" = "#5B9BD5", "femenino" = "#ED7D31")) + 
      theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
      theme(legend.position = "none") +
      theme(panel.background = element_blank(),
            panel.grid = element_blank())
  })
  
  dataLocalizacionTable <- reactive({
    
    dataLocalizacionTable <- dataLocalizacion %>%
      filter(LOCALIZACION %in% input$in_local_localizacion) %>%
      filter(YEAR %in% input$in_local_year) %>%
      filter(SEXO %in% input$in_local_sexo) %>%
      arrange(YEAR, SEXO, LOCALIZACION)
    
    dataLocalizacionSexo <- dataLocalizacionTable %>%
      distinct(SEXO)
    
    flgMasculino <- "masculino" %in% dataLocalizacionSexo$SEXO
    flgFemenino <- "femenino" %in% dataLocalizacionSexo$SEXO
    
    dataLocalizacionWider <- dataLocalizacionTable %>%
      add_column(flgMas = ifelse(flgMasculino, "TRUE", "FALSE")) %>%
      add_column(flgFem = ifelse(flgFemenino, "TRUE", "FALSE")) %>%
      pivot_wider(names_from = "SEXO",
                  values_from = "CASOS")
    
    dataLocalizacionWider <- dataLocalizacionWider %>%
      add_column(MASCULINO = as.numeric(ifelse(dataLocalizacionWider$flgMas, ifelse(is.na(dataLocalizacionWider$masculino), 0, dataLocalizacionWider$masculino), 0))) %>%
      add_column(FEMENINO = as.numeric(ifelse(dataLocalizacionWider$flgFem, ifelse(is.na(dataLocalizacionWider$femenino), 0, dataLocalizacionWider$femenino), 0)))
    
    
    dataLocalizacionWider <- dataLocalizacionWider %>%
      mutate(TOTAL_CASOS = MASCULINO + FEMENINO) %>%
      mutate(MASCULINO = ifelse(dataLocalizacionWider$flgMas, format(MASCULINO, nsmall = 0), "NA")) %>%
      mutate(FEMENINO = ifelse(dataLocalizacionWider$flgFem, format(FEMENINO, nsmall = 0), "NA")) %>%
      mutate(TOTAL_CASOS = format(TOTAL_CASOS, nsmall = 0)) %>%
      rename("Masc" = MASCULINO) %>%
      rename("Fem" = FEMENINO) %>%
      rename("TOTAL CASOS" = TOTAL_CASOS) %>%
      select(YEAR, LOCALIZACION, Masc, Fem, "TOTAL CASOS")
    
    dataLocalizacionWider <- with(dataLocalizacionWider, dataLocalizacionWider[order(YEAR,LOCALIZACION),])
  })
  
  output$out_table_localizacion <- renderTable({
    
    dataLocalizacionTable()
    
  })
  
  output$downloadDataLocalizacion <- downloadHandler(
    filename = function() {
      "Cuadro.csv"
    },
    content = function(file) {
      write.csv(dataLocalizacionTable(), file, row.names = FALSE)
    }
  )
  
  ######################################################
  #  Pediatricos
  ######################################################
  output$out_plot_pediatrico_boygirl <- renderPlot({
    
    dataPediatricoTotal <- dataPediatrico %>%
      filter(YEAR %in% input$in_pediatrico_year) %>%
      filter(SEXO %in% input$in_pediatrico_sexo) %>%
      filter(DIAGNOSTICO %in% input$in_pediatrico_diagnostico_solido | DIAGNOSTICO %in% input$in_pediatrico_diagnostico_liquido) %>%
      group_by(SEXO) %>%
      summarise(total_val = sum(CASOS))
    
    flgMasculino <- "masculino" %in% dataPediatricoTotal$SEXO
    flgFemenino <- "femenino" %in% dataPediatricoTotal$SEXO
    
    dataPediatricoTotalBoy <- dataPediatricoTotal %>%
      filter(SEXO == "masculino")
    
    dataPediatricoTotalGirl <- dataPediatricoTotal %>%
      filter(SEXO == "femenino")
    
    output$out_text_boy <- renderText(ifelse(flgMasculino, format(dataPediatricoTotalBoy$total_val, big.mark = ","), "NA"))

    output$out_text_girl <- renderText(ifelse(flgFemenino, format(dataPediatricoTotalGirl$total_val, big.mark = ","), "NA"))
    
    
    # Create data.
    data <- data.frame(
      category=c("femenino", "masculino"),
      count=c(ifelse(length(dataPediatricoTotalGirl$total_val) == 0, 0, dataPediatricoTotalGirl$total_val), 
              ifelse(length(dataPediatricoTotalBoy$total_val) == 0, 0, dataPediatricoTotalBoy$total_val))
    )
    
    #percentages
    data$fraction <- data$count / sum(data$count)
    
    #cumulative percentages (top of each rectangle)
    data$ymax <- cumsum(data$fraction)
    
    #the bottom of each rectangle
    data$ymin <- c(0, head(data$ymax, n=-1))
    
    #label position
    data$labelPosition <- (data$ymax + data$ymin) / 2
    
    #good label
    #data$label <- paste0(data$category, "\n value: ", data$count)
    data$label <- paste0(percent(data$fraction))
    
    data$label <- ifelse(data$label == "0.00%", "", data$label)
    
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
      geom_rect() +
      scale_fill_manual("", values = c("masculino" = "#5B9BD5", "femenino" = "#ED7D31")) + 
      #geom_text( x=5, aes(y=labelPosition, label=label, color=c("#17202A","#17202A")), size=4) + # x here controls label position (inner / outer)
      geom_text( x=3.5, aes(y=labelPosition, label=label), size=4) + # x here controls label position (inner / outer)
      #scale_fill_brewer(palette=3) +
      #scale_color_brewer(palette=3) +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none")
  })
  
  output$out_plot_pediatrico_solido <- renderPlot({
    
    dataPediatrico <- dataPediatrico %>%
      filter(YEAR %in% input$in_pediatrico_year) %>%
      filter(SEXO %in% input$in_pediatrico_sexo) %>%
      filter(DIAGNOSTICO %in% input$in_pediatrico_diagnostico_solido)
    
    diagnostico <- dataPediatrico$DIAGNOSTICO
    tipo <- dataPediatrico$SEXO
    val <- dataPediatrico$CASOS
    df <- data.frame(x = diagnostico, y = val, sexo = tipo)
    
    ggplot(df, aes(x = x, y = y, fill = sexo)) +
      geom_bar(position = "stack", stat = "identity") + 
      xlab("Diagnósticos tumores  sólidos") +
      ylab("Casos") +
      theme_bw(base_size = 13) +
      scale_fill_manual("", values = c("masculino" = "#5B9BD5", "femenino" = "#ED7D31")) + 
      theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
      theme(legend.position = "none") +
      theme(panel.background = element_blank(),
            panel.grid = element_blank())
  })
  
  output$out_plot_pediatrico_liquido <- renderPlot({
    
    dataPediatrico <- dataPediatrico %>%
      filter(YEAR %in% input$in_pediatrico_year) %>%
      filter(SEXO %in% input$in_pediatrico_sexo) %>%
      filter(DIAGNOSTICO %in% input$in_pediatrico_diagnostico_liquido)
    
    diagnostico <- dataPediatrico$DIAGNOSTICO
    tipo <- dataPediatrico$SEXO
    val <- dataPediatrico$CASOS
    df <- data.frame(x = diagnostico, y = val, sexo = tipo)
    
    ggplot(df, aes(x = x, y = y, fill = sexo)) +
      geom_bar(position = "stack", stat = "identity") + 
      xlab("Diagnósticos tumores líquidos") +
      ylab("Casos") +
      theme_bw(base_size = 13) +
      scale_fill_manual("", values = c("masculino" = "#5B9BD5", "femenino" = "#ED7D31")) + 
      theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
      theme(legend.position = "none") +
      theme(panel.background = element_blank(),
            panel.grid = element_blank())
  })
  
  dataPediatricoSolidoWider <- reactive({
    
    dataPediatrico <- dataPediatrico %>%
      filter(YEAR %in% input$in_pediatrico_year) %>%
      filter(SEXO %in% input$in_pediatrico_sexo) %>%
      filter(DIAGNOSTICO %in% input$in_pediatrico_diagnostico_solido)
    
    dataPediatricoSexo <- dataPediatrico %>%
      distinct(SEXO)
    
    flgMasculino <- "masculino" %in% dataPediatricoSexo$SEXO
    flgFemenino <- "femenino" %in% dataPediatricoSexo$SEXO
    
    dataPediatricoSolidoWider <- dataPediatrico %>%
      add_column(flgMas = ifelse(flgMasculino, "TRUE", "FALSE")) %>%
      add_column(flgFem = ifelse(flgFemenino, "TRUE", "FALSE")) %>%
      pivot_wider(names_from = "SEXO",
                  values_from = "CASOS")
    
    dataPediatricoSolidoWider <- dataPediatricoSolidoWider %>%
      add_column(MASCULINO = as.numeric(ifelse(dataPediatricoSolidoWider$flgMas, dataPediatricoSolidoWider$masculino, 0))) %>%
      add_column(FEMENINO = as.numeric(ifelse(dataPediatricoSolidoWider$flgFem, dataPediatricoSolidoWider$femenino, 0)))
    
    dataPediatricoSolidoWider <- dataPediatricoSolidoWider %>%
      mutate(TOTAL_CASOS = MASCULINO + FEMENINO) %>%
      mutate(MASCULINO = ifelse(dataPediatricoSolidoWider$flgMas, format(MASCULINO, nsmall = 0), "NA")) %>%
      mutate(FEMENINO = ifelse(dataPediatricoSolidoWider$flgFem, format(FEMENINO, nsmall = 0), "NA")) %>%
      mutate(TOTAL_CASOS = format(TOTAL_CASOS, nsmall = 0)) %>%
      rename("Masc" = MASCULINO) %>%
      rename("Fem" = FEMENINO) %>%
      rename("TOTAL CASOS" = TOTAL_CASOS) %>%
      select(YEAR, DIAGNOSTICO, Masc, Fem, "TOTAL CASOS")
    
    dataPediatricoSolidoWider <- with(dataPediatricoSolidoWider, dataPediatricoSolidoWider[order(YEAR,DIAGNOSTICO),])
    
  })
  
  output$out_table_pediatrico_solido <- renderTable(
    
    dataPediatricoSolidoWider()
    
  )
  
  output$downloadDataPediatricoSolido <- downloadHandler(
    filename = function() {
      "Cuadro1.csv"
    },
    content = function(file) {
      write.csv(dataPediatricoSolidoWider(), file, row.names = FALSE)
    }
  )
  
  dataPediatricoLiquidoWider <- reactive({
    
    dataPediatrico <- dataPediatrico %>%
      filter(YEAR %in% input$in_pediatrico_year) %>%
      filter(SEXO %in% input$in_pediatrico_sexo) %>%
      filter(DIAGNOSTICO %in% input$in_pediatrico_diagnostico_liquido)
    
    dataPediatricoSexo <- dataPediatrico %>%
      distinct(SEXO)
    
    flgMasculino <- "masculino" %in% dataPediatricoSexo$SEXO
    flgFemenino <- "femenino" %in% dataPediatricoSexo$SEXO
    
    dataPediatricoLiquidoWider <- dataPediatrico %>%
      add_column(flgMas = ifelse(flgMasculino, "TRUE", "FALSE")) %>%
      add_column(flgFem = ifelse(flgFemenino, "TRUE", "FALSE")) %>%
      pivot_wider(names_from = "SEXO",
                  values_from = "CASOS")
    
    dataPediatricoLiquidoWider <- dataPediatricoLiquidoWider %>%
      add_column(MASCULINO = as.numeric(ifelse(dataPediatricoLiquidoWider$flgMas, dataPediatricoLiquidoWider$masculino, 0))) %>%
      add_column(FEMENINO = as.numeric(ifelse(dataPediatricoLiquidoWider$flgFem, dataPediatricoLiquidoWider$femenino, 0)))
    
    dataPediatricoLiquidoWider <- dataPediatricoLiquidoWider %>%
      mutate(TOTAL_CASOS = MASCULINO + FEMENINO) %>%
      mutate(MASCULINO = ifelse(dataPediatricoLiquidoWider$flgMas, format(MASCULINO, nsmall = 0), "NA")) %>%
      mutate(FEMENINO = ifelse(dataPediatricoLiquidoWider$flgFem, format(FEMENINO, nsmall = 0), "NA")) %>%
      mutate(TOTAL_CASOS = format(TOTAL_CASOS, nsmall = 0)) %>%
      rename("Masc" = MASCULINO) %>%
      rename("Fem" = FEMENINO) %>%
      rename("TOTAL CASOS" = TOTAL_CASOS) %>%
      select(YEAR, DIAGNOSTICO, Masc, Fem, "TOTAL CASOS")
    
    dataPediatricoLiquidoWider <- with(dataPediatricoLiquidoWider, dataPediatricoLiquidoWider[order(YEAR,DIAGNOSTICO),])
  })
  
  output$out_table_pediatrico_liquido <- renderTable({
    
    dataPediatricoLiquidoWider()

  })
  
  output$downloadDataPediatricoLiquido <- downloadHandler(
    filename = function() {
      "Cuadro2.csv"
    },
    content = function(file) {
      write.csv(dataPediatricoLiquidoWider(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
