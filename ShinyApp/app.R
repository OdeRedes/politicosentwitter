#
# This is a Shiny web application. 
#
# Librerias--------------------------------------------------------------
library(shiny)
library(shinythemes)
library(mongolite)
library(dplyr)
library(DT)
library(shinydashboard)
library(sqldf)
library(tidyverse)
library(lubridate)
library(shinycssloaders)
library(plotly)
library(highcharter)
library(tidyverse)
library(stringi)
library(stringr)
library(tidytext)
library(RColorBrewer)
library(emo)
library(writexl)
library(wordcloud2)
library(emojifont)
library(dygraphs)
library(xts)
require(visNetwork)

rm(list=ls()) #borramos del environment todos los elementos
options(scipen = 999)

## nos conectamos a mongolite -----------------------------------------
url_path = 'mongodb+srv://xxxxx:xxxxx@cluster0.xxxxx.mongodb.net/admin' #pen,, config
url_path_2 = 'mongodb+srv://xxxxx:xxxxx@cluster0.xxxxx.mongodb.net/test' # hcdn
url_path_3 = 'mongodb+srv://xxxxx:xxxxx@cluster0.xxxxx.mongodb.net/test' # hcsm
url_path_4 = 'mongodb+srv://xxxxx:xxxxx@cluster0.xxxxx.mongodb.net/admin' #otros, prov
url_path_5 = 'mongodb+srv://xxxxx:xxxxx@cluster0.xxxxx.mongodb.net/test' # data net + data colors

# descargamos los datasets necesarios -------------------------------------

## lista_politicxs ------

my_data <- mongo(collection = "lista_politicxs", # Data Table
                 db = "configuration_db", # DataBase
                 url = url_path, 
                 verbose = TRUE)
data_politicxs <- my_data$find(query = '{}')
### data crecimiento ------

data_crec_db <- mongo(collection = "data_crec", # Data Table
                      db = "CREC_db", # DataBase
                      url = url_path, 
                      verbose = TRUE)
data_crec <- data_crec_db$find(query = '{}')

my_data <- mongo(collection = "data_colors", # Data Table
                 db = "data_net", # DataBase
                 url = url_path_5, 
                 verbose = TRUE)
data_colors <- my_data$find('{}')

my_data_2 <- mongo(collection = "data_network_mensual", # Data Table
                   db = "data_net", # DataBase
                   url = url_path_5, 
                   verbose = TRUE)
data_net <- my_data_2$find('{}')
data_net <-data_net %>%
  filter(month_year > "2015-01-01")
data_net$month_year <- as.Date(data_net$month_year)
# my_query <- mongo(collection = "count_descargas", # Data Table
#                   db = "configuration_db", # DataBase
#                   url = url_path, 
#                   verbose = TRUE)
# count <- my_query$find(query = '{}')

# my_query <- mongo(collection = "data_politicos",
#                   db = "configuration_db", 
#                   url = url_path, 
#                   verbose = TRUE)
# data_politicos <- my_query$find(query = '{}')

# value boxes -------------------------------------------------------------
### bibliografia: ttps://www.r-bloggers.com/2018/06/valuebox-without-shinydashboard-2/
valueBox <- function(value, subtitle, icon, color) {
  div(class = "col-lg-3 col-md-6",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = "row",
                  div(class = "col-xs-3",
                      icon(icon, "fa-3x")
                  ),
                  div(class = ("col-xs-9 text-right"),
                      div(style = ("font-size: 25px; font-weight: bold;"),
                          textOutput(value)
                      ),
                      div(subtitle)
                  )
              )
          ),
          div(class = "panel-footer",
              div(class = "clearfix")
          )
      )
  )
}


# User Interface ----------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(".navbar{background-color:#c6c6c6;} .navbar{color: #C6C6C6;}")),
  # Application title
  tags$head(HTML("<title>Politic@s en Twitter</title>")), #Without company logo
  navbarPage(selected = "hola",
             title = div( img(src='https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/politicos-en-redes.jpg',
                              style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 60)),
             
             tags$head(
               # Note the wrapping of the string in HTML()
               tags$style(HTML("
       .navbar  {
         background-color: #C6C6C6;
         color: #15202B;
       }
         .navbar-default {color: #C6C6C6;}
     ")),
             ),
             # tags$footer("Atribución 2.5 Argentina (CC BY 2.5 AR)", align = "center", style = "
             #           position:absolute;
             #           bottom:0;
             #           width:100%;
             #           height:30px;   /* Height of the footer */
             #           color: #15202B;
             #           padding: 10px;
             #           z-index: 1000;"),
             
             # Descargá los tweets -----------------------------------------------------
             
             tabPanel(value = "hola", 
                      title = "Descargá", 
                      sidebarLayout(
                        sidebarPanel(
                          ### WIDGET 1 --> text
                          radioButtons(inputId='tipo_organismo', 
                                       label= h4('Seleccioná la Categoria:'), 
                                       choiceValues = unique(data_politicxs$Tipo_organismo_2), 
                                       choiceNames = c("Tod@s","Diputados/as Nacionales", 
                                                       "Funcionario/as Provinciales",
                                                       "Poder Ejecutivo Nacional",
                                                       "Senadores Nacionales",
                                                       "Otras figuras públicas"),
                                       selected = NULL), ### SELECCIONAR -
                          ### WIDGET 2 --> descargá
                          
                          uiOutput("seleccion_usuario"),
                          br(),
                          div(shiny::HTML("<h4>Descargá la data*:</h4> "),
                              downloadButton('downloadcsv',"CSV"), 
                              downloadButton('downloadxlsx',"XLSX" )),
                          a("*¿Qué información me trae esto?",target="_blank",href="https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/descarga_timeline_usuarios.pdf"),
                          
                          br(), 
                          br(),
                          
                          div(shiny::HTML("<h4>Descargá la data por categoria**:</h4> "),
                              withSpinner(downloadButton('downloadcsv_all',"CSV"))),
                          helpText('**Puede tardar entre 5 y 15 min esta descarga. Se recomienda evitar utilizar la categoria "Tod@s" porque puede demorar màs tiempo.')
                          
                        ),
                        # Show a plot of the generated distribution
                        mainPanel(
                          dataTableOutput("data_base")
                        )        
                      )
             ),
             
             
             # Visualiza ---------------------------------------------------------------
             
             
             tabPanel("Visualizá", sidebarLayout(
               sidebarPanel(
                 ### WIDGET 1 --> text
                 
                 
                 ### WIDGET 2 --> descargá
                 
                 
                 radioButtons(inputId='tipo_organismo2', 
                              label=h4('Seleccioná la Categoria:'), 
                              choiceValues = unique(data_politicxs$Tipo_organismo_2), 
                              choiceNames = c("Tod@s","Diputados/as Nacionales", 
                                              "Funcionario/as Provinciales",
                                              "Poder Ejecutivo Nacional",
                                              "Senadores Nacionales",
                                              "Otras figuras públicas"),
                              selected = NULL), ### SELECCIONAR -
                 
                 
                 ### WIDGET 2 --> descargá
                 
                 uiOutput("seleccion_usuario_2")
               ), 
               mainPanel(
                 
                 fluidRow(id="main-panel_1",
                          
                          valueBox(value = "favs",
                                   subtitle = "promedio de favs",
                                   icon = "heart",
                                   color = alpha(colour = "#FE435C", alpha = 0.5 )),
                          
                          valueBox(value = "rtweet",
                                   subtitle = "promedio de rtweets",
                                   icon = "retweet",
                                   color = alpha(colour = "#31C485", alpha = 0.5 )),
                          
                          valueBox(value = "followers",
                                   subtitle = "cantidad de followers",
                                   icon = "user-friends", 
                                   color = alpha(colour = "#79E7FB", alpha = 0.5 )),
                 ),
                 
                 
                 
                 # Frontpage - tweet volume plots - start ----------------------------------
                 
                 br(),
                 br(),
                 fluidRow(
                   tabBox(
                     width = 12,
                     tabPanel(
                       status = "primary",
                       title = "Cantidad de Interacciones",
                       br(),
                       withSpinner(dygraphOutput("plot_rt_favs_progress", height='400px',width='100%'))
                     ),
                     tabPanel(
                       status = "success",
                       title = "Cantidad de seguidores",
                       br(),
                       withSpinner(dygraphOutput("plot_followers", height='400px',width='100%')), 
                       div(align = "right",
                           shiny::HTML("<h4>Descargá la data*:</h4> "),
                           downloadButton('downloadcsv_fr_foll',"CSV"), 
                           downloadButton('downloadxlsx_fr_foll',"XLSX" ), 
                           br(),
                           a(align = "right", 
                             "*¿Qué información me trae esto?",
                             target="_blank",
                             href="https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/descarga_interacciones_usuaros.pdf")),
                       br(),
                       br(), 
                       div(align = "right",
                           shiny::HTML("<h4>Descargá la data para tod@s:</h4> "),
                           downloadButton('downloadcsv_all2',"CSV"))
                     ), 
                     
                     tabPanel(
                       uiOutput("dateRange"),
                       status = "success_2",
                       title = "Wordcloud",
                       br(),
                       withSpinner(wordcloud2Output("wordcloud", height='400px',width='100%'))),
                     tabPanel(
                       uiOutput("dateRange_emo"),
                       status = "success_2",
                       title = "Emojis",
                       br(),
                       withSpinner(plotlyOutput("emoji",  height='400px',width='100%')))
                     
                   ))
               )
             )
             ),
             
             
             tabPanel("Grafo", sidebarLayout(
               sidebarPanel(
                 uiOutput("dateRange_net"),
                 br(),
                 checkboxGroupInput("checkGroup", label = h4("Seleccioná las Categorias: "), 
                                    choiceValues = unique(data_politicxs$Tipo_organismo_2), 
                                    choiceNames = c("Tod@s",
                                                    "Diputados/as Nacionales", 
                                                    "Funcionario/as Provinciales",
                                                    "Poder Ejecutivo Nacional",
                                                    "Senadores Nacionales",
                                                    "Otras figuras públicas"),
                                    selected = "TODXS"),
                 br(),
                 helpText(
                   tags$ul("Puntos a tener en consideración:",
                           tags$li("Esta nube muestra la interacción de los usuarios descargados en esta app durante el periodo seleccionado en base a retweets."),
                           tags$li("Entre más cercana al día de la fecha sea el periodo seleccionado, mayor será la cantidad de conexiones entre los usuarios, debido a reestricciones de descarga de la API de Twitter."),
                           tags$li("El tamaño de los nodos responde a la cantidad de retweets obtenidos por el usuario y el color al sector político al que pertenecen."),
                           tags$li("La actualización de esta nube es mensual -primero de cada mes- y los cambios observables en la misma también son mes a mes."),
                         #  style="text-align:justify;color:black;background-color:#c6c6c6;padding:15px;border-radius:10px"
                         ),
                  # style="text-align:justify;color:black;background-color:#c6c6c6;padding:15px;border-radius:10px"
                  )
               ),
               mainPanel(
                 withSpinner(visNetworkOutput("red_rt", height='700px',width='100%'))
               )
             )
             ),
             
             
             # Frontpage - tweet volume plots - end ------------------------------------
             
             
             
             # about us ----------------------------------------------------------------
             
             
             tabPanel("Conocé más", 
                      column(6, includeHTML("https://github.com/Observatorio-de-Redes/politicosentwitter/raw/main/ShinyApp/www/conocemas.html"))
             )# cierra aboutus
  )
)



# Server ------------------------------------------------------------------


server <- function(input, output) {
  options(scipen = 999)
  
  # primer tab --------------------------------------------------------------
  df.filt <- reactive({
    df.filt=df[data_politicxs$Tipo_organismo_2==input$user_name,] 
    df.filt
  })
  
  output$seleccion_usuario <- renderUI({
    selectInput(inputId="user_name", h4("Seleccioná el usuario:"), 
                choices = sort(unique(data_politicxs[data_politicxs$Tipo_organismo_2 == input$tipo_organismo, 'screen_name'])), ### SELECCIONAR database
                selected = "SergioMassa"
    )
  })
  
  
  
  
  
  ## boton de descargq    
  output$downloadcsv <- downloadHandler(
    filename = function(){paste0(input$user_name, "_timeline_database.csv")}, 
    content = function(fname){
      my_query <- mongo(collection = paste0(input$user_name),
                        db = paste0(data_politicxs[data_politicxs$screen_name == input$user_name, 'database']), 
                        url = paste0(data_politicxs[data_politicxs$screen_name == input$user_name, 'url_path']), 
                        verbose = TRUE)
      database <- my_query$find(query = '{}')   
      database <- database %>% arrange(desc(created_at))
      write.csv(database, fname, row.names = F)
      my_data1 <- mongo(collection = "data_full_csv", # Data Table
                        db = "data_count", # DataBase
                        url = url_path_5,
                        verbose = TRUE)
      l <- my_data1$find('{}')
      l[l$screen_name == input$user_name, "full_csv"] <- l[l$screen_name == input$user_name, "full_csv"] +1
      my_data1$drop()
      my_data1$insert(l)
    }
  )
  
  output$downloadxlsx <- downloadHandler(
    filename = function(){paste0(input$user_name, "_timeline_database.xlsx")}, 
    content = function(fname){
      my_query <- mongo(collection = paste0(input$user_name),
                        db = paste0(data_politicxs[data_politicxs$screen_name == input$user_name, 'database']), 
                        url = paste0(data_politicxs[data_politicxs$screen_name == input$user_name, 'url_path']), 
                        verbose = TRUE)
      database <- my_query$find(query = '{}')   
      database <- database %>% arrange(desc(created_at))
      write_xlsx(database, fname)
      
      my_data2 <- mongo(collection = "data_full_excel", # Data Table
                        db = "data_count", # DataBase
                        url = url_path_5,
                        verbose = TRUE)
      l <- my_data2$find('{}')
      l[l$screen_name == input$user_name, "full_excel"] <-  l[l$screen_name == input$user_name, "full_excel"]  +1
      my_data2$drop()
      my_data2$insert(l)
      
    }
  )
  
  output$downloadcsv_all <- downloadHandler(
    filename = function(){paste0(input$tipo_organismo, "_timeline_database.csv")},
    content = function(fname){
      for(i in data_politicxs[data_politicxs$Tipo_organismo_2 == input$tipo_organismo, 'screen_name']){
        my_query <- mongo(collection = i,
                          db = paste0(data_politicxs[data_politicxs$screen_name == i, 'database']),
                          url = paste0(data_politicxs[data_politicxs$screen_name == i, 'url_path']),
                          verbose = TRUE)
        database <- my_query$find(query = '{}')
        data_base_all <- dplyr::bind_rows(database, data_base) 
      }
      write.csv(data_base_all, fname, row.names = F)
      
    }
  )

  
  
  ####
  output$data_base <- DT::renderDataTable({
    data_politicxs_table <- data_politicxs %>% 
      filter(data_politicxs$Tipo_organismo_2 == input$tipo_organismo) %>% 
      arrange(desc(as.numeric(followers_count))) %>%
      rename( "Usuario" = screen_name, 
              "Descripción" = description,
              "Organismo" = Tipo_organismo,
              "Imagen" =image, 
              "Seguidores"=followers_count, 
              "Nombre" =name) %>%
      select('Imagen', Usuario, Nombre, Descripción, 'Seguidores') 
    
    
    DT::datatable(data_politicxs_table, escape = FALSE) # HERE
  })
  
  # segundo / visualizacion tab --------------------------------------------------------------
  
  output$seleccion_usuario_2 <- renderUI({
    selectInput(inputId="user_name_2", h4("Seleccioná el usuario:"), 
                choices = sort(unique(data_politicxs[data_politicxs$Tipo_organismo_2 == input$tipo_organismo2, 'screen_name'])), ### SELECCIONAR database
                selected = "SergioMassa"
    )
  })
  
  
  
  ### select user 2 2021-02-18
  df.filt2 <- reactive({
    my_query_2 <- mongo(collection = paste0(input$user_name_2),
                        db = paste0(data_politicxs[data_politicxs$screen_name == input$user_name_2, 'database']), 
                        url = paste0(data_politicxs[data_politicxs$screen_name == input$user_name_2, 'url_path']), 
                        verbose = TRUE)
    df.filt2 <- my_query_2$find(query = '{}')   
    #df.filt2 <- data.frame(lapply(df.filt2, as.character), stringsAsFactors=FALSE)
    df.filt2
  })
  
  df.filt3 <- reactive({
    
    # data_crec_db_3 <- mongo(collection = "data_crec", # Data Table
    #                         db = "CREC_db", # DataBase
    #                         url = url_path, 
    #                         verbose = TRUE)
    # data_crec_3 <- data_crec_db_3$find(query = '{}')
    df.filt3 <- data_crec[data_crec$screen_name == input$user_name_2,]
    
    df.filt3
  })
  
  
  output$downloadcsv_all2 <- downloadHandler(
    filename = function(){paste0( "todxs_interacciones_database.csv")},
    content = function(fname){
      write.csv(data_crec, fname, row.names = F)}
  )
  ### primer granito --> followes
  
  
  output$rtweet <- renderText({ 
    df.filt2_rt <- df.filt2()
    round(mean(as.numeric(df.filt2_rt[df.filt2_rt$is_retweet == F, "retweet_count"]), na.rm = T),1)
  })
  
  output$favs<- renderText({ 
    df.filt2_fav <- df.filt2()
    round(mean(as.numeric(df.filt2_fav[df.filt2_fav$is_retweet == F, "favorite_count"]), na.rm = T),1)
    
  })
  
  output$followers<- renderText({ 
    df.filt3_foll <- df.filt3()
    paste0(df.filt3_foll[df.filt3_foll$date == max(as.Date(df.filt3_foll$date), na.rm=T), "followers_count"])
  })
  
  
  output$plot_rt_favs_progress <- renderDygraph({
    
    df_rtfav <- df.filt2()  %>%
      filter(is_retweet == F) %>%
      select(created_at, retweet_count, favorite_count) %>%
      mutate(Retweets = as.numeric(as.character(retweet_count)), 
             Favoritos = as.numeric(as.character(favorite_count)), 
             time =as.POSIXct(as.character(created_at, "%Y-%m-%d %H:%M:S"))) %>%
      # filter(between(time , input$date[1], input$date[2])) %>%
      select(time, Favoritos, Retweets) %>%
      arrange(time) 
    don=xts( x=df_rtfav[,-1], order.by=df_rtfav$time)
    dygraph(don,  main = paste0("Evolución en la cantidad de Interacciones de @",input$user_name_2 )) %>%
      dyRangeSelector() %>%
      dyOptions(maxNumberWidth = 999) %>%
      dySeries("Favoritos",  color = alpha(colour = "#FE435C", alpha = 0.5 )) %>%
      dySeries("Retweets", color = alpha(colour = "#31C485", alpha = 0.5 ))
  })
  
  
  
  output$plot_followers <- renderDygraph({
    
    df3 <- df.filt3()  %>%
      filter(date >="2021-02-11" & screen_name == input$user_name_2) %>%
      select(date, followers_count) %>%
      mutate(Followers = as.numeric(as.character(followers_count)), 
             time = as.Date(as.character(date, "%Y-%m-%d")) ) %>%
      select(time, Followers) %>%
      arrange(time)
    
    don_3=xts( x=df3[,-1], order.by=df3$time)
    dygraph(don_3,  main = paste0("Evolución en la cantidad de Seguidores de @", input$user_name_2)) %>%
      dySeries("V1", label ="Followers" , color = "#62c6d9", fillGraph = T) %>%
      dyOptions(maxNumberWidth = 999) %>%
      dyRangeSelector() 
    
    
    
  })
  
  output$downloadcsv_fr_foll <- downloadHandler(
    filename = function(){paste0(input$user_name_2, "_interacciones_database.csv")}, 
    content = function(fname){
      
      database <- df.filt3() %>% 
        filter(date >="2021-02-11" & screen_name == input$user_name_2)  %>% 
        arrange(desc(date))
      write.csv(database, fname)
      my_data4 <- mongo(collection = "data_interacions_csv", # Data Table
                        db = "data_count", # DataBase
                        url = url_path_5,
                        verbose = TRUE)
      l <- my_data4$find('{}')
      l[l$screen_name == input$user_name, "interacions_csv"] <- l[l$screen_name == input$user_name, "interacions_csv"] +1
      my_data4$drop()
      my_data4$insert(l)
    }
  )
  
  output$downloadxlsx_fr_foll <- downloadHandler(
    filename = function(){paste0(input$user_name, "_interacciones_database.xlsx")}, 
    content = function(fname){
      database <- df.filt3() %>% 
        filter(date >="2021-02-11" & screen_name == input$user_name_2)  %>% 
        arrange(desc(date))
      write_xlsx(database, fname)
      my_data3 <- mongo(collection = "data_interacions_excel", # Data Table
                        db = "data_count", # DataBase
                        url = url_path_5,
                        verbose = TRUE)
      l <- my_data3$find('{}')
      l[l$screen_name == input$user_name, "interacions_excel"] <-l[l$screen_name == input$user_name, "interacions_excel"] +  1
      my_data3$drop()
      my_data3$insert(l)
    }
  )
  
  
  output$dateRange <- renderUI(
    dateRangeInput('dateRange2',
                   label = shiny::HTML('<h4> Seleccioná el periodo de tiempo: </h4> '),
                   start = min(as.Date(df.filt2()$created_at), na.rm = T), end = max(as.Date(df.filt2()$created_at), na.rm = T),
                   min = min(as.Date(df.filt2()$created_at), na.rm = T), max = max(as.Date(df.filt2()$created_at), na.rm = T),
                   separator = " - ", format = "yyyy-mm-dd",
                   startview = 'year', language = 'es', weekstart = 1
    )
  )
  
  
  
  df.filt4 <- reactive({
    wcloud <- df.filt2()
    wcloud$created_at <- as.Date(wcloud$created_at)
    df.filt4 <- wcloud[((as.Date(wcloud$created_at) > as.Date(input$dateRange2[1])) & (as.Date(wcloud$created_at) < as.Date(input$dateRange2[2]))) & (wcloud$is_retweet == F),
                       c("is_retweet", "text")]
    df.filt4
  })
  
  output$wordcloud <- renderWordcloud2({
    
    stop_words_es <- read.table("https://github.com/Alir3z4/stop-words/raw/master/spanish.txt")
    names(stop_words_es)[1] <- "word"
    df.filt4() %>%
      mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
             text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
             text = str_remove_all(text, "[^\x01-\x7F]")) %>% 
      unnest_tokens(word, text, token = "tweets") %>%
      filter(!word %in% stop_words_es$word,
             !word %in% str_remove_all(stop_words_es$word, "'"),
             str_detect(word, "[a-z]"),
             !str_detect(word, "^#"),         
             !str_detect(word, "@\\S+"), 
             !nchar(as.character(word)) <= 3) %>%
      # filter(!word %in% stop_words$word,
      #        !word %in% str_remove_all(stop_words$word, "'"),
      #        str_detect(word, "[a-z]"),
      #        !str_detect(word, "^#"),         
      #        !str_detect(word, "@\\S+")) %>%
      count(word, sort = TRUE) %>%
      wordcloud2(color = brewer.pal(14, "Set3")  #size=input$size
      )
  })
  
  
  
  output$dateRange_emo <- renderUI(
    dateRangeInput('dateRange2_emo',
                   label = shiny::HTML('<h4> Seleccioná el periodo de tiempo: </h4> '),
                   start = min(as.Date(df.filt2()$created_at), na.rm = T), end = max(as.Date(df.filt2()$created_at), na.rm = T),
                   min = min(as.Date(df.filt2()$created_at), na.rm = T), max = max(as.Date(df.filt2()$created_at), na.rm = T),
                   separator = " - ", format = "yyyy-mm-dd",
                   startview = 'year', language = 'es', weekstart = 1
    )
  )
  
  
  
  df.filt5 <- reactive({
    emojis_filt <- df.filt2()
    emojis_filt$created_at <- as.Date(emojis_filt$created_at)
    
    df.filt5 <- emojis_filt[((as.Date(emojis_filt$created_at) > as.Date(input$dateRange2_emo[1])) & (as.Date(emojis_filt$created_at) < as.Date(input$dateRange2_emo[2]))) &
                              (emojis_filt$is_retweet == F), c("is_retweet", "text")]
    df.filt5
  })
  
  output$emoji <- renderPlotly({
    
    df.emoji <- df.filt5() %>%
      mutate(emoji = ji_extract_all(text)) %>%
      unnest(cols = c(emoji)) %>%
      select(emoji) %>%
      count(emoji, sort = TRUE) %>% 
      arrange(desc(n))%>%
      head(10) 
    
    fig <- plot_ly(df.emoji, x = ~emoji, y = ~n, type = 'bar', 
                   text =  ~emoji, textposition = 'auto', size = 14, 
                   marker = list(color = 'rgb(158,202,225)'))
    fig <- fig %>% layout(xaxis = list(categoryorder = "array", categoryarray = order(df.emoji$n))) 
    fig %>% layout(title = list(title = "Emojis más frecuentes utilizados por",
                                titlefont = list(size = 28, color = "orange", family = "Arial")),
                   xaxis = list(title = "Emoji"),
                   yaxis = list(title = "Cantidad"))
    
  })
  
  output$dateRange_net <- renderUI(
    dateRangeInput('dateRange3',
                   label = shiny::HTML('<h4> Seleccioná el periodo de tiempo: </h4> '),
                   start =  max(data_net$month_year, na.rm = T)-89,  end = max(data_net$month_year, na.rm = T),
                   min = min(data_net$month_year, na.rm = T), 
                   max = max(data_net$month_year, na.rm = T),
                   separator = " - ", format = "yyyy-mm-dd",
                   startview = 'year', language = 'es', weekstart = 1)
  )
  
  
  data_net_reactive <- reactive({
    lista <- unique(data_politicxs[data_politicxs$Tipo_organismo_2 %in% input$checkGroup, "user_id"])
    data_net_reactive <- data_net[((data_net$month_year > as.Date(input$dateRange3[1])) & 
                                     (data_net$month_year < as.Date(input$dateRange3[2]))) & 
                                    ((data_net$user_id %in% lista) &  (data_net$user_id %in% lista)), ]
    
  })
  
  output$red_rt <- renderVisNetwork({
    
    data_net_net <- data_net_reactive()
    
    nodes <-  gather(data_net_net, key = "tipo", value = "identificacion", 
                     c("user_id","retweet_user_id"))
    nodes <- sqldf("SELECT *, COUNT(*) as count FROM nodes 
               GROUP BY identificacion HAVING COUNT(*)")
    nodes <- nodes %>% select(identificacion, count)
    nodes <- nodes[!is.na(nodes$identificacion),]
    nodes$id_2 = nodes$identificacion
    nodes <- nodes %>% rename(id = identificacion, label = id_2, value = count)
    data_colors$id <- as.character(as.numeric(data_colors$id))
    # unimos con data colors
    nodes <- left_join( data_colors, nodes, by = "id")  %>% 
      select(id, value, screen_name, color) %>%
      rename(label=screen_name) 
    
    nodes[ is.na(nodes$value ), "value"] <- 0
    
    # armamos los edges -------------------------------------
    data_net_net$value <- as.numeric(as.character(data_net_net$value))
    links <- data_net_net %>% 
      filter(user_id %in% data_colors$id &
               retweet_user_id %in% data_colors$id ) %>%
      group_by(retweet_user_id, user_id) %>% 
      summarise(cantidad = sum(value)) %>%
      rename(from = user_id,
             to = retweet_user_id,
             friendship = cantidad)
    links <- links[!is.na(links$from),]
    links <- links[!is.na(links$friendship),]
    links <- links[!is.na(links$to),]
    
    
    visNetwork(nodes, links, main = list(text = paste0('Interacción en Twitter desde el "',day(as.Date(input$dateRange3[1])),'-',month(as.Date(input$dateRange3[1])),'-', year(as.Date(input$dateRange3[1])),
                                                       '" al "', day(as.Date(input$dateRange3[2])),'-',month(as.Date(input$dateRange3[2])),'-', year(as.Date(input$dateRange3[2])), '"'), 
                                         style = "font-family:Arial;color:#15202b;font-size:20px;text-align:center;")) %>%
      visIgraphLayout() %>%
      visNodes(
        shape = "dot",
        shadow = list(enabled = TRUE, size = 10)
      ) %>%
      visEdges(
        shadow = FALSE,
        color = list(color = "#C4C4C4", highlight = "#C4C4C4"), length = 1000
      ) %>%
      visOptions(highlightNearest = list(enabled = T, degree = 1,
                                         hover = T)) %>%
      visPhysics(enabled = F, solver = "repulsion", repulsion = list(nodeDistance = 1000)) %>%
      visInteraction(navigationButtons = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
