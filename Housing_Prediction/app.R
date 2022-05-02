library(shiny)
library(shinydashboard)
library(leaflet)
library("httr")
library(mlbench)
library(jsonlite)
library(dplyr)
library(shinyBS)
library(scales)
library(shinyWidgets) # for pretty slide
library(DataRobotColors)

dat <- read.csv("Boulder_Housing_Student_4326.csv") %>% 
  # rename("Latitude" = Y, "Longitude" = X) %>% 
  select("year","TotalFinishedSF", "mainfloorSF",    
         "nbrBedRoom","carStorageSF","qualityCodeDscr",
         "price","Latitude","Longitude") %>% 
  filter(price <= 3000000 & price >= 50000) %>% 
  mutate(id = seq(1,n()))

quality_codes <- c("EXCEPTIONAL 1 ","EXCELLENT++ ", "EXCELLENT ","VERY GOOD ++ ",
"VERY GOOD + ","VERY GOOD ","GOOD ++ ","GOOD + ","GOOD ",
"AVERAGE ++ ","AVERAGE + ","AVERAGE ")

## API
source("keys.r")
API_URL       <- "https://cfds-ccm-prod.orm.datarobot.com"
API_TOKEN     <-  API_TOKEN     #'YOUR API TOKEN'
DATAROBOT_KEY <-  DATAROBOT_KEY #'YOUR DATAROBOT KEY'
USERNAME      <-  USERNAME      #"YOUR USE NAME"
DEPLOYMENT_ID <- '62202e003b4a705bda0f62ee'
URL <- paste0(API_URL, "/predApi/v1.0/deployments/", DEPLOYMENT_ID, "/predictions")
URL_predex <- paste0(API_URL, "/predApi/v1.0/deployments/", DEPLOYMENT_ID, "/predictionExplanations")

### test API ####
response <- POST(URL,
                 body = rjson::toJSON(unname(split(dat[1,], 1:1))),
                 add_headers("datarobot-key" = DATAROBOT_KEY),
                 httr::content_type_json(),
                 authenticate(USERNAME, API_TOKEN, type = "basic"))
httr::content(response, simplifyVector=TRUE)

# also returns prediction value, can use one call when pred/ex enabled
pred_exp <- POST(URL_predex,
                 body = rjson::toJSON(unname(split(dat[1,], 1:1))),
                 add_headers("datarobot-key" = DATAROBOT_KEY),
                 httr::content_type_json(),
                 authenticate(USERNAME, API_TOKEN, type = "basic"))
httr::content(pred_exp, simplifyVector=TRUE)
#################

# get_pred <- function(Latitude,Longitude,qualityCodeDscr,carStorageSF,
#                      nbrBedRoom, TotalFinishedSF){
#   ## quick model to estimate mainfloor from finished
#   mainfloorSF_lm <- lm(mainfloorSF ~ TotalFinishedSF, data = dat)
#   mainfloorSF <- predict(mainfloorSF_lm,
#                          data.frame(TotalFinishedSF = TotalFinishedSF))
#   ####
#   predictions <- dat[1,] 
#   predictions$id  <- 1
#   predictions$Latitude <- Latitude
#   predictions$Longitude <- Longitude
#   predictions$qualityCodeDscr  <- qualityCodeDscr
#   predictions$carStorageSF <- carStorageSF
#   predictions$year <- 2020 
#   predictions$nbrBedRoom <- nbrBedRoom
#   predictions$mainfloorSF <- mainfloorSF # prediction
#   predictions$TotalFinishedSF <- TotalFinishedSF
#   response <- POST(URL, 
#                    body = rjson::toJSON(unname(split(predictions, 1:nrow(predictions)))),
#                    add_headers("datarobot-key" = DATAROBOT_KEY),
#                    httr::content_type_json(),
#                    authenticate(USERNAME, API_TOKEN, type = "basic")
#   )
#   data <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector=FALSE)$data[[1]]$prediction
#   return(data)
# }
get_pred_exp <- function(Latitude,Longitude,qualityCodeDscr,carStorageSF,
                     nbrBedRoom, TotalFinishedSF){
  ## quick model to estimate mainfloor from finished
  mainfloorSF_lm <- lm(mainfloorSF ~ TotalFinishedSF, data = dat)
  mainfloorSF <- predict(mainfloorSF_lm,
                         data.frame(TotalFinishedSF = TotalFinishedSF))
  ####
  predictions <- dat[1,] 
  predictions$id  <- 1
  predictions$Latitude <- Latitude
  predictions$Longitude <- Longitude
  predictions$qualityCodeDscr  <- qualityCodeDscr
  predictions$carStorageSF <- carStorageSF
  predictions$year <- 2020 
  predictions$nbrBedRoom <- nbrBedRoom
  predictions$mainfloorSF <- mainfloorSF # prediction
  predictions$TotalFinishedSF <- TotalFinishedSF
  response <- POST(URL_predex, 
                   body = rjson::toJSON(unname(split(predictions, 1:nrow(predictions)))),
                   add_headers("datarobot-key" = DATAROBOT_KEY),
                   httr::content_type_json(),
                   authenticate(USERNAME, API_TOKEN, type = "basic")
  )
  response_content <- fromJSON(content(response, "text", encoding = "UTF-8"), 
                               simplifyVector=TRUE)
  cat(response$times,"\n")
  pred_price <- response_content$data$prediction
  pred_exp   <- response_content$data$predictionExplanations[[1]]
  return(list(pred_price = pred_price, 
              pred_exp   = pred_exp))
}
####

ui = dashboardPage(
  dashboardHeader(title = "Home Price Demo"),
  dashboardSidebar(
    # sidebarMenu(
    #   menuItem(
    #   "Home Prediction Menu",
    #   tabName = "Dashboard",
    #   icon = icon("table")
    # )
    # ),
    switchInput("pred_mode", 
                label = "Predictions", 
                value = FALSE),
    # checkboxInput("pred_mode", label = "Turn on Prediction Mode", value = FALSE),
    sliderInput("TotalFinishedSF", "Total Finished SqFt:",
                min = 500, max = 5000, value = 2050
    ),
    sliderInput("nbrBedRoom", "Number of Bedrooms:",
                min = 1, max = 7, value = 3
    ),
    sliderInput("carStorageSF", "Garage SqFt:",
                min = 0, max = 1500, value = 450
    ),
    selectInput("qualityCodeDscr", "Overall Quality:",
                quality_codes, selected = "GOOD "
    )
  ),
  dashboardBody(
    tags$script(HTML("addClass(‘sidebar-mini’);")),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    fluidRow(column(width = 12,
                    leafletOutput('map', height = "90vh")))
  )
)
server = function(input, output, session) {
  
  pred <- reactiveValues(pred_price = NULL,
                         pred_exp = NULL)

  map = createLeafletMap(session, 'map')
  
  pal <- colorNumeric(
    palette = "PiYG",
    domain = c(50000,3000000))
  
  DR_colors <- as.character(DataRobot_get_palettes()$DR_Diverging)
  pal_DR <- colorFactor(
    palette = DR_colors,
    domain = dat$price
  )
  
  session$onFlushed(once = TRUE, function() {

    pnt_labels <- paste("<b>Actual Price:</b>",
                       scales::dollar(dat$price),"<br>",
                       "<b>Finished SqFt:</b>",
                       dat$TotalFinishedSF,"<br>",
                       "<b>Bedrooms:</b>",
                       dat$nbrBedRoom,"<br>",
                       "<b>Garage SqFt:</b>",
                       dat$carStorageSF,"<br>",
                       "<b>Quality:</b>",
                       dat$qualityCodeDscr)
    
    output$map <- renderLeaflet({
      leaflet(dat) %>%
        addCircleMarkers(~ Longitude, ~ Latitude,
                         color = ~pal_DR(price),
                         label = lapply(pnt_labels, htmltools::HTML)) %>%
        setView(lat = 40.03494514594114,lng = -105.27329016435645,zoom = 11) %>%
        addProviderTiles("CartoDB.Positron",group = "CartoDB.Positron") %>%
        addProviderTiles("OpenStreetMap",group = "OpenStreetMap") %>%
        addProviderTiles("Esri.WorldImagery",group = "Esri.WorldImagery") %>%
        addProviderTiles("CartoDB.DarkMatter",group = "CartoDB.DarkMatter") %>%
        addLayersControl(
          baseGroups = c("CartoDB.Positron", "OpenStreetMap",
                         "Esri.WorldImagery","CartoDB.DarkMatter"),
          position = "topright")
    })
  })

  observe({
    req(input$pred_mode)
    
    click <- input$map_click
    if(is.null(click))
      return()

    ##get prediction
    pred_details <- get_pred_exp(click$lat, click$lng,
                          input$qualityCodeDscr, 
                          input$carStorageSF, input$nbrBedRoom,
                          input$TotalFinishedSF)
    pred$pred_price <- pred_details$pred_price
    pred$pred_exp   <- pred_details$pred_exp

    #print(click)
    get_color <- function(sign){
      if(grepl("\\+",sign)){
        return("<font color = 'red'>")
      } else if(grepl("-",sign)){
        return("<font color = 'blue'>")
      }
    }
      get_color2 <- function(sign){
        if(sign == "+++"){
          return("<font color = '#ff803f'>")
        } else if(sign == "++"){
          return("<font color = '#ffb38c'>")
        } else if(sign == "+"){
          return("<font color = '#ffe5d8'>")
        } else if(sign == "---"){
          return("<font color = '#61abe9'>")
        } else if(sign == "--"){
          return("<font color = '#a0ccf1'>")
        } else if(sign == "-"){
          return("<font color = '#dfeefa'>")
        }
    }
    text <- paste("<b>Predicted Price:</b><h2>",
              scales::dollar(pred$pred_price),"</h2><br>",
              pred$pred_exp$feature[1],": <b>", 
              get_color2(pred$pred_exp$qualitativeStrength[1]), 
              pred$pred_exp$qualitativeStrength[1],"</font></b><br>",
              pred$pred_exp$feature[2],": <b>", 
              get_color2(pred$pred_exp$qualitativeStrength[2]),
              pred$pred_exp$qualitativeStrength[2],"</font></b><br>",
              pred$pred_exp$feature[3],": <b>", 
              get_color2(pred$pred_exp$qualitativeStrength[3]),
              pred$pred_exp$qualitativeStrength[3],"</font></b><br>"
              )
    leafletProxy(mapId = "map") %>%
      clearPopups() %>%
      addPopups(dat = click, lat = ~lat, lng = ~lng, popup = text)
  })

}

shinyApp(ui, server)


