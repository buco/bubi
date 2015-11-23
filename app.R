# MOL Bubi app
library(shiny)
library(httr)
library(leaflet)

# Define UI for application that plots random distributions 
ui<-fluidPage(
  tags$head(includeScript("google.js")),
  br(),div(leafletOutput("Plot")),br(),"Refreshing in every 10 minute.",br(),"Free slots: Number of free slots",br(),"Free bikes: Number of bikes in the station",br(),br(),br(),
  img(src="http://harrywood.co.uk/maps/examples/leaflet/marker-icon-red.png")," less then 5 free bikes",br(),
  img(src="http://www.fosterly.com/assets/maps/marker-3a77f9ac6522a026170f1aed325f2d19.png")," less then 10 free bikes",br(),
  img(src="https://camo.githubusercontent.com/256954f7aeac24805575508a3427a4956b5fb5b7/68747470733a2f2f7261772e6769746875622e636f6d2f706f696e7468692f6c6561666c65742d636f6c6f722d6d61726b6572732f6d61737465722f696d672f6d61726b65722d69636f6e2d677265656e2e706e673f7261773d74727565")," more then 10 free bikes",br(),
  br(),
  dataTableOutput("szoveg"),br(),
  "source: CityBikes, http://api.citybik.es/"
)

server<-function(input, output,session) {
  
  bikes<-reactive({ 
    invalidateLater(600000, session)
    bike<-content(GET("http://api.citybik.es/bubi.json"))
    coord<-unlist(lapply(bike,function(x) paste((x$lat/10^6),(x$lng/10^6),sep=",")))
    
    bikes<-as.data.frame(do.call(rbind,(strsplit(coord,","))))
    colnames(bikes)<-c("lat","long")
    bikes$lat<-as.numeric(as.character(bikes$lat))
    bikes$long<-as.numeric(as.character(bikes$long))
    bikes$bikes<-unlist(lapply(bike,function(x) x$bikes))
    bikes$timestamp<-unlist(lapply(bike,function(x) x$timestamp))
    bikes$timestamp<-as.POSIXct(bikes$timestamp, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")+3600
    bikes$name<-unlist(lapply(bike,function(x) x$name))
    bikes$free<-unlist(lapply(bike,function(x) x$free))
    if (file.exists('bikes.csv')==FALSE)
      write.csv(bikes, file='bikes.csv', row.names=F)
    
    stack <- read.csv(file='bikes.csv')
    stack <- rbind(stack, bikes)
    write.csv(stack, file='bikes.csv', row.names=F)
    
    
    
    return(bikes)
  })
  
  output$szoveg<-renderDataTable({
    bikes2<-bikes()[,c("name","free","bikes")]
    colnames(bikes2)<-c("Name","Free slots","Free bikes")
    bikes2
  })
  
  output$Plot <- renderLeaflet({
    leafIcons <- icons(
      iconUrl = ifelse(bikes()$bikes < 5,
                       "http://harrywood.co.uk/maps/examples/leaflet/marker-icon-red.png",
                       ifelse(bikes()$bikes<10, 
                              "http://www.fosterly.com/assets/maps/marker-3a77f9ac6522a026170f1aed325f2d19.png",
                              "https://camo.githubusercontent.com/256954f7aeac24805575508a3427a4956b5fb5b7/68747470733a2f2f7261772e6769746875622e636f6d2f706f696e7468692f6c6561666c65742d636f6c6f722d6d61726b6572732f6d61737465722f696d672f6d61726b65722d69636f6e2d677265656e2e706e673f7261773d74727565"
                       )),
      
      iconWidth = 25, iconHeight = 41,
      iconAnchorX = 22, iconAnchorY = 94
    )
    leaflet(data = bikes()) %>% addTiles() %>% addMarkers(~long, ~lat, popup = ~as.character(paste(name,":<br/>","Free bikes: ",bikes,"<br>Free slots: ",free,sep="")), icon=leafIcons)
  })
}
shinyApp(ui = ui, server = server)
