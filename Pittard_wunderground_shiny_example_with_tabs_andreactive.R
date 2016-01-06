# Steve Pittard ticopittard@gmail.com Shiny demo that allows a user to specify a location in the form of City, State
# (or latitude/longitude) and a radius in miles. Once submitted the app will 
# 
# 1) use the API service of Wunderground Weather to locate Personal Weather stations located 
# within that specified radius.
#
# 2) From that list the app will extract the station name, it's location (usually the 
# neighborhood name), and basic weather information. 
#
# 3) Next we will build a map using the googleVis library and display it


# Note that the UI and Server are contained in this single file so all one needs to do to execute it
# load it in R Studio and then Run it. Note that it uses a key assigned to me (Steve Pittard) so if you
# run this alot it will generate nasty messages to my email for "over using" the API since this example
# uses the free tier of their service. You can sing up for your own key at 
# http://www.wunderground.com/weather/api/

library(shiny)

# Write the UI

ui <- fluidPage(
  
  # Application title
  titlePanel("Wunderground Personal Weather Station Browser"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      textInput("City","City",value="Boston",width=200),
      textInput("State","State",value="MA",width=200),
      helpText("Enter a City/State pair such as Boston,MA or Houston,TX"),
      sliderInput("dist","Radius", min = 1, max = 50, value = 2),   
      helpText("Find Personal Weather Stations within the specified radius in miles"),
      submitButton("Submit")
    ),
    
    
    # Show a plot of the generated distribution
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map",tableOutput("info")),
        tabPanel("Table",tableOutput("table"))
      )
    )
    
  )
)

# Now for the Server - We will display a map of Personal Weather Stations courtesy of GoogleVis

server <- function(input,output, session) {
  library(googleVis)
  
  #
  # Set up a reactive function that allows us to fetch weather stats for each 
  #  

  datum <- reactive({
    locstring <- paste("Weather Station Info",paste(input$City,input$State,sep=","),sep=" : ")
    withProgress(message=locstring,value=0, { 
      library(XML)
      library(RCurl)
      
      # The base url is given to us by the Wunderground API. Note that you should get your
      # very own key and in the line below put it in place of 2686f8dd293197c4
      
      base_url <- "http://api.wunderground.com/api/2686f8dd293197c4/geolookup/q/"
      city <- input$City
      city <- gsub(" ","%20",city)
      
      query_url <- paste(base_url,input$State,city,sep="/")
      query_url <- paste(query_url,"xml",sep=".")
      
      wunderQuery <- getURL(query_url)
      parsedoc <- xmlParse(wunderQuery,useInternalNodes=TRUE)
      type <- getNodeSet(parsedoc,"//location/type")
      type <- xmlSApply(type,xmlValue)
      
      # Here we check to see if the user supplied a non US locale such as Paris, FR
      # in which case we need to parse the XML a little differently
      
      if (type == "INTLCITY") {
        country <- getNodeSet(parsedoc,"//location/country_name")
        country <- xmlSApply(country,xmlValue)
        city <- getNodeSet(parsedoc,"//location/city")
        city <- xmlSApply(city,xmlValue)
        locstring <- paste(city,country,sep=" ")
      } else {
        country <- "US"
        state <- getNodeSet(parsedoc,"//location/state")
        state <- xmlSApply(state,xmlValue)
        city <- getNodeSet(parsedoc,"//location/city")
        city <- xmlSApply(city,xmlValue)
        locstring <- paste(city,state,country,sep=" ")
      }
      
      # Could use Xslst to parse out everything at once with a stylesheet but this
      # approach will work
      
      stations <- getNodeSet(parsedoc,"//location/nearby_weather_stations/pws/station/id")
      statlist <- xmlSApply(stations,xmlValue)
      neighborhood <- getNodeSet(parsedoc,"//location/nearby_weather_stations/pws/station/neighborhood")
      neighborhood <- xmlSApply(neighborhood,xmlValue)
      distance <- getNodeSet(parsedoc,"//location/nearby_weather_stations/pws/station/distance_mi")
      distlist <- as.numeric(xmlSApply(distance,xmlValue))
      
      statdf <- data.frame(station=statlist,neighborhood=neighborhood,
                           distance_mi=distlist,stringsAsFactors=FALSE)
      statdf <- statdf[statdf$distance_mi <= input$dist,]
      # statdf
      
      #
      # Now we query Wunderground to get the report associated with each station
      #
      
      stations <- statdf$station
      base_string <- "http://api.wunderground.com/api/2686f8dd293197c4/forecast/geolookup/conditions/q/pws:" 
      masterlist <- list()
      retlist <- list()
      jj <- 1
      len <- length(stations)
      
      for (ii in 1:len) {
        retlist$name <- stations[ii]
        query_string <- paste(base_string,stations[ii],sep="")
        query_string <- paste(query_string,".xml",sep="")
        print(query_string)
        
        # Do processing of each station
        
        wunderQuery <- getURL(query_string)
        parsedoc <- xmlParse(wunderQuery,useInternalNodes=TRUE)
        
        # Get Lat and Lon
        
        lat <- getNodeSet(parsedoc,"//response/location/lat")
        lat <- xmlSApply(lat,xmlValue)
        
        lon <- getNodeSet(parsedoc,"//response/location/lon")
        lon <- xmlSApply(lon,xmlValue)
        
        retlist$latlon <- paste(lat,lon,sep=":")
        
        # Get weather string
        
        weather <- getNodeSet(parsedoc,"//response/current_observation/weather")
        weather <- xmlSApply(weather,xmlValue)
        retlist$weather <- weather
        
        # Get temp_f
        
        temp_f <- getNodeSet(parsedoc,"//response/current_observation/temp_f")
        temp_f <- xmlSApply(temp_f,xmlValue)
        retlist$temp_f <- temp_f
        
        obsloc <- getNodeSet(parsedoc,"//response/current_observation/observation_location/city")
        obsloc <- xmlSApply(obsloc,xmlValue)
        retlist$obsloc <- unlist(strsplit(obsloc,","))[1]
        
        feelslike_f <- getNodeSet(parsedoc,"//response/current_observation/feelslike_f")
        feelslike_f <- xmlSApply(feelslike_f,xmlValue)
        retlist$feelslike_f <- feelslike_f
        
        precip_daily_in <- getNodeSet(parsedoc,"//response/current_observation/precip_today_in")
        precip_daily_in <- xmlSApply(precip_daily_in,xmlValue)
        retlist$precip_daily_in <- precip_daily_in
        
        #      wind_string <- getNodeSet(parsedoc,"//response/current_observation/wind_string")
        #      wind_string <- xmlSApply(wind_string,xmlValue)
        #      retlist$wind_string <- wind_string
        
        #      wind_mph <- getNodeSet(parsedoc,"//response/current_observation/wind_mph")
        #      wind_mph <- xmlSApply(wind_mph,xmlValue)
        #      retlist$wind_mph <- wind_mph
        
        masterlist[[jj]] <- retlist
        jj = jj + 1
        retlist <- list()
        
        incProgress(1/len,detail=paste("Fetching Info for Station",stations[ii]))
        Sys.sleep(0.1)
        
      }
      
    })
    retdf <- data.frame(do.call(rbind,masterlist))
    retdf$tip <- paste(paste("STATION",retdf$name,sep=" : "),
                       paste("WEATHER",retdf$weather,sep=" : "),
                       paste("TEMP",retdf$temp_f,sep=" : "),
                       paste("LOCATION",retdf$obsloc,sep=" : "),
                       paste("FEELS_LIKE",retdf$feelslike_f,sep=" : "),
                       paste("PRECIPITATION",retdf$precip_daily_in,sep=" : "),
                       sep="<BR>")
    retdf
     
  })
  
  
  
  ## 
  output$info <- renderGvis({
    datum <- datum()
    gvisMap(datum,locationvar="latlon",tipvar="tip")
    
  })
  
  output$table <- renderTable({
   datum <- datum()
   datum[,c(-2,-8)]   # Don't display the Tip and latlon columns since they make sense only
                      # to the gvisMap function
  })
  
}

shinyApp(ui, server)  # This cranks up the server 