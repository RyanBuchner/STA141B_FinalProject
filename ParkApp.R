library(shiny)
library(httr)
library(tidyverse)
library(jsonlite)
library(anytime)
library(shinydashboard)
library(googleway)
library(shinyjs)
library(leaflet)
library(plotly)
library(ggridges)

source("helperFunctions.R") # load helper functions

ui <-
  dashboardPage(
    dashboardHeader(title = "National Park Trip Planner"),
    dashboardSidebar(
      collapsed = TRUE,
      sidebarMenu(
        menuItem("Search", tabName = "info", icon = icon("address-card")),
        menuItem("Directions", tabName = "directions", icon = icon("directions")),
        menuItem("Weather Forecast", tabName = "weather", icon = icon("cloud-meatball")),
        menuItem("Photos", tabName = "pics", icon = icon("camera-retro")),
        menuItem("Reviews", tabName = "reviews", icon = icon("comment")),
        menuItem("Visitor Logs", tabName = "visitor", icon = icon("chart-line")),
        menuItem("Camping and Attraction Info", tabName = "campsites", icon = icon("campground"))
      )
    ),
    dashboardBody(
      useShinyjs(),
      tabItems(
        tabItem(
          tabName = "info",
          box(p("Your guide to national parks. Select a park 
                      from the list to get directions, reviews, and more. Check out the
                      weather tab to help prepare for your trip."), width = 12),
          box(
            h2("Enter State of Interest"),
            selectInput("state", "State", selected = "CA", choices = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")),
            actionButton("submit", "Find Parks", icon = icon("tree")),
            conditionalPanel(
              condition = " input.submit!=0",
              selectInput("parkList", "Choose A Park", choices = c("-")),
              actionButton("submit2", "Select Park", icon = icon("leaf")),
            )
          ),
          leafletOutput("locmap")
        ),

        tabItem(
          tabName = "directions",
          box(
            h2("Enter you departure city"),
            textInput("city", "City"),
            selectInput("homeState", "State", selected = "CA", choices = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")),
            actionButton("getDirs", "Get Directions", icon = icon("redo"))
          ),
          box(htmlOutput("htmlstep"), solidHeader = TRUE),
          box(htmlOutput("routeSum"), solidHeader = TRUE),
          box(
            google_mapOutput("myMap")
          )
        ),

        tabItem(
          tabName = "reviews",
          box(htmlOutput("rev_string"), width = 12),
        ),

        tabItem(
          tabName = "pics",
          box(htmlOutput("photosPage"), width = 12),
        ),

        tabItem(
          tabName = "weather",
          box(h2("Weater 3-Day forecast"),
            p("powered by Open Weather"),
            p("Wind advisories are issued at speeds above 30 mph.                
                      UV Index of above 5 is considered high risk, but regardless of UV Index
                      sunscreen should always be worn outside on exposed body parts"),
            width = "100%"
          ),
          box(
            radioButtons("weather_choice", "What information would you like to view?", choiceValues = c("wind", "rain", "uvi", "temp"), choiceNames = c("Wind", "Rain", "UV Index", "Temperature"), c()),
            sliderInput("maxData", "Maximum weather forecast (hours)", 1, 72, 72, step = 1)
          ),
          box(plotOutput("weather_plot"))
        ),

        tabItem(
          tabName = "visitor",
          box(
            width = "100%",
            h2("Visitor Information"),
            p("Traveling during off season can help beat the crowds."),
            radioButtons("monthYear", "Would you like the monthly values or just the yearly trends?", choiceValues = c("year","month"), choiceNames = c("Yearly","Monthly")),
            sliderInput(
              inputId = "visitorYearRange",
              label = "What is the oldest data you would like to include?",
              min = 1900, # have to update...#set errr message (data not available)
              max = 2010,
              value = 1900,
              step = 1
            ),
            htmlOutput("visitorError"),
          ),
          box(
            plotOutput("visitorMonthly")
          ),
          box(
            plotlyOutput("visitorLine")
          )
        ),
        tabItem(
          tabName = "campsites",
          box(
            selectizeInput(
              inputId = "placeInterest",
              label = "Which attraction would you like to learn more about?",
              choices = c("-")
            ),
            htmlOutput("placeDesc")
          ),
          box(
            selectizeInput(
              inputId = "campInterest",
              label = "Which campground would you like to learn more about?",
              choices = c("-")
            ),
            htmlOutput("campDesc")
          ),
          box(leafletOutput("campMap"))
        )
      )
    )
  )

server <- function(input, output, session) {
  shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")

  # this solution courtesy of https://stackoverflow.com/questions/35170065/hide-sidebar-in-default-in-shinydashboard
  observeEvent(input$submit2, {
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  })

  observeEvent(input$state, {
    updateSelectInput(session, "homeState", selected = input$state)
  })

  prevState <- ""
  prevParks <- data.frame()

  # updates the list of park options upon the user choosing a state, passes on API response as a RV to prevent double calling
  parkChoice <- eventReactive(input$submit, {
    state <- input$state
    if (state != prevState) {
      parks <- getNatParks(state)
      updateSelectInput(session, "parkList", choices = parks[["parkNames"]] %>% select("name"))
      updateSelectInput(session, "secondState", selected = input$state)
      prevParks <<- parks
      return(parks)
    } else {
      return(prevParks)
    }
  })

  # Main data loop, gets all the needed information when a user chooses a park
  reaction <- eventReactive(input$submit2, {
    parks <- parkChoice()
    park <- parks[["parkNames"]] %>% filter(`name` == input$parkList)
    lat <- park %>% select(`lat`)
    lng <- park %>% select(`lng`)
    parkCode <- park %>% select(`code`)

    campsites <- getCampgrounds(parkCode)
    places <- getPlaces(parkCode)
    if (length(campsites) == 0) {
      updateSelectInput(session, "campInterest", choices = c("-"))
    } else {
      updateSelectInput(session, "campInterest", choices = campsites %>% select(`name`))
    }
    if (length(places) == 0) {
      updateSelectInput(session, "placeInterest", choices = c("-"))
    } else {
      updateSelectInput(session, "placeInterest", choices = places %>% select(`name`))
    }

    weather <- getWeather(lat, lng)

    photos <- c()
    images <- parks[["allParks"]][[which((parks[["parkNames"]] %>% select(`name`)) == input$parkList)]][["images"]]
    for (i in 1:length(images)) {
      photos <- c(photos, images[[i]][["url"]])
    }

    currPark <- getYelpInfo(input$parkList, lat, lng)
    if (!(is.null(currPark))) {
      parkId <- currPark %>% select(`id`)
      rating <- c(currPark %>% select(`rating`), currPark %>% select(`review_count`))
      reviews <- getReviews(parkId)
      yelpPhotos <- getPhotos(parkId)
      photos <- c(photos, yelpPhotos)
    } else {
      reviews <- NULL
      rating <- NULL
    }

    visitData <- getMonthlyVisitors(parkCode)
    visitors <- visitData[["visitors"]]
    scaledVisitors <- visitData[["scaledVisitors"]]
    if (!(is.null(visitors))) {
      updateSliderInput(session, "visitorYearRange", min = visitors %>% select(`year`) %>% min())
    }

    return(list(
      reviews = reviews,
      photos = photos,
      rating = rating,
      weather = weather,
      campsites = campsites,
      places = places,
      visitors = visitors,
      scaledVisitors = scaledVisitors
    ))
  })

  # creates map with national parks for the state on it
  output$locmap <- renderLeaflet({
    leaflet(data = parkChoice()[["parkNames"]]) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addMarkers(~ as.numeric(lng), ~ as.numeric(lat), popup = ~name, label = ~name)
  })

  # creates HTML for the description of the place of choice
  output$placeDesc <- renderUI({
    places <- reaction()$places
    choice <- input$placeInterest
    if (choice == "-") {
      return(p("Sorry no attractions in this park, but still plently of memories to be made."))
    } else {
      place <- places %>% filter(`name` == choice)
      out <- list(
        h2(place %>% select(`name`)),
        br(),
        gsub("<.*?>", "", place %>% select(`text`)),
        br(),
        br(),
        ifelse(((place %>% select(`open`)) == 1), "Currently open", "Currently closed.") %>% p()
      )
      return(tags$div(out))
    }
  })

  # creates HTML for the description of the campsite of choice
  output$campDesc <- renderUI({
    sites <- reaction()$campsites
    choice <- input$campInterest
    if (choice == "-") {
      return(p("Sorry no camping in the park."))
    } else {
      site <- sites %>% filter(`name` == choice)
      if (site %>% select(`url`)==""){
        out <- list(
          h2(site %>% select(`name`)),
          br(),
          p(site %>% select(`text`))
        )
      } else {
        out <- list(
          h2(site %>% select(`name`)),
          br(),
          p(site %>% select(`text`)),
          br(),
          tags$a(href = site %>% select(`url`), "Reservation Link")
        )
      }
      return(tags$div(out))
    }
  })

  # creates a map with campsites and attractions on it
  output$campMap <- renderLeaflet({
    map <- leaflet(data) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addLegend("bottomright",
        labels = c("Attraction", "Campground"),
        colors = c("green", "red"),
        opacity = 1
      )

    place <- reaction()[["places"]]
    if (length(place) != 0) {
      map <- map %>% addCircleMarkers(data = place, ~ as.numeric(`lng`), ~ as.numeric(`lat`), color = "green", popup = ~name, opacity = .9)
    }

    camp <- reaction()[["campsites"]]
    if (length(camp) != 0) {
      map <- map %>% addCircleMarkers(data = camp, ~ as.numeric(`lng`), ~ as.numeric(`lat`), color = "red", popup = ~name, opacity = .9)
    }
    return(map)
  })

  # When the user asks for directions, updates the Google Map and creates the summary statistics and step by step directions
  directions <- eventReactive(input$getDirs, {
    park <- parkChoice()[["parkNames"]] %>% filter(`name` == input$parkList)
    origin <- paste(input$city, input$homeState, sep = "+") %>% str_replace_all("\\s", "+")
    lat <- park %>% select(`lat`)
    lng <- park %>% select(`lng`)
    directions <- getDirections(origin, lat, lng, park %>% select(`name`))
    polyline <- directions$poly
    if (!(is.null(polyline))) {
      df_route <- data.frame(route = polyline)
      if (input$submit2 == 1) {
        google_map_update(map_id = "myMap") %>%
          add_polylines(data = df_route, polyline = "route")
      } else {
        google_map_update(map_id = "myMap") %>%
          clear_polylines() %>%
          add_polylines(data = df_route, polyline = "route")
      }
      return(list(steps = directions$steps, summStats = directions$summStats))
    }
  })

  # Render the Route Summary HTML
  output$routeSum <- renderUI({
    summStats <- directions()$summStats
    if (!(is.null(summStats))) {
      out <- list(
        h2("Route Overview:"),
        br(),
        p(paste("Distance:", summStats[1])),
        br(),
        p(paste("Travel Time:", summStats[2]))
      )
      return(tags$div(out))
    } else {
      return(p("No directions available"))
    }
  })

  # Render the error if visitor data not present for park
  output$visitorError <- renderUI({
    if (is.null(reaction()$visitors)) {
      return(p(strong("No data available for this park")))
    }
  })

  # Creates empty Google Map
  output$myMap <- renderGoogle_map({
    return(getGoogleMap())
  })

  # Generate the HTML for the reviews page
  output$rev_string <- renderUI({
    reviews <- reaction()$reviews
    rating <- reaction()$rating
    if (!(is.null(reviews))) {
      if (length(reviews$text) == 0) {
        return(tags$div(h2("Sorry, no reviews for this park")))
      }
      out <- vector("list", 15)
      out[[1]] <- h2("Here's what the community had to say about the park:")
      out[[2]] <- img(src = paste(rating[1], "_5.png", sep = ""))
      out[[3]] <- tags$b(paste("Based on ", rating[2], " ratings", sep = ""))
      out[[4]] <- br()
      out[[5]] <- br()
      for (i in 1:length(reviews$text)) {
        out[[(3 + i * 3)]] <- img(src = paste(reviews$rating[i], ".png", sep = ""))
        out[[4 + (i * 3)]] <- p(reviews$text[i])
        out[[5 + (i * 3)]] <- br()
      }
      out[[15]] <- p("powered by YELP")
      return(tags$div(out))
    } else {
      return(tags$div(h2("Sorry, no reviews for this park")))
    }
  })

  # generates the HTML for the photos page
  output$photosPage <- renderUI({
    photos <- reaction()$photos
    if (!(is.null(photos))) {
      if (length(photos) == 0) {
        return(tags$div(h2("Sorry no photos available for this park")))
      }
      out <- vector("list", (3 + length(photos)))
      out[[1]] <- h2("Photo Gallery")
      out[[2]] <- p("images provided by NPS and YELP")
      out[[3]] <- br()
      for (i in 1:length(photos)) {
        out[[(2 + i * 2)]] <- img(src = photos[i], height = 400)
        out[[3 + (i * 2)]] <- br()
      }
      return(tags$div(out))
    }
  })

  # Generates the HTML for the Directions
  output$htmlstep <- renderUI({
    steps <- directions()$steps
    park_name <- reaction()$park_name
    start <- reaction()$start
    if (!(is.null(steps))) {
      step <- vector("list", (length(steps) + 1))
      step[[1]] <- h2(paste("Directions to ", park_name, ":", sep = ""), br())
      for (i in 1:(length(steps))) {
        step[[1 + i]] <- p(paste(i, ". ", steps[i], sep = ""), br())
      }
      return(tags$div(step))
    }
  })

  # Generates the Plot for the montly visitors
  output$visitorMonthly <- renderPlot({
    scaledVisitors <- reaction()$scaledVisitors
    if (is.null(scaledVisitors)) {
      return()
    } else {
      scaledVisitors <- scaledVisitors %>% filter(`year` > input$visitorYearRange)
      plot <- ggplot(scaledVisitors, aes(x = `visitors`, y = `monthNames`, group = `month`)) +
        stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
        xlab("Monthly Vistors Adjusted for Yearly Trends") +
        ylab("Month") +
        ggtitle("Visitors By Month")
      return(plot)
    }
  })

  # Generates the plot with line for visitors over time
  output$visitorLine <- renderPlotly({
    visitors <- reaction()$visitors
    if (is.null(visitors)) {
      return()
    } else {
      visitors <- visitors %>% filter(`year` > input$visitorYearRange)
      if (input$monthYear == "year") {
        year <- input$year
        numParks <- input$numPark
        visitors <- visitors %>%
          group_by(`park`, `year`) %>%
          summarize(yearlyVisits = `visitors` %>% sum())
        plot <- (ggplot(visitors, aes(x = `year`, y = `yearlyVisits`)) +
          geom_line() +
          xlab("Year") +
          ylab("Yearly Visitors") +
          ggtitle("Trends of Park Visits")
        ) %>%
          ggplotly()
      } else {
        plot <- (ggplot(visitors, aes(x = `time`, y = `visitors`)) +
          geom_line() +
          xlab("Year") +
          ylab("Monthly Visitors") +
          ggtitle("Trends of Park Visits")
        ) %>%
          ggplotly()
      }
      return(plot)
    }
  })

  # Creates the weather plot corresponding to user request
  output$weather_plot <- renderPlot({
    weather <- reaction()$weather
    currtime <- weather %>%
      select(`dt`) %>%
      slice_head(n = 1)
    weather <- weather %>% filter(`dt` < (currtime + 3600 * input$maxData))
    wc <- input$weather_choice
    if (length(weather) != 1) {
      if (wc == "temp") {
        return(ggplot(weather, aes(x = `dt`, y = `temp`, fill = `temp`, color = `temp`)) +
          geom_bar(stat = "identity") +
          geom_abline(slope = 0, intercept = 32, color = "red") +
          xlab("") +
          ylab("Temperature (F)") +
          theme(legend.position = "none", panel.background = element_rect(fill = "white", colour = NA)) +
          scale_color_gradient2(midpoint = 65, low = "blue", mid = "gray", high = "red") +
          scale_fill_gradient2(midpoint = 65, low = "blue", mid = "gray", high = "red"))
      } else if (wc == "wind") {
        return(ggplot(weather, aes(x = `dt`, y = `windSpeed`, fill = `windSpeed`, color = `windSpeed`)) +
          geom_bar(stat = "identity") +
          xlab("") +
          ylab("Wind Speed (mph)") +
          theme(legend.position = "none", panel.background = element_rect(fill = "white", colour = NA)) +
          scale_fill_gradient2(midpoint = 15, low = "blue", mid = "yellow", high = "red") +
          scale_color_gradient2(midpoint = 15, low = "blue", mid = "yellow", high = "red"))
      } else if (wc == "uvi") {
        return(ggplot(weather, aes(x = `dt`, y = `uvi`, fill = `uvi`, color = `uvi`)) +
          geom_bar(stat = "identity") +
          xlab("") +
          ylab("UV Index") +
          theme(legend.position = "none", panel.background = element_rect(fill = "white", colour = NA)) +
          scale_fill_gradient2(midpoint = 3, low = "black", mid = "yellow", high = "red") +
          scale_color_gradient2(midpoint = 3, low = "black", mid = "yellow", high = "red"))
      } else if (wc == "rain") {
        return(ggplot(weather, aes(x = `dt`, y = `pop`, color = `pop`, fill = `pop`)) +
          geom_bar(stat = "identity") +
          xlab("") +
          ylab("Chance of Precipitation") +
          ylim(0, 1) +
          theme(legend.position = "none", panel.background = element_rect(fill = "white", colour = NA)) +
          scale_fill_gradient2(midpoint = .3, low = "black", mid = "grey", high = "blue") +
          scale_color_gradient2(midpoint = .3, low = "black", mid = "grey", high = "blue"))
      }
    }
  })
}


shinyApp(ui, server)
