# Contains the helper functions used within the main application.

# Creates an empty google map
getGoogleMap <- function() {
  return(google_map(key = Sys.getenv("GOOGLE")))
}

#getNatParks: Helper function to get the national park information for a state
# Inputs:
# 1.  `state`: State abbreviation (2 letters) for which to search for parks in

# Outputs:
# 1. `parkNames`: Data.frame containing the federal park information for parks in that state
# columns:
# `lat`:latitude
# `lng`:longitude
# `name`:full name of the park
# `code`: 4 letter code for the park, needed for querying the NPS API
# 2. `allParks`: full API response list
getNatParks <- function(state) {
  r <- GET(
    paste("https://developer.nps.gov/api/v1/parks?stateCode=", state, "&api_key=", Sys.getenv("NPS"), sep = "")
  )
  json <- content(r, as = "parsed")$data
  parkList <- data.frame(
    name = vector(length = length(json)),
    lat = vector(length = length(json)),
    lng = vector(length = length(json)),
    code = vector(length = length(json))
  )
  for (i in 1:length(json)) {
    parkList[i, "name"] <- json[[i]][["fullName"]]
    parkList[i, "lat"] <- json[[i]][["latitude"]]
    parkList[i, "lng"] <- json[[i]][["longitude"]]
    parkList[i, "code"] <- json[[i]][["parkCode"]]
  }
  return(list(parkNames = parkList, allParks = json))
}

#getCampgrounds: For a park, gets all the information on campgrounds there
#Inputs:
#1. parkCode: 4 letter park code corresponding to a NPS park

#Outputs
#1. campList: data.frame with necessary information about the campgrounds, empty data.frame() if no campgrounds
# columns: 
# `lat`:latitude
# `lng`:longitude
# `name`:name of the campground
# `url`: link to reservation webpage for the campground
# `text`: description of the campground
getCampgrounds <- function(parkCode) {
  r <- GET(
    paste("https://developer.nps.gov/api/v1/campgrounds?parkCode=", parkCode, "&api_key=", Sys.getenv("NPS"), sep = "")
  )
  json <- content(r, as = "parsed")$data
  if (length(json) == 0) { #i.e if no campgrounds exist
    return(data.frame())
  }
  campList <- data.frame(
    name = vector(length = length(json)),
    lat = vector(length = length(json)),
    lng = vector(length = length(json)),
    url = vector(length = length(json)),
    text = vector(length = length(json))
  )
  for (i in 1:length(json)) {
    campList[i, "name"] <- json[[i]][["name"]]
    campList[i, "lat"] <- json[[i]][["latitude"]]
    campList[i, "lng"] <- json[[i]][["longitude"]]
    campList[i, "url"] <- json[[i]][["reservationUrl"]]
    campList[i, "text"] <- json[[i]][["description"]]
  }
  return(campList)
}

#getPlaces: For a park, gets all the attractions there
#Inputs:
#1. parkCode: 4 letter park code corresponding to a NPS park

#Outputs
#1. placeList: data.frame with necessary information about the campgrounds, empty data.frame() if no campgrounds
# columns: 
# `lat`:latitude
# `lng`:longitude
# `name`:name of the attraction
# `open`: status of the attraction 1=open, 0=closed
# `text`: description of the attraction
getPlaces <- function(parkCode) {
  r <- GET(
    paste("https://developer.nps.gov/api/v1/places?parkCode=", parkCode, "&api_key=", Sys.getenv("NPS"), sep = "")
  )
  json <- content(r, as = "parsed")$data
  if (length(json) == 0) {
    return(data.frame())
  }
  placeList <- data.frame(
    name = vector(length = length(json)),
    lat = vector(length = length(json)),
    lng = vector(length = length(json)),
    open = vector(length = length(json)),
    text = vector(length = length(json))
  )
  for (i in 1:length(json)) {
    placeList[i, "name"] <- json[[i]][["title"]]
    placeList[i, "lat"] <- json[[i]][["latitude"]]
    placeList[i, "lng"] <- json[[i]][["longitude"]]
    placeList[i, "open"] <- json[[i]][["isOpenToPublic"]]
    placeList[i, "text"] <- json[[i]][["bodyText"]]
  }
  return(placeList)
}

#getMonthlyVisitors: For a park, gets information on the monthly visitor statistics
#Inputs:
#1. parkCode: 4 letter park code corresponding to a NPS park
#2. startYear: first year to look for information
#3. startMonth: start month for the query (1=Jan, 2=Feb, etc.)
#4. endYear: last year to query for
#5. endMonth: end month for the query (1=Jan, 2=Feb, etc.)

#Outputs
#1. visitors: data.frame with raw visitor counts per month, NULL if no data for the requested range
# columns: 
# `park`:name of the park
# `year`: year
# `month`: month (1=Jan, 2=Feb, etc.)
# `visitors`: total number of visitors for the month
#2. scaledVisitors: data.frame with raw visitor counts per month, counts are scaled by year, NULL if no data for the requested range
# columns: 
# `park`:name of the park
# `year`: year
# `month`: month (1=Jan, 2=Feb, etc.)
# `visitors`: scaled number of visitors for the month
getMonthlyVisitors <- function(parkCode, startYear = 1900, startMonth = 1, endYear = 2020, endMonth = 12) {
  r <- GET(paste("https://irmaservices.nps.gov/v3/rest/stats/visitation?unitCodes=",
    parkCode,
    "&startMonth=",
    startMonth,
    "&startYear=",
    startYear,
    "&endMonth=",
    endMonth,
    "&endYear=",
    endYear,
    sep = ""
  ))
  json <- content(r, as = "parsed")
  if (length(json) == 0) {
    return(list(scaledVisitors = NULL, visitors = NULL))
  }
  offset <- ifelse(json[[1]][["Month"]] == 1, 0, 13 - json[[1]][["Month"]])
  scaledVisitors <- data.frame()
  visitors <- data.frame()
  for (j in 1:((length(json) - offset) / 12)) {
    visitorList <- data.frame(
      park = vector(length = 12),
      year = vector(length = 12),
      month = vector(length = 12),
      visitors = vector(length = 12)
    )
    for (k in 1:12) {
      i <- k + (j - 1) * 12 + offset
      visitorList[k, "park"] <- json[[i]][["UnitName"]]
      visitorList[k, "year"] <- json[[i]][["Year"]]
      visitorList[k, "month"] <- json[[i]][["Month"]]
      visitorList[k, "visitors"] <- json[[i]][["RecreationVisitors"]]
    }
    visitors <- rbind(visitors, visitorList)
    visitorList <- visitorList %>% mutate(visitors = visitors %>% scale())
    scaledVisitors <- rbind(scaledVisitors, visitorList)
  }
  visitors <- visitors %>% mutate(`time` = year + month / 12)
  scaledVisitors <- scaledVisitors %>% mutate(monthNames = (month.name[month] %>% fct_relevel(levels = "December", "November", "October", "September", "August", "July", "June", "May", "April", "March", "February", "January")))
  return(list(scaledVisitors = scaledVisitors, visitors = visitors))
}

#getYelpInfo: For a park, gets all the information on campgrounds there. Assumes that the first result with be the correct one if one exists, so acts conservatively and may output NULL even if results exist.
#Inputs:
#1. parkName: Full name of the park
#2. lat: latitiude of park
#3. lng: longitude of park

#Outputs
#1. visitors: data.frame with the yelp information for the park, or NULL if the park is not found
# columns: 
# `id`: YELP ID for the park
# `review_count`: Number of YELP reviews for the park
# `rating`: YELP rating for the park
getYelpInfo <- function(parkName, lat, lng) {
  r <- GET(
    "https://api.yelp.com/v3/businesses/search",
    add_headers(Authorization = paste("Bearer", Sys.getenv("YELP_TOKEN"))),
    query = list(
      term = parkName,
      latitude = lat,
      longitude = lng
    )
  )
  json <- content(r, as = "text")
  park <- fromJSON(json)[[1]]
  if (length(park) == 0) {
    return(NULL)
  } else if ("parks" %in% (fromJSON(json)[[1]] %>% select(`categories`) %>% unlist())) {
    park <- park %>%
      select(`id`, `review_count`, `rating`) %>%
      slice_head(n = 1)
    return(park)
  } else {
    return(NULL)
  }
}

#getReviews: For a business, gets a the YELP reviews. YELP only provides up to 3 reviews
#Inputs:
#1. yelpID: YELP ID for the business

#Outputs
#1. reviews: data.frame
#columns:
# `text`: The review in written form
# `rating`: rating given by the reviewer
getReviews <- function(yelpID) {
  r <- GET(
    paste("https://api.yelp.com/v3/businesses/", yelpID, "/reviews", sep = ""),
    add_headers(Authorization = paste("Bearer", Sys.getenv("YELP_TOKEN")))
  )
  json <- content(r, as = "text")
  reviews <- fromJSON(json)[[1]]
  reviews <- reviews %>% select(`text`, `rating`)
  return(reviews)
}

#getPhotos: For a business, gets a the associated photos from YELP
#Inputs:
#1. yelpID: YELP ID for the business

#Outputs
#1. photos: vector of the URL's to the images
getPhotos <- function(yelpID) {
  r <- GET(
    paste("https://api.yelp.com/v3/businesses/", yelpID, sep = ""),
    add_headers(Authorization = paste("Bearer", Sys.getenv("YELP_TOKEN")))
  )
  json <- content(r, as = "text")
  photos <- fromJSON(json)
  return(photos[["photos"]])
}

#getWeather: For a loctation, gets the 3 day forecast
#Inputs:
#1. lat: latitude
#2. lng: longitude

#Outputs
#1. weather: data.frame
# columns
# `dt`: what time the weather is predicted or
# `uvi`: UV Index
# `temp`: Temperature
# `windSpeed`: Wind speed
# `pop`: Chance of precipitation
getWeather <- function(lat, lng) {
  r <- GET(paste("https://api.openweathermap.org/data/2.5/onecall?lat=",
    lat,
    "&lon=",
    lng,
    "&units=imperial&appid=",
    Sys.getenv("OPEN_WET"),
    sep = ""
  ))
  json <- content(r, as = "text")
  weather_data <- fromJSON(json)

  weather_data[["hourly"]]$dt <- anytime(weather_data[["hourly"]]$dt)


  time_points <- weather_data[["hourly"]]$dt
  weather <- weather_data[["hourly"]] %>%
    select(`dt`, `uvi`, `temp`, `wind_speed`, `pop`) %>%
    mutate(dt = anytime(dt),windSpeed=`wind_speed`) %>% select(-`wind_speed`)
  return(weather)
}

#getDicrections: Gets directions to a national park
#Inputs:
#1. origin: departure place, cannot have spaces (replace spaces with "+")
#2. lat: latitude
#3. lng: longitude
#4. parkName: Name of park (spaces ok)

#Outputs
#1. steps: vector of the step by step instructions
#2. summStats: vector of form (distance, travel time)
#3. poly: string for the polyline that will be used to create route on map
getDirections <- function(origin, lat, lng, parkName) {
  r <- GET(paste("https://maps.googleapis.com/maps/api/directions/json?origin=",
    origin,
    "&destination=",
    lat, "+", lng,
    "&key=",
    Sys.getenv("GOOGLE"),
    sep = ""
  ))
  json <- content(r)
  if (length(json[["routes"]]) == 0) { # in case coordinates don't work, sometimes they too far from roads
    r <- GET(paste("https://maps.googleapis.com/maps/api/directions/json?origin=",
      origin,
      "&destination=",
      parkName %>% str_replace_all("\\s", "+"),
      "&key=",
      Sys.getenv("GOOGLE"),
      sep = ""
    ))
    json <- content(r)
  }
  if (length(json[["routes"]]) == 0) {
    return(list(steps = NULL, summstats = NULL, poly = NULL))
  }
  summStats <- c(json[["routes"]][[1]][["legs"]][[1]][["distance"]][["text"]], json[["routes"]][[1]][["legs"]][[1]][["duration"]][["text"]])
  start <- json[["routes"]][[1]][["legs"]][[1]][["start_address"]]
  end <- json[["routes"]][[1]][["legs"]][[1]][["end_address"]]
  steps <- c()
  route <- json[["routes"]][[1]][["legs"]][[1]][["steps"]]
  for (i in 1:length(route)) { #take each step from the list and format in plane text
    step <- route[[i]][["html_instructions"]] %>%
      str_replace_all("<.{0,4}>", " ") %>%
      str_replace_all("<.*>", "\n") %>%
      str_replace_all("&nbsp;", " ") %>%
      str_split("\n") %>%
      unlist()
    steps <- c(steps, step)
  }
  poly <- json[["routes"]][[1]][["overview_polyline"]][["points"]]


  return(list(steps = steps, summStats = summStats, poly = poly))
}
