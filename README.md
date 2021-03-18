# STA141B_FinalProject

## [National Park Trip Planner](https://bababuck.shinyapps.io/National_Park_Helper/)

This app works to provide a nice user interface to help plan a trip to a federal park (technically not just national parks). It contains several tabs to guide the user.

#### Important Information for Grader

When using the App, there are times where "warnings" may appear to the viewer. Generally they are causes by the API not having the data requested, and are handled by the code. The warning just appear for the users convinience. Example errors:  
-"directions not found": Google Directions API did not find directions, generally caused when trying to travel across an ocean or road closures.
-"Sorry no attractions in this park, but still plently of memories to be made.": The NPS API has no listed attractions for the park. Generally occurs in smaller, less popular parks.
-"Sorry no camping in the park.": Some parks do not have an campgrounds (i.e Alcatraz)
-"No data available for this park": NPS IRMA does not have montly visitor statistics for some parks available. Generally occurs for smaller parks.
-"Sorry, no reviews for this park": YELP API, does not have reviews for this park. Generally occurs for some very small parks.

#### Search  

There is a two part search scheme present, first the user selects a state and then they can choose a park from the parks in that state. The list of the parks per state is provided through the NPS API.

#### Directions  

In the directions tab, the user can ask for directions to the national park selected in the search tab. It asks for a city to depart from as well as a state. The departure state automatically updates to be the same as the state chosen in the search tab although it can be changed manually. The directions provided include a step-by-step list, total distance and time, as well as a map with the route shown on it. These features were implemented using the Google Directions API and Google Maps Javascript API (in conjunction with the googleway package needed for viewing the map in R)

#### Weather Forecast

The user can look at the weather for the forecast to help them plan a trip. The 3 day forecast is provided visually in a graph form, although the user can lower the limit to less than 72 hours. The user can request to view either UV Index, Temperature, Wind Speed, or Precipitation.

#### Photos

This tab provides a photo gallery of the park. The photos are combined to show those both obtained from the YELP API as well as the NPS API.

#### Reviews

Powered by the YELP API, this tab provides a total score for the park as well as a few individual reviews. Unfortunately, the YELP API on provides 3 reviews as max, and sometimes cuts off the reviews.

#### Visitor Logs  

This trip planning tool allows the user to view the visitor statistics month by month. Since raw numbers are hard to interpret and change year to year, I plotted is via a density plot so the user to get a more holistic picture of the month by month visitation statistics and the variances associated with them. In addition, the viewer can view the historical visitor data either in month by month format or yearly. The yearly provides a much more interpretable, smoother graph and is recommended. The user can also choose a time range from which to view the data in case they prefer only more recent data. The data is obtained via the NPS IRMA API.

#### Camping and Attraction Information

This tab provides a place to research campgrounds and things to do at the park. The data and text is all from the NPS API, and it provides details about each attraction and campground in the park, as well as a map with the locations plotted.