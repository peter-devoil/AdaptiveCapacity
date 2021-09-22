#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(raster)
library(rgdal)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
#devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
library(ggradar)

#setwd("~/work/Projects/Adaptive Capacity/Repack-2021/ac")
setwd("~/srv/shiny-server/AdaptiveCapacity")

df <- read.csv("SmoothedDataNorm.csv")

helpText<- list()
helpText[['opedu']] <- "Ordinal variable with the following values: (1) primary school completed or attended; (2) 1 - 4 years high school completed; (3) 5 - 6 years high school completed; (4) trade apprenticeship or technical completed; and (5) university or other tertiary completed.";
helpText[['spedu']] <- "Same as above but for the operator's spouse." ;
helpText[['health']]<- "Self assessed health status is an ordinal variable ranging from 0 (poor health) to 4 (excellent health) obtained from the ABS health survey.";
helpText[['landcare']]<-"Indicator of whether the farmer is a member of a Landcare group.";
helpText[['partners']]<-"Number of partners running the business.";
helpText[['internet']]<-"Indicator representing whether or not the farm has access to the internet.";
helpText[['pgi']]<-"A measure of pasture productivity from the Aussie GRASS national forecasting system (Carter, 2000), which is based on the GRASP model of Ricket et al. (2000). Pasture growth is modelled using a large number of biophysical variables that include rainfall, plant phenology, evaporation, soil type, and farm management. For each farm an average value of the pasture growth index is taken for the period of time it remained in sample during the decade ending 2005-06.";
helpText[['dams']]<-"Number of dams on the farm. This measures the potential of the farm to access stored water. Dams was last collected in the 2001 AAGIS so it was necessary to impute this variable to other farms using spatial kernel smoothing techniques (Cowling et al., 1996)";
helpText[['veg_potential']]<- "This is measured as a categorical variable ranging from 1 to 6 where 1 is for farms in regions with the greatest extent of remanent native vegetation (90-100%), and 6 is for those farms in regions with the least extent of remanent native vegetation (0-10%) (((((Rohan, I need your help defining this one))))))";
helpText[['plantmachine']]<- "This is a value-weighted Fisher index of the quantity of plant and machinery on the farm relative to other farms. These quantities are derived by dividing the value of each item by a price index. Note that this index measures the quantity of these inputs used relative to the same or other farms at different points in time.";
helpText[['structure']]<- "Defined similarly to the plant and machinery index above, except it is for fixed structures on the farm such as buildings, fences and dams.";        
helpText[['livestock']]<-"Defined similar to the plant and machinery index above, except it is for average numbers of cattle, sheep and other livestock held during the year.";
helpText[['capital']]<-"This is the total capital value of the farm including the value leased land and of the operator�s house, but excluding the value of land leased out. Note that this is an absolute measure of the value of capital and hence differs from the quantity indexes used for physical capital.";
helpText[['mtci']]<-"Total cash income is defined as the sum of farm cash income and off farm income. It is the amount of income households have to meet living and other expenses. For each farm an average value of total cash income is taken for the period of time it remains in sample during the decade ending 2005-06.";
helpText[['access']]<-"Access to credit plus liquid assets. This metric has been constructed by summing borrowing capacity and liquid assets. Borrowing capacity has been derived according to each farms� equity ratio. Where the equity ratio is less than 70%, borrowing capacity is zero, otherwise borrowing capacity = (equity ratio - 0.70) � capital (see above). Although it is acknowledged that corporate farms are likely to have different financing facilities, it was not possible model these effects separately and therefore all farms had these finance data constructed in the same manner. ";
helpText[['rain_cv']]<-"In constructing this variable total annual rainfall for each year from 1996-97 to 2005-06 was used. The coefficient of variation of these values was determined at each point on a regular grid across Australia. This coefficient is defined as the 90th percentile minus the 10th percentile divided by the 50th percentile of annual rainfall.";
helpText[['pgi_cv']]<-"Defined as above except the variable used is annual average pasture growth index for the same time period as for rainfall.";
helpText[['fcr_cv']]<-"Same as rainfall c.v. except that relative farm income, that is total cash receipts divided by total cash costs, is used instead.";
helpText[['all']]<-"All indices";
helpText[['human']]<-"Human indices";
helpText[['social']]<-"Social indices";
helpText[['natural']]<-"Natural indices";
helpText[['physical']]<-"Physical indices";
helpText[['financial']]<- "Financial indices";

nearestID <- function(longitude, latitude) {
    mindist = 1E12
    minRow = -1
    for (i in 1:nrow(df)) {
        dist <- sqrt(
            (df$longitude[i] - longitude) * (df$longitude[i] - longitude) +
            (df$latitude[i] - latitude) * (df$latitude[i] - latitude))
        if (dist < mindist) { minRow = i ; mindist = dist }
    }
    return(minRow)
}

df.sp <-SpatialPointsDataFrame(coords = df[,c(1,2)], data = df[,c(-1,-2)],
                               proj4string = CRS("+proj=longlat +datum=WGS84"))

# Define UI for application that draws a histogram
ui <-bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10,
                  selectInput('measure', 'Measure', names(df)[c(-1, -2)]),
                  plotOutput('radar'),
                  plotOutput('bar')),
    absolutePanel(bottom = 10, left = 10,
                  textOutput("text"))
                                                  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    getRaster <- eventReactive (input$measure, {
        rasterFromXYZ(df[,c("longitude", "latitude", input$measure)], crs=CRS("+proj=longlat +datum=WGS84"))
    })
    
    
    output$map <-renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            fitBounds(min(df$longitude), min(df$latitude), max(df$longitude), max(df$latitude))
    })
    
    observe({
        pal <- colorBin(c("#ffffcc","#c2e699","#78c679","#31a354","#006837"), c(0,33,50,66, 100),
                        na.color = "transparent")
        leafletProxy("map") %>%
            clearImages() %>%
            addRasterImage(getRaster(), colors = pal, opacity = 0.5)
    })
    
    rv_location <- reactiveValues(id=NULL,lat=NULL,lng=NULL)
    
    observeEvent(input$map_click,{
        rv_location$lng <- input$map_click$lng
        rv_location$lat <- input$map_click$lat
        rv_location$id <- nearestID(input$map_click$lng, input$map_click$lat)
        leafletProxy('map')%>%
            clearMarkers() %>% 
            addMarkers(lng = input$map_click$lng, lat = input$map_click$lat)

    })
    output$radar <-renderPlot({
        if (!is.null(rv_location$id)) {
            cell <- cbind(group=1, df[rv_location$id, c("human", "social", "natural", "physical", "financial")])
        #cell$name <- factor(cell$name, levels=rev(names(df)[c(-1, -2)]))
            g<- ggradar(cell, grid.min = 0, grid.mid = 50, grid.max = 100) 
            return(g)
        }
    })
    output$bar <-renderPlot({
        cell <- df[rv_location$id, ] %>% 
            dplyr::select(-longitude, -latitude) %>%
            pivot_longer(cols = everything())
        cell$name <- factor(cell$name, levels=rev(names(df)[c(-1, -2)]))
        g<- ggplot(cell) + 
            geom_bar(aes(x=name, y = value, fill=value), stat = "identity") +
            binned_scale(aesthetics = "fill",
                         scale_name = "stepsn",
                         palette = function(x) { c("#ffffcc","#c2e699","#78c679","#31a354","#006837") } ,
                         breaks= c(33,50,66),
                         limits = c(0, 100), 
                         show.limits = TRUE,
                         guide = "colorsteps") +
            labs(y="%", x= "") +
            coord_flip() + 
            theme_minimal()
        return(g)
    })
    
    getText <- eventReactive (input$measure, { return(helpText[[ input$measure ]])})
                              
    output$text <- renderText({
        getText()
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
