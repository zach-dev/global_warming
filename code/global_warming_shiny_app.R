## This is a shiny app that loads and plots a number of different globabl warming centric
## data sets as defined by the user.  Currently, these cleaned and prepped data sets must
## exist in the data directory that is at the same level as this code directory.  This app
## should be run from the code direcotry

## Zach Wilson - 20161209

library(shiny)

#source("data_load_and_clean.R")
# create list of possible data sets to be displayed
avail_data <- c("None", "Arctic Sea Ice", "Antarctic Sea Ice", "Glacier Ice", "Sea Level",
                "Sea Temperature", "United States Average Temperature in August", 
                "Hadley, England Temperature", "Arctic Surface Temperature",
                "Antarctic Surface Temperature")

# create the shiny gui to be used for displaying the above data sets
ui <- fluidPage(
  titlePanel("Global Warming Plot GUI"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_input", "Year", min = 1878, max = 2016, value = c(1958, 2016), sep = ""),
      selectInput("plot_2_input", "Data For Plot 2", avail_data),
      selectInput("plot_3_input", "Data For Plot 3", avail_data)),
    mainPanel(
      plotOutput("carbon_plot", height = 335),
      plotOutput("plot_2", height = 335),
      plotOutput("plot_3", height = 335)
    )
  )
)

# define server logic
server <- function(input, output) {
  # function to plot co2 content of the atmosphere
  output$carbon_plot <- renderPlot({
    # load carbon data
    co2 <- read_csv("../data/co2/co2_recent.csv")
    
    # filter to specified year range
    co2 <- filter(co2, year >= input$year_input[1] & year <= input$year_input[2])
    
    # plot data
    ggplot(co2, aes(decimal_date, interpolated)) + geom_point() + geom_line() + 
           xlim(input$year_input[1], input$year_input[2]) +
           ggtitle("Monthly Mean CO_2 at Mauna Loa") +
           labs(x = "Year", y = "CO_2 (parts per million)")
  }, height = 300)
  
  # function to plot user selected data set in plot 2
  output$plot_2 <- renderPlot({
    if (input$plot_2_input == "Arctic Sea Ice") {
      # load arctic ice data
      arctic_ice <- read_csv("../data/ice/arctic_sea_ice.csv")
      
      # filter to specified year range
      arctic_ice <- filter(arctic_ice, year >= input$year_input[1] & year <= input$year_input[2])
      
      # plot arctic ice data
      ggplot(arctic_ice, aes(year, september)) + geom_point() + geom_line() + 
             xlim(input$year_input[1], input$year_input[2]) +
             ggtitle("Arctic Sea Ice Yearly Minimum Extent") +
             labs(x = "Year", y = "Sea Ice Extent (millions of sq. miles)")
      
    } else if (input$plot_2_input == "Antarctic Sea Ice") {
      # load antarctic ice data
      antarctic_ice <- read_csv("../data/ice/antarctic_sea_ice.csv")
      
      # filter to specified year range
      antarctic_ice <- filter(antarctic_ice, year >= input$year_input[1] & year <= input$year_input[2])
      
      # plot antarctic ice data
      ggplot(antarctic_ice, aes(year, february)) + geom_point() + geom_line() + 
             xlim(input$year_input[1], input$year_input[2]) +
             ggtitle("Antarctic Sea Ice Yearly Minimum Extent") +
             labs(x = "Year", y = "Sea Ice Extent (millions of sq. miles)")
      
    } else if (input$plot_2_input == "Glacier Ice") {
      # load glacier ice data
      glacier_ice <- read_csv("../data/ice/glacier_ice.csv")
      
      # filter to specified year range
      glacier_ice <- filter(glacier_ice, year >= input$year_input[1] & year <= input$year_input[2])
      
      # plot glacier ice data
      ggplot(glacier_ice, aes(year, mass_balance)) + geom_point() + geom_line() + 
             xlim(input$year_input[1], input$year_input[2]) +
             ggtitle("Average Cumulative Mass Balance of Worldwide Reference Glaciers") +
             labs(x = "Year", y = "Cumulative Mass Balance (meters of water equivalent)")
      
    } else if (input$plot_2_input == "Sea Level") {
      # load sea level data
      sea_level <- read_csv("../data/sea_level/sea_level.csv")
      
      # filter to specified year range
      sea_level <- filter(sea_level, year >= input$year_input[1] & year <= input$year_input[2])
      
      # plot sea level data
      ggplot(sea_level, aes(year, adj_sea_level)) + geom_point() + geom_line() + 
             xlim(input$year_input[1], input$year_input[2]) +
             ggtitle("Global Average Absolute Sea Level Change") +
             labs(x = "Year", y = "Cumulative Sea Level Change (inches)")
      
    } else if (input$plot_2_input == "Sea Temperature") {
      # load sea surface temperature data
      # 1971 - 2000 average is zero
      sea_surface_temp <- read_csv("../data/surface_temp/sea_surface_temp.csv")
      
      # filter to specified year range
      sea_surface_temp <- filter(sea_surface_temp, year >= input$year_input[1] & year <= input$year_input[2])
      
      # plot sea surface temperature data
      ggplot(sea_surface_temp, aes(year, annual_anomaly)) + geom_point() + geom_line() + 
             xlim(input$year_input[1], input$year_input[2]) +
             ggtitle("Average Global Sea Surface Temperature") +
             labs(x = "Year", y = "Surface Temperature (deg F)")
      
    } else if (input$plot_2_input == "Hadley, England Temperature") {
      # load hadley england temperature data
      hadley_temp <- read_csv("../data/surface_temp/hadley_temp.csv")
      
      # filter to specified year range
      hadley_temp <- filter(hadley_temp, year >= input$year_input[1] & year <= input$year_input[2])
      
      # plot hadley england temprature data
      ggplot(hadley_temp, aes(year, year_avg)) + geom_point() + geom_line() + 
             xlim(input$year_input[1], input$year_input[2]) +
             ggtitle("Hadley England Yearly Average Temperature") +
             labs(x = "Year", y = "Temperature (deg C)")
      
    } else if (input$plot_2_input == "United States Average Temperature in August") {
      # load contiguous united states august average temperature data
      contig_48_temp <- read_csv("../data/surface_temp/contig_48_avg_temp_aug.csv")
      
      # filter to specified year range
      contig_48_temp <- filter(contig_48_temp, year >= input$year_input[1] & year <= input$year_input[2])
      
      # plot us temperature data
      ggplot(contig_48_temp, aes(year, temp)) + geom_point() + geom_line() + 
             xlim(input$year_input[1], input$year_input[2]) +
             ggtitle("Contiguous 48 United States Yearly Average Temperature") +
             labs(x = "Year", y = "Temperature (deg F)")
      
    } else if (input$plot_2_input == "Arctic Surface Temperature") {
      # load arctic surface temperature data
      arctic_surf_temp <- read_csv("../data/surface_temp/arctic_surface_temp.csv")
      
      # grab only temperatures less than 50 to avoid -999 bad data markers
      arctic_surf_temp <- arctic_surf_temp %>% filter(dbt < 50)
      
      # filter to specified year range
      arctic_surf_temp <- filter(arctic_surf_temp, year >= input$year_input[1] & year <= input$year_input[2])
      
      # summarize and grab each years maximum temperature
      ast <- arctic_surf_temp %>% group_by(year) %>% summarize(max_temp = max(dbt))
      
      # plot arctic max temperature data
      ggplot(ast, aes(year, max_temp)) + geom_point() + geom_line() + 
             xlim(input$year_input[1], input$year_input[2]) +
             ggtitle("Station 043900 Arctic Temperature History") +
             labs(x = "Year", y = "Temperature (deg F)")
      
    } else if (input$plot_2_input == "Antarctic Surface Temperature") {
      # load antarctic surface temperature data
      antarctic_surf_temp <- read_csv("../data/surface_temp/antarctic_surface_temp.csv")
      
      # grab only temperatures less than 50 to avoid -999 bad data markers
      antarctic_surf_temp <- antarctic_surf_temp %>% filter(dbt < 50)
      
      # filter to specified year range
      antarctic_surf_temp <- filter(antarctic_surf_temp, year >= input$year_input[1] & year <= input$year_input[2])
      
      # summarize and grab each years maximum temperature
      aast <- antarctic_surf_temp %>% group_by(year) %>% summarize(max_temp = max(dbt))
      
      # plot antarctic max temperature data
      ggplot(aast, aes(year, max_temp)) + geom_point() + geom_line() + 
             xlim(input$year_input[1], input$year_input[2]) +
             ggtitle("Station 896110 Antarctic Temperature History") +
             labs(x = "Year", y = "Temperature (deg F)")
    }
  }, height = 300)
  
  # function to plot user selected data set in plot 2
  output$plot_3 <- renderPlot({
    if (input$plot_3_input == "Arctic Sea Ice") {
      # load arctic ice data
      arctic_ice <- read_csv("../data/ice/arctic_sea_ice.csv")
      
      # filter to specified year range
      arctic_ice <- filter(arctic_ice, year >= input$year_input[1] & year <= input$year_input[2])
      
      # plot arctic ice data
      ggplot(arctic_ice, aes(year, september)) + geom_point() + geom_line() + 
             xlim(input$year_input[1], input$year_input[2]) +
             ggtitle("Arctic Sea Ice Yearly Minimum Extent") +
             labs(x = "Year", y = "Sea Ice Extent (millions of sq. miles)")
      
    } else if (input$plot_3_input == "Antarctic Sea Ice") {
      # load antarctic ice data
      antarctic_ice <- read_csv("../data/ice/antarctic_sea_ice.csv")
      
      # filter to specified year range
      antarctic_ice <- filter(antarctic_ice, year >= input$year_input[1] & year <= input$year_input[2])
      
      # plot antarctic ice data
      ggplot(antarctic_ice, aes(year, february)) + geom_point() + geom_line() + 
             xlim(input$year_input[1], input$year_input[2]) +
             ggtitle("Antarctic Sea Ice Yearly Minimum Extent") +
             labs(x = "Year", y = "Sea Ice Extent (millions of sq. miles)")
      
    } else if (input$plot_3_input == "Glacier Ice") {
      # load glacier ice data
      glacier_ice <- read_csv("../data/ice/glacier_ice.csv")
      
      # filter to specified year range
      glacier_ice <- filter(glacier_ice, year >= input$year_input[1] & year <= input$year_input[2])
      
      # plot glacier ice data
      ggplot(glacier_ice, aes(year, mass_balance)) + geom_point() + geom_line() + 
             xlim(input$year_input[1], input$year_input[2]) +
             ggtitle("Average Cumulative Mass Balance of Worldwide Reference Glaciers") +
             labs(x = "Year", y = "Cumulative Mass Balance (meters of water equivalent)")
      
    } else if (input$plot_3_input == "Sea Level") {
      # load sea level data
      sea_level <- read_csv("../data/sea_level/sea_level.csv")
      
      # filter to specified year range
      sea_level <- filter(sea_level, year >= input$year_input[1] & year <= input$year_input[2])
      
      # plot sea level data
      ggplot(sea_level, aes(year, adj_sea_level)) + geom_point() + geom_line() + 
             xlim(input$year_input[1], input$year_input[2]) +
             ggtitle("Global Average Absolute Sea Level Change") +
             labs(x = "Year", y = "Cumulative Sea Level Change (inches)")
      
    } else if (input$plot_3_input == "Sea Temperature") {
      # load sea surface temperature data
      # 1971 - 2000 average is zero
      sea_surface_temp <- read_csv("../data/surface_temp/sea_surface_temp.csv")
      
      # filter to specified year range
      sea_surface_temp <- filter(sea_surface_temp, year >= input$year_input[1] & year <= input$year_input[2])
      
      # plot sea surface temperature data
      ggplot(sea_surface_temp, aes(year, annual_anomaly)) + geom_point() + geom_line() + 
             xlim(input$year_input[1], input$year_input[2]) +
             ggtitle("Average Global Sea Surface Temperature") +
             labs(x = "Year", y = "Surface Temperature (deg F)")
      
    } else if (input$plot_3_input == "Hadley, England Temperature") {
      # load hadley england temperature data
      hadley_temp <- read_csv("../data/surface_temp/hadley_temp.csv")
      
      # filter to specified year range
      hadley_temp <- filter(hadley_temp, year >= input$year_input[1] & year <= input$year_input[2])
      
      # plot hadley england temprature data
      ggplot(hadley_temp, aes(year, year_avg)) + geom_point() + geom_line() + 
             xlim(input$year_input[1], input$year_input[2]) +
             ggtitle("Hadley England Yearly Average Temperature") +
             labs(x = "Year", y = "Temperature (deg C)")
      
    } else if (input$plot_3_input == "United States Average Temperature in August") {
      # load contiguous united states august average temperature data
      contig_48_temp <- read_csv("../data/surface_temp/contig_48_avg_temp_aug.csv")
      
      # filter to specified year range
      contig_48_temp <- filter(contig_48_temp, year >= input$year_input[1] & year <= input$year_input[2])
      
      # plot us temperature data
      ggplot(contig_48_temp, aes(year, temp)) + geom_point() + geom_line() + 
             xlim(input$year_input[1], input$year_input[2]) +
             ggtitle("Contiguous 48 United States Yearly Average Temperature") +
             labs(x = "Year", y = "Temperature (deg F)")
      
    } else if (input$plot_3_input == "Arctic Surface Temperature") {
      # load arctic surface temperature data
      arctic_surf_temp <- read_csv("../data/surface_temp/arctic_surface_temp.csv")
      
      # grab only temperatures less than 50 to avoid -999 bad data markers
      arctic_surf_temp <- arctic_surf_temp %>% filter(dbt < 50)
      
      # filter to specified year range
      arctic_surf_temp <- filter(arctic_surf_temp, year >= input$year_input[1] & year <= input$year_input[2])
      
      # summarize and grab each years maximum temperature
      ast <- arctic_surf_temp %>% group_by(year) %>% summarize(max_temp = max(dbt))
      
      # plot arctic max temperature data
      ggplot(ast, aes(year, max_temp)) + geom_point() + geom_line() + 
             xlim(input$year_input[1], input$year_input[2]) +
             ggtitle("Station 043900 Arctic Temperature History") +
             labs(x = "Year", y = "Temperature (deg F)")
      
    } else if (input$plot_3_input == "Antarctic Surface Temperature") {
      # load antarctic surface temperature data
      antarctic_surf_temp <- read_csv("../data/surface_temp/antarctic_surface_temp.csv")
      
      # grab only temperatures less than 50 to avoid -999 bad data markers
      antarctic_surf_temp <- antarctic_surf_temp %>% filter(dbt < 50)
      
      # filter to specified year range
      antarctic_surf_temp <- filter(antarctic_surf_temp, year >= input$year_input[1] & year <= input$year_input[2])
      
      # summarize and grab each years maximum temperature
      aast <- antarctic_surf_temp %>% group_by(year) %>% summarize(max_temp = max(dbt))
      
      # plot antarctic max temperature data
      ggplot(aast, aes(year, max_temp)) + geom_point() + geom_line() + 
             xlim(input$year_input[1], input$year_input[2]) +
             ggtitle("Station 896110 Antarctic Temperature History") +
             labs(x = "Year", y = "Temperature (deg F)")
    }
  }, height = 300)
}

shinyApp(ui = ui, server = server)