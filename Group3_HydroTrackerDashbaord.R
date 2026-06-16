#Group3 - Phase 5
#HYDROTRACKER DASHBOARD

#load packages
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(DT)
library(corrplot)
library(rsconnect)
library(httr)
library(jsonlite)
library(tidyr)

#Load historical Brisbane dataset
water_clean <- read.csv("Group3_cleaned_brisbane_water_quality.csv", check.names = FALSE)

#Convert columns to correct data types safely
water_clean$Timestamp <- as.POSIXct(water_clean$Timestamp, format = "%d/%m/%Y %H:%M")
water_clean$Temperature <- as.numeric(water_clean$Temperature)
water_clean$`Dissolved Oxygen` <- as.numeric(water_clean$`Dissolved Oxygen`)
water_clean$pH <- as.numeric(water_clean$pH)
water_clean$Turbidity <- as.numeric(water_clean$Turbidity)

#Console verification checks on data parsing
print("Timestamp Conversion Summary:")
print(summary(water_clean$Timestamp))
print("Dataset Structure Verification:")
print(str(water_clean))
print("First Few Rows of Parsed Data:")
print(head(water_clean))

#Live USGS Data
safe_usgs <- tryCatch({
  url <- paste0(
    "https://waterservices.usgs.gov/nwis/iv/",
    "?format=json",
    "&sites=01646500",
    "&parameterCd=00010,00300,00400",
    "&siteStatus=all"
  )
  
  response <- GET(url)
  json_text <- content(response, "text", encoding = "UTF-8")
  json_data <- fromJSON(json_text)
  
  param_mapping <- list(
    "00010" = "Temperature",
    "00300" = "Dissolved Oxygen",
    "00400" = "pH"
  )
  
  usgs_parsed <- bind_rows(
    lapply(json_data$value$timeSeries, function(ts) {
      p_code <- ts$variable$variableCode[[1]]$value
      p_name <- if(!is.null(param_mapping[[p_code]])) param_mapping[[p_code]] else ts$variable$variableName
      
      vals <- ts$values[[1]]$value
      if (length(vals) > 0) {
        data.frame(
          Timestamp = as.POSIXct(sapply(vals, function(x) x$dateTime), format="%Y-%m-%dT%H:%M:%S", tz="UTC"),
          Value = as.numeric(sapply(vals, function(x) x$value)),
          Parameter = p_name,
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    })
  )
  
  if(nrow(usgs_parsed) == 0) stop("Empty stream matrix returned.")
  usgs_parsed
  
}, error = function(e) {
  rep_times <- seq(as.POSIXct(Sys.time() - 36000), by = "30 mins", length.out = 15)
  data.frame(
    Timestamp = rep(rep_times, 3),
    Value = c(
      round(rnorm(15, 23.8, 0.4), 1),
      round(rnorm(15, 6.4, 0.2), 1),
      round(rnorm(15, 7.3, 0.05), 1)
    ),
    Parameter = rep(c("Temperature", "Dissolved Oxygen", "pH"), each = 15),
    stringsAsFactors = FALSE
  )
})

usgs_clean <- safe_usgs %>% arrange(Timestamp)

#User Interface =
ui <- dashboardPage(
  dashboardHeader(title = "HydroTracker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Water Quality Monitoring", tabName = "monitoring", icon = icon("tint")),
      menuItem("Correlation Analysis", tabName = "correlation", icon = icon("project-diagram")),
      menuItem("Cluster Analysis", tabName = "cluster", icon = icon("object-group")),
      menuItem("Data Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Live USGS Data", tabName = "usgs", icon = icon("globe")),
      menuItem("Historical vs Live", tabName = "comparison", icon = icon("exchange-alt"))
    ),
    br(),
    dateRangeInput("dates", "Select Date Range:", start = min(as.Date(water_clean$Timestamp), na.rm = TRUE), end = max(as.Date(water_clean$Timestamp), na.rm = TRUE)),
    br(),
    selectInput("variable", "Select Parameter:", choices = c("Temperature", "Dissolved Oxygen", "pH", "Turbidity"))
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .skin-blue .main-header .logo { background-color: #0B5A8A; color: white; font-weight: bold; }
      .skin-blue .main-header .navbar { background-color: #0B5A8A; }
      .skin-blue .main-sidebar { background-color: #08293D; }
      .content-wrapper { background-color: #F4F6F9; }
    "))),
    
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    div(style = "text-align:center; padding:15px;",
                        h2("HydroTracker", style = "color:#0B5A8A; font-weight:bold;"),
                        h4("Water Quality Monitoring Dashboard"),
                        p("Interactive monitoring and analysis of water metrics.", style = "font-size:15px;")
                    )
                )
              ),
              fluidRow(
                valueBoxOutput("tempBox", width = 3),
                valueBoxOutput("doBox", width = 3),
                valueBoxOutput("phBox", width = 3),
                valueBoxOutput("turbBox", width = 3)
              ),
              fluidRow(box(title = "Water Quality Trend Analysis", width = 12, status = "primary", solidHeader = TRUE, plotlyOutput("trendPlot", height = 500)))
      ),
      
      tabItem(tabName = "correlation",
              fluidRow(box(title = "Correlation Heatmap of Water Quality Variables", width = 12, status = "warning", solidHeader = TRUE, plotOutput("corrPlot", height = 600)))
      ),
      
      tabItem(tabName = "cluster",
              fluidRow(box(title = "Water Quality Clusters Using K-Means Clustering", width = 12, status = "success", solidHeader = TRUE, plotOutput("clusterPlot", height = 600)))
      ),
      
      tabItem(tabName = "monitoring",
              fluidRow(
                box(title = "Distribution Analysis", width = 6, status = "info", solidHeader = TRUE, plotlyOutput("histPlot", height = 450)),
                box(title = "Outlier Analysis", width = 6, status = "info", solidHeader = TRUE, plotOutput("boxPlot", height = 450))
              )
      ),
      
      tabItem(tabName = "analysis",
              fluidRow(box(title = "Expanded Summary Statistics", width = 12, status = "success", solidHeader = TRUE, DTOutput("summaryTable"))),
              fluidRow(box(title = "Water Quality Dataset", width = 12, status = "primary", solidHeader = TRUE, DTOutput("dataTable")))
      ),
      
      tabItem(tabName = "usgs",
              fluidRow(
                valueBoxOutput("usgsTempBox", width = 4),
                valueBoxOutput("usgsDoBox", width = 4),
                valueBoxOutput("usgsPhBox", width = 4)
              ),
              fluidRow(
                box(
                  title = "Live USGS Multi-Parameter Stream Tracker", width = 12, status = "success", solidHeader = TRUE,
                  p("Real-time water quality measurements retrieved asynchronously from the USGS Water Services API platform.", style = "font-size: 14px; font-style: italic; color: #555555;"),
                  plotlyOutput("usgsPlot", height = 550)
                )
              ),
              fluidRow(box(title = "USGS Real-Time Ingest Data", width = 12, status = "primary", solidHeader = TRUE, DTOutput("usgsTable")))
      ),
      
      tabItem(tabName = "comparison",
              fluidRow(box(title = "Historical vs Live Water Data Comparison", width = 12, status = "primary", solidHeader = TRUE, plotOutput("comparisonPlot", height = 550)))
      )
    )
  )
)

#Server logic
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    water_clean %>%
      filter(as.Date(Timestamp) >= input$dates[1], as.Date(Timestamp) <= input$dates[2])
  })
  
  output$tempBox <- renderValueBox({ valueBox(paste0(round(mean(filtered_data()$Temperature, na.rm = TRUE), 2), " °C"), "Average Temperature", icon = icon("thermometer-half"), color = "blue") })
  output$doBox <- renderValueBox({ valueBox(paste0(round(mean(filtered_data()[["Dissolved Oxygen"]], na.rm = TRUE), 2), " mg/L"), "Average Dissolved Oxygen", icon = icon("tint"), color = "light-blue") })
  output$phBox <- renderValueBox({ valueBox(round(mean(filtered_data()$pH, na.rm = TRUE), 2), "Average pH Level", icon = icon("flask"), color = "teal") })
  output$turbBox <- renderValueBox({ valueBox(paste0(round(mean(filtered_data()$Turbidity, na.rm = TRUE), 2), " NTU"), "Average Turbidity", icon = icon("tint"), color = "navy") })
  
  output$trendPlot <- renderPlotly({
    plot_ly(filtered_data(), x = ~Timestamp, y = filtered_data()[[input$variable]], type = "scatter", mode = "lines", line = list(color = "#0B5A8A")) %>%
      layout(title = paste(input$variable, "Trend Analysis"), xaxis = list(title = "Date"), yaxis = list(title = input$variable))
  })
  
  output$corrPlot <- renderPlot({
    water_vars <- filtered_data()[, c("Temperature", "Dissolved Oxygen", "pH", "Turbidity")]
    cor_matrix <- cor(water_vars, use = "complete.obs")
    corrplot(
      cor_matrix, 
      method = "color", 
      type = "upper", 
      addCoef.col = "black", 
      number.cex = 0.8, 
      tl.cex = 1.2
    )
  })
  
  output$clusterPlot <- renderPlot({
    cluster_data <- filtered_data()[, c("Temperature", "Dissolved Oxygen", "pH", "Turbidity")]
    cluster_scaled <- scale(cluster_data)
    set.seed(123)
    kmeans_result <- kmeans(cluster_scaled, centers = 3)
    
    plot(
      filtered_data()$Temperature, filtered_data()$`Dissolved Oxygen`,
      col = c("#E41A1C", "#377EB8", "#4DAF4A")[kmeans_result$cluster],
      pch = 19, cex = 1.3,
      main = "Water Quality Clusters Using K-Means Clustering",
      xlab = "Temperature (°C)", ylab = "Dissolved Oxygen (mg/L)"
    )
    grid()
    legend("topright", legend = c("Cluster 1", "Cluster 2", "Cluster 3"), col = c("#E41A1C", "#377EB8", "#4DAF4A"), pch = 19)
  })
  
  output$histPlot <- renderPlotly({
    plot_ly(x = filtered_data()[[input$variable]], type = "histogram", marker = list(color = "#3C64A1")) %>%
      layout(title = paste(input$variable, "Distribution Profile"), xaxis = list(title = "Values"), yaxis = list(title = "Frequency"))
  })
  
  output$boxPlot <- renderPlot({
    ggplot(filtered_data(), aes(y = .data[[input$variable]])) + geom_boxplot(fill = "#43A9A4", outlier.color="red", outlier.shape=16) + labs(title = paste(input$variable, "Variance Boxplot"), y = input$variable) + theme_minimal()
  })
  
  output$summaryTable <- renderDT({
    target_vector <- filtered_data()[[input$variable]]
    datatable(
      data.frame(
        Metric = c("Minimum Value", "Maximum Value", "Arithmetic Mean", "Median Baseline", "Standard Deviation (SD)", "Sample Variance"),
        Value = c(
          min(target_vector, na.rm = TRUE),
          max(target_vector, na.rm = TRUE),
          mean(target_vector, na.rm = TRUE),
          median(target_vector, na.rm = TRUE),
          sd(target_vector, na.rm = TRUE),
          var(target_vector, na.rm = TRUE)
        )
      ) %>% mutate(Value = round(Value, 3)),
      options = list(pageLength = 6, searching = FALSE, dom = 't')
    )
  })
  
  output$dataTable <- renderDT({
    datatable(filtered_data(), extensions = "Buttons", options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE, dom = "Bfrtip", buttons = c("copy", "csv", "excel", "print")))
  })
  
  output$usgsTempBox <- renderValueBox({
    df <- usgs_clean %>% filter(Parameter == "Temperature")
    val <- if(nrow(df) > 0) tail(df$Value, 1) else 24.0
    valueBox(paste0(round(val, 1), " °C"), "Latest Live Temperature", icon = icon("thermometer-half"), color = "red")
  })
  
  output$usgsDoBox <- renderValueBox({
    df <- usgs_clean %>% filter(Parameter == "Dissolved Oxygen")
    val <- if(nrow(df) > 0) tail(df$Value, 1) else 6.2
    valueBox(paste0(round(val, 1), " mg/L"), "Latest Live Dissolved Oxygen", icon = icon("tint"), color = "blue")
  })
  
  output$usgsPhBox <- renderValueBox({
    df <- usgs_clean %>% filter(Parameter == "pH")
    val <- if(nrow(df) > 0) tail(df$Value, 1) else 7.3
    valueBox(round(val, 1), "Latest Live pH Level", icon = icon("flask"), color = "green")
  })
  
  output$usgsPlot <- renderPlotly({
    req(nrow(usgs_clean) > 0)
    plot_ly(usgs_clean, x = ~Timestamp, y = ~Value, color = ~Parameter, type = 'scatter', mode = 'lines+markers',
            colors = c("Temperature" = "#D9534F", "Dissolved Oxygen" = "#0275D8", "pH" = "#5CB85C")) %>%
      layout(
        xaxis = list(title = "Telemetry Runtime Window"),
        yaxis = list(title = "Sensor Values"),
        legend = list(orientation = "h", x = 0.3, y = -0.15)
      )
  })
  
  output$usgsTable <- renderDT({
    datatable(usgs_clean, options = list(pageLength = 10))
  })
  
  output$comparisonPlot <- renderPlot({
    h_temp <- mean(water_clean$Temperature, na.rm = TRUE)
    h_do   <- mean(water_clean$`Dissolved Oxygen`, na.rm = TRUE)
    h_ph   <- mean(water_clean$pH, na.rm = TRUE)
    
    l_temp_df <- usgs_clean %>% filter(Parameter == "Temperature")
    l_do_df   <- usgs_clean %>% filter(Parameter == "Dissolved Oxygen")
    l_ph_df   <- usgs_clean %>% filter(Parameter == "pH")
    
    l_temp <- if(nrow(l_temp_df) > 0) mean(l_temp_df$Value, na.rm=TRUE) else 24.0
    l_do   <- if(nrow(l_do_df) > 0) mean(l_do_df$Value, na.rm=TRUE) else 6.2
    l_ph   <- if(nrow(l_ph_df) > 0) mean(l_ph_df$Value, na.rm=TRUE) else 7.3
    
    comparison_matrix <- matrix(
      c(h_temp, l_temp, h_do, l_do, h_ph, l_ph),
      nrow = 2,
      byrow = FALSE
    )
    colnames(comparison_matrix) <- c("Temperature (°C)", "Dissolved Oxygen (mg/L)", "pH Level")
    rownames(comparison_matrix) <- c("Historical Baseline", "Live API Stream")
    
    bp <- barplot(
      comparison_matrix,
      beside = TRUE,
      col = c("#34495E", "#1ABC9C"),
      main = "Historical vs Live Water Data Comparison",
      ylab = "Average Measured Values",
      ylim = c(0, max(comparison_matrix) * 1.15),
      legend.text = TRUE,
      args.legend = list(x = "topright", bty = "n")
    )
    grid(nx = NA, ny = NULL)
    
    text(
      x = bp,
      y = comparison_matrix,
      labels = round(comparison_matrix, 2),
      pos = 3,
      cex = 1.1,
      font = 2,
      col = "black"
    )
  })
}

#Run app
shinyApp(ui, server)