#Group 3 - Phase 5
#HYDROTRACKER DASHBOARD

#load packages
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(DT)

#dashboard
ui <- dashboardPage(
  
  #header
  dashboardHeader(
    title = "HydroTracker"
  ),
  
  #sidebar
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      
      menuItem(
        "Water Quality Monitoring",
        tabName = "monitoring",
        icon = icon("tint")
      ),
      
      menuItem(
        "Data Analysis",
        tabName = "analysis",
        icon = icon("chart-line")
      )
      
    ),
    
    br(),
    
    #date filter
    dateRangeInput(
      "dates",
      "Select Date Range:",
      start = min(as.Date(water_clean$Timestamp)),
      end = max(as.Date(water_clean$Timestamp))
    ),
    
    br(),
    
    #parameter filter
    selectInput(
      "variable",
      "Select Parameter:",
      choices = c(
        "Temperature",
        "Dissolved Oxygen",
        "pH",
        "Turbidity"
      )
    )
    
  ),
  
  #body
  dashboardBody(
    
    tags$head(
      tags$style(HTML("

        .skin-blue .main-header .logo {
          background-color: #0B5A8A;
          color: white;
          font-weight: bold;
        }

        .skin-blue .main-header .navbar {
          background-color: #0B5A8A;
        }

        .skin-blue .main-sidebar {
          background-color: #08293D;
        }

        .content-wrapper {
          background-color: #F4F6F9;
        }

      "))
    ),
    
    tags$script(
      HTML("
        Shiny.addCustomMessageHandler('print', function(message) {
          window.print();
        });
      ")
    ),
    
    tabItems(
      
      #dashboard tab
      tabItem(
        
        tabName = "dashboard",
        
        fluidRow(
          
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            div(
              style = "text-align:center; padding:15px;",
              
              h2(
                "HydroTracker",
                style = "color:#0B5A8A; font-weight:bold;"
              ),
              
              h4(
                "Water Quality Monitoring Dashboard"
              ),
              
              p(
                "Interactive monitoring and analysis of Temperature, Dissolved Oxygen, pH, and Turbidity measurements.",
                style = "font-size:15px;"
              ),
              
              actionButton(
                "printDashboard",
                "Print Dashboard",
                icon = icon("print")
              )
              
            )
            
          )
          
        ),
        
        fluidRow(
          
          valueBoxOutput("tempBox", width = 3),
          valueBoxOutput("doBox", width = 3),
          valueBoxOutput("phBox", width = 3),
          valueBoxOutput("turbBox", width = 3)
          
        ),
        
        fluidRow(
          
          box(
            title = "Water Quality Trend Analysis",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("trendPlot", height = 500)
          )
          
        )
        
      ),
      
      #monitoring tab
      tabItem(
        
        tabName = "monitoring",
        
        fluidRow(
          
          box(
            title = "Distribution Analysis",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("histPlot", height = 450)
          ),
          
          box(
            title = "Outlier Analysis",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            plotOutput("boxPlot", height = 450)
          )
          
        )
        
      ),
      
      #analysis tab
      tabItem(
        
        tabName = "analysis",
        
        fluidRow(
          
          box(
            title = "Summary Statistics",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            DTOutput("summaryTable")
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Water Quality Dataset",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            DTOutput("dataTable")
          )
          
        )
        
      )
      
    )
    
  )
  
)

#server
server <- function(input, output, session) {
  
  #filtered data using dplyr
  filtered_data <- reactive({
    
    water_clean %>%
      filter(
        as.Date(Timestamp) >= input$dates[1],
        as.Date(Timestamp) <= input$dates[2]
      )
    
  })
  
  #print dashboard
  observeEvent(input$printDashboard, {
    
    session$sendCustomMessage(
      "print",
      list()
    )
    
  })
  
  #temperature card
  output$tempBox <- renderValueBox({
    
    valueBox(
      paste0(round(mean(filtered_data()$Temperature, na.rm = TRUE), 2), " °C"),
      "Average Temperature",
      icon = icon("thermometer-half"),
      color = "blue"
    )
    
  })
  
  #dissolved oxygen card
  output$doBox <- renderValueBox({
    
    valueBox(
      paste0(round(mean(filtered_data()[["Dissolved Oxygen"]], na.rm = TRUE), 2), " mg/L"),
      "Average Dissolved Oxygen",
      icon = icon("tint"),
      color = "light-blue"
    )
    
  })
  
  #ph card
  output$phBox <- renderValueBox({
    
    valueBox(
      round(mean(filtered_data()$pH, na.rm = TRUE), 2),
      "Average pH Level",
      icon = icon("flask"),
      color = "teal"
    )
    
  })
  
  #turbidity card
  output$turbBox <- renderValueBox({
    
    valueBox(
      paste0(round(mean(filtered_data()$Turbidity, na.rm = TRUE), 2), " NTU"),
      "Average Turbidity",
      icon = icon("tint"),
      color = "navy"
    )
    
  })
  
  #trend chart
  output$trendPlot <- renderPlotly({
    
    plot_ly(
      filtered_data(),
      x = ~Timestamp,
      y = ~Turbidity,
      type = "scatter",
      mode = "lines",
      line = list(color = "#0B5A8A")
    ) %>%
      layout(
        title = "Water Quality Trend Analysis",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Turbidity (NTU)")
      )
    
  })
  
  #histogram
  output$histPlot <- renderPlotly({
    
    plot_ly(
      x = filtered_data()[[input$variable]],
      type = "histogram",
      marker = list(color = "#3C64A1")
    ) %>%
      layout(
        title = paste(input$variable, "Distribution"),
        xaxis = list(title = input$variable),
        yaxis = list(title = "Frequency")
      )
    
  })
  
  #boxplot using ggplot2
  output$boxPlot <- renderPlot({
    
    ggplot(
      filtered_data(),
      aes_string(y = input$variable)
    ) +
      geom_boxplot(fill = "#43A9A4") +
      labs(
        title = paste(input$variable, "Boxplot"),
        y = input$variable
      ) +
      theme_minimal()
    
  })
  
  #summary table
  output$summaryTable <- renderDT({
    
    datatable(
      
      data.frame(
        
        Statistic = c(
          "Minimum",
          "Maximum",
          "Mean",
          "Median"
        ),
        
        Value = c(
          min(filtered_data()[[input$variable]], na.rm = TRUE),
          max(filtered_data()[[input$variable]], na.rm = TRUE),
          mean(filtered_data()[[input$variable]], na.rm = TRUE),
          median(filtered_data()[[input$variable]], na.rm = TRUE)
        )
        
      ),
      
      options = list(
        pageLength = 5,
        searching = FALSE
      )
      
    )
    
  })
  
  #dataset table
  output$dataTable <- renderDT({
    
    datatable(
      
      filtered_data(),
      
      extensions = "Buttons",
      
      options = list(
        
        pageLength = 10,
        
        scrollX = TRUE,
        
        autoWidth = TRUE,
        
        dom = "Bfrtip",
        
        buttons = c(
          "copy",
          "csv",
          "excel",
          "print"
        )
        
      )
      
    )
    
  })
  
}

#run app
shinyApp(ui, server)