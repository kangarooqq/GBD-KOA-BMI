library(shiny)
library(dplyr)
library(readr)
library(plotly)
library(ggplot2)

# 读取数据
data <- read_csv("GBD KOA BMI SIX 50.csv")

# 定义单位映射
metric_units <- list(
  "Number" = "Count",         
  "Rate" = "Per 100,000",     
  "Percent" = "%"             
)

# UI
ui <- fluidPage(
  tags$div(
    style = "text-align: center; font-size: 24px; font-weight: bold; margin-bottom: 10px;",
    "High BMI-Attributed YLDs for KOA: A Global Burden Analysis (1990-2021)"
  ),
  tags$div(
    style = "text-align: center; font-size: 14px; color: gray; margin-bottom: 20px;",
    "Version 1.0 design by Dr. KANG CHENGWEI, 2 January 2025"
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("location", "Select Country:", 
                  choices = unique(data$location_name), 
                  selected = "Global"),
      sliderInput("year_range", "Select Year Range:",
                  min = min(data$year), max = max(data$year), 
                  value = c(1990, 2020), step = 1),
      selectInput("sex", "Select Sex:", 
                  choices = unique(data$sex_name), 
                  selected = "Both"),
      selectInput("age", "Select Age Group:", 
                  choices = unique(data$age_name), 
                  selected = "50-54 years"),
      selectInput("metric", "Select Metric Type:", 
                  choices = unique(data$metric_name), 
                  selected = "Number"),
      downloadButton("downloadPlot", "Download Plot"),
      downloadButton("downloadData", "Download Data")  # 添加数据导出按钮
    ),
    mainPanel(
      plotlyOutput("trendPlot")
    )
  )
)

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    data %>% 
      filter(location_name == input$location,
             year >= input$year_range[1],
             year <= input$year_range[2],
             sex_name == input$sex,
             age_name == input$age,
             metric_name == input$metric)
  })
  
  output$trendPlot <- renderPlotly({
    plot_data <- filtered_data()
    unit <- metric_units[[input$metric]]
    
    plot_ly(
      data = plot_data,
      x = ~year,
      y = ~val,
      type = 'scatter',
      mode = 'lines+markers',
      marker = list(color = 'red', size = 8),
      line = list(color = 'blue', width = 2),
      hoverinfo = 'text',
      text = ~paste(
        "Year: ", year,
        "<br>Value: ", round(val, 2), " ", unit,
        "<br>Metric: ", input$metric
      )
    ) %>%
      layout(
        title = list(
          text = paste("Trends in", input$metric, "for", input$location),
          x = 0.5
        ),
        xaxis = list(title = "Year"),
        yaxis = list(title = paste("Value (", unit, ")", sep = "")),
        hovermode = "closest"
      )
  })
  
  # 下载图表 (PNG, 300 DPI)
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("TrendPlot_", input$location, "_", input$metric, ".png", sep = "")
    },
    content = function(file) {
      plot_data <- filtered_data()
      unit <- metric_units[[input$metric]]
      
      p <- ggplot(plot_data, aes(x = year, y = val)) +
        geom_line(color = "blue", size = 1.2) +
        geom_point(color = "red", size = 2) +
        labs(
          title = paste("Trends in", input$metric, "for", input$location),
          subtitle = paste(input$sex, "-", input$age, "(", min(input$year_range), "-", max(input$year_range), ")"),
          x = "Year",
          y = paste("Value (", unit, ")", sep = "")
        ) +
        theme_minimal(base_size = 14) +
        theme(
          axis.line = element_line(color = "black"), # 添加黑色坐标轴线
          panel.grid.major = element_line(color = "gray90"), # 调整主网格线颜色
          panel.grid.minor = element_blank(), # 隐藏次网格线
          panel.border = element_blank() # 移除边框
        )
      
      # 保存为 300 DPI 的 PNG 文件
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300, bg = "white")
    }
  )
  
  # 下载数据
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("FilteredData_", input$location, "_", input$metric, ".csv", sep = "")
    },
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
