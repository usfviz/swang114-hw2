library(ggplot2)
library(shiny)

ui <- fluidPage(

  headerPanel(h4('MSAN 622 Assignment 2 - Su Wang')),
  
  sidebarPanel(
    sliderInput("year", "Year", min = 1960, max = 2014, step = 1, value= 1960, animate = animationOptions(interval = 550)),
    checkboxGroupInput("region", "Regions to show:", unique(DF$Region))
  ),
  
  mainPanel(
    plotOutput("ggplot", hover = "plot_hover"),
    uiOutput("hover_info")
  )
)

server <- function(input, output){
  
  input_year <- reactive({input$year})
  input_region <- reactive({input$region})
  
  output$ggplot <- renderPlot({
    if (is.null(input_region())){
      ggplot() + 
      geom_point(data = subset(DF, Year == input_year()), 
      aes( x = life_expectancy, y = fertility_rate, size = Population, fill = Region), colour="black", pch=21, alpha=.95) + 
      xlim(10,90) + ylim(0,10) + scale_size(guide = "none") +
      xlab("\n Life Expectancy \n (point size represents country population)") + ylab("Fertility Rate \n") + 
      ggtitle("Fertility vs Life Expectancy \n") + 
      scale_size_area(max_size=30, guide = "none") + 
      theme(text = element_text(size=15)) + 
      guides(fill = guide_legend(override.aes = list(size=8)))
    }
    else{
      ggplot() + 
      geom_point(data = subset(DF, Year == input_year() & Region %in% c(input_region())), 
      aes(x = life_expectancy, y = fertility_rate, fill = Region, size = Population), alpha=.95, colour="black", pch=21) +
      geom_point(data = subset(DF, Year == input_year() & !Region %in% c(input_region())), 
      aes(x = life_expectancy, y = fertility_rate, fill = Region, size = Population), alpha=.1, alpha=.95, colour="black", pch=21) +
      xlim(10,90) + ylim(0,10) + scale_size(guide = "none") +
      xlab("\n Life Expectancy \n (point size represents country population)") + ylab("Fertility Rate \n") + 
      ggtitle("Fertility vs Life Expectancy \n") + 
      scale_size_area(max_size=30, guide = "none") + 
      theme(text = element_text(size=15)) + 
      guides(fill = guide_legend(override.aes = list(size=8)))
    }
    },
    height = 550, width = 750)
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(DF, hover, threshold = 1, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    
    
    wellPanel(style = style, p(HTML(paste0("<b>", point$`Country Name`,"</b>",
                                           "<br> Population in million: ", point$Population/1000000.0)))
    )
  })
}

shinyApp(ui = ui, server = server)
