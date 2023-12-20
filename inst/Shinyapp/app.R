library(shiny)
library(dplyr)
library(ggplot2)
library(immoswiss)

#lausanne <- read.csv("data/lausanne.csv")


# Define UI
ui <- fluidPage(

  # titlePanel(
  #   "# app title/description"
  # ),
  #
  #Set the theme
  theme = bslib::bs_theme(bootswatch = "flatly"),

  #Create the main page
  navbarPage(
    "Lausanne - Real Estate Market Analysis",

    # Define the first panel (title)
    tabPanel("Estimate the price",

             #Specify input button for the first panel
             sidebarLayout(
               sidebarPanel(
                 selectInput("selected_loc_sel", "Select the postal code of your proprety:",
                             choices = sort(unique(lausanne$location)),
                             selected = "1000"),

                 numericInput("rooms_sel", "Number of rooms:", 2.5 , min = 1),

                 numericInput("meter_sel", "Square Meter:", 50 , min = 5),

                 actionButton("calc_sel", "Calculate the estimated rent of your housing"),
               ),

               #Specify the output of the first panel
               mainPanel(
                 h4("Results:"),
                 uiOutput("estimation"),
                 plotOutput("plot", click = "plot_click"),
                 tableOutput("data")

               )
             )
    ),


    #Define the second panel (title)
    tabPanel("Explore your option",

             #Specify input button for the second panel
             sidebarLayout(
               sidebarPanel(
                 selectInput("selected_loc_buy", "Test the postal code of your proprety:",
                             choices = sort(unique(lausanne$location)),
                             selected = "1000"),

                 sliderInput("room_slide", "Select the number of rooms:", min = 1, max = 10,
                             value = 2.5, step = 0.5),

                 sliderInput("square_slide", "Select the squares meters:", min = 10, max = 500,
                             value = 50, step = 10),

                 sliderInput("k", "Select how many similar apartments should be given:", min = 1, max = 50,
                             value = 5, step = 1)

               ),

               #Specify the output of the second panel
               mainPanel(
                 h4("Here you can find the most similar apartments based on your requirements:",br(),br()),
                 DT::dataTableOutput("table")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output) {


  #Table for knn
  output$table <- DT::renderDataTable({

    table_knn <- find_nearest_neighbors(input$room_slide, input$square_slide, input$selected_loc_buy, lausanne, input$k)

    table_knn
  })



  #Price estimation
  rooms <- reactive(input$rooms_sel) %>% bindEvent(input$calc_sel)
  meters <- reactive(input$meter_sel) %>% bindEvent(input$calc_sel)
  loc <- reactive(input$selected_loc_sel) %>% bindEvent(input$calc_sel)

  output$estimation <- renderPrint({
      estimate <- estimate_price(rooms(), meters(), loc(),lausanne)
      results <- paste0("<p>The estimated rent of your desired house/flat is: <strong>", round(estimate[1],3), " </strong> CHF.")
      results2 <- paste0("This estimation ranges from <strong>", round(estimate[2],3), "</strong> CHF to <strong>", round(estimate[3],3), "</strong> CHF.</p><br><br><p>
                       Below you will find the change in rent per location for your desired house features <br>You can click on any point to get rent details.</p>")

    # Use HTML tags to create a line break
    HTML(paste(results, results2, sep = "<br><br>"))

  })
  #end output$estimation



  #Price estimation with all param fixed but Location
  #Get all location
  unique_loc <- sort(unique(lausanne$location))


  # Set empty data frame for price estimation
  price_loc <- data.frame("Location" = as.factor(unique_loc), "Estimate" = numeric(length(unique_loc)),
                          "Lower" = numeric(length(unique_loc)), "Upper" = numeric(length(unique_loc)))



  #Plot the results
  output$plot <- renderPlot({

    #Loop through all location to get estimate and update price_loc
    for (loc in unique_loc) {

      index <- which(price_loc$Location == loc)

      estimate <- estimate_price(rooms(), meters(), location = loc, lausanne)
      price_loc$Estimate[index] <- estimate[1]
      price_loc$Lower[index] <- estimate[2]
      price_loc$Upper[index] <- estimate[3]



    }

    #Plot definition
    ggplot(price_loc, aes(x = Location, y = Estimate, group = 1)) +
      geom_point(aes(color = "Rent estimation"), size = 4, shape = 20) +
      geom_line(linetype = "dashed", linewidth = 0.5) +
      geom_ribbon(aes(fill = "95% rent range", ymin = Lower, ymax = Upper), alpha = 0.1) +
      labs(title = "",
           x = "Postal Code", y = "Estimated Rent (CHF)",
           color = "Legend", fill = "Legend") +
      scale_color_manual(values = c("Rent estimation" = "red")) +
      scale_fill_manual(values = c("95% rent range" = "blue")) +
      theme_minimal()

  }, res = 96)
  #end output$plot


  #Interactive table reacting to input$plot_click
  output$data <- renderTable({

    #Loop through all location to get estimate and update price_loc
    for (loc in unique_loc) {

      index <- which(price_loc$Location == loc)

      estimate <- estimate_price(rooms(), meters(), location = loc,lausanne)
      price_loc$Estimate[index] <- estimate[1]
      price_loc$Lower[index] <- estimate[2]
      price_loc$Upper[index] <- estimate[3]

    }
    #Retrieve mouse coordinate and display the nearest data frame entry (no need of xvar, yvar with ggplot2)
    nearPoints(price_loc, input$plot_click)

  })
  #end output$data

}



# Run the app
shinyApp(ui, server)
