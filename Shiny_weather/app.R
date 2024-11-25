library(shiny)

# UI
ui <- fluidPage(
  titlePanel("Ice Cream Order App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "flavor",
        label = "Select a flavor:",
        choices = c("Vanilla", "Chocolate", "Strawberry", "Mint"),
        selected = "Vanilla"
      ),
      checkboxGroupInput(
        inputId = "toppings",
        label = "Choose your toppings:",
        choices = c("Sprinkles", "Whipped Cream", "Nuts", "Cherries")
      ),
      radioButtons(
        inputId = "serving_style",
        label = "Serving Style:",
        choices = c("Cup", "Cone"),
        selected = "Cone"
      ),
      sliderInput(
        inputId = "size",
        label = "Select size (in oz):",
        min = 4,
        max = 16,
        value = 8
      )
    ),
    
    mainPanel(
      textOutput("order_summary"),
      textOutput("order_price"),
      plotOutput("ice_cream_plot", height = "300px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Generate order summary
  output$order_summary <- renderText({
    toppings_selected <- if (is.null(input$toppings) || length(input$toppings) == 0) {
      "no toppings"
    } else {
      paste(input$toppings, collapse = ", ")
    }
    paste0(
      "You ordered a ", input$size, "oz ",
      input$flavor, " ice cream in a ", input$serving_style,
      " with ", toppings_selected, "."
    )
  })
  
  # Calculate total price
  output$order_price <- renderText({
    base_price <- input$size * 0.10  # $0.10 per oz
    topping_price <- if (is.null(input$toppings)) 0 else length(input$toppings) * 0.50
    total_price <- base_price + topping_price
    paste0("Total price: $", round(total_price, 2))
  })
  
  # Draw ice cream cone
  output$ice_cream_plot <- renderPlot({
    # Simple plot to represent the ice cream
    if (input$serving_style == "Cone") {
      plot(
        c(-1, 1, 0), c(0, 0, -3), type = "n", 
        xlim = c(-2, 2), ylim = c(-3, 4), axes = FALSE, xlab = "", ylab = ""
      )
      polygon(c(-1, 1, 0), c(0, 0, -3), col = "tan", border = NA)
    } else {
      plot(
        c(-1.5, 1.5), c(0, 0), type = "n", 
        xlim = c(-2, 2), ylim = c(-3, 4), axes = FALSE, xlab = "", ylab = ""
      )
      rect(-1.5, -2.5, 1.5, 0, col = "tan", border = NA)
    }
    # Draw scoops
    colors <- c("Vanilla" = "lightgoldenrod", "Chocolate" = "chocolate",
                "Strawberry" = "pink", "Mint" = "palegreen")
    scoops <- seq(0, input$size / 4, by = 0.5)
    for (i in seq_along(scoops)) {
      y <- scoops[i]
      symbols(
        x = 0, y = y + 0.5, circles = 1, inches = FALSE,
        add = TRUE, bg = colors[input$flavor], fg = NA
      )
    }
    # Optional: Sprinkle dots for toppings
    if (!is.null(input$toppings)) {
      points(runif(50, -1, 1), runif(50, 0, input$size / 4), col = "red", pch = 16)
    }
  })
}

# Run the app
shinyApp(ui, server)
