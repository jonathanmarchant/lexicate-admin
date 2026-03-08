library(shiny)
library(gt)

# Global variables can go here
userlist <- get_users(con)


# Define the UI
ui <- bootstrapPage(
  selectInput("user", 'User:', userlist),
  actionButton("refresh", "Refresh"),
  gt_output("user_table")
)


# Define the server code
server <- function(input, output) {
  output$user_table <- render_gt({
    input$refresh
    user_summary(con, input$user)
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)