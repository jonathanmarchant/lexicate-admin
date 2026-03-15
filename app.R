library(shiny)
library(gt)

# Global variables can go here
userlist <- get_users(con)


# Define the UI
ui <- bootstrapPage(
  selectInput("user", 'User:', userlist),
  actionButton("refresh", "Refresh"),
  plotOutput("progress"),
  gt_output("user_table")
)


# Define the server code
server <- function(input, output) {
  output$progress <- renderPlot({
    summary_log <- user_summary(con, input$user)
    progress <- progress_summary(con, summary_log)

    ggplot(progress, aes(
      x = factor(difficulty, levels = c(1, 2)),
      fill = factor(progress, levels = c("Unlearned", "Variable", "Learned")))) +
      geom_bar() +
      scale_fill_discrete(palette = c("tomato", "gold", "forestgreen"), name="Whether learned") +
      scale_x_discrete(labels = c("Yr 1", "Yr 2"), name="Difficulty") +
      coord_flip() +
      geom_text(stat = "count", aes(label = after_stat(count)), position=position_stack(vjust=0.5), size=6) +
      theme(text = element_text(size=20))

  })

  output$user_table <- render_gt({
    input$refresh
    user_summary(con, input$user) |> 
      gt() |> 
      fmt_datetime(last_attempt)
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)