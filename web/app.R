library(shiny)
source("todo.R")

ui <- fluidPage(
    titlePanel("Task List"),
    sidebarLayout(
        sidebarPanel(
            textInput(
                inputId = "text",
                label = "Task Name"
            ),
            actionButton(
                inputId = "add",
                label = "Add Task"
            ),
            actionButton(
                inputId = "done",
                label = "Mark Task Complete"
            ),
            actionButton(
                inputId = "remove",
                label = "Remove Task"
            )
        ),
        mainPanel(
            tableOutput(
                outputId = "data"
            )
        )
    ),
)

update_table <- function(output, session) {
    output$data <- renderTable(get_tasks())
    updateTextInput(session, "text", value = "")
}

server <- function(input, output, session) {
    update_table(output, session)

    observeEvent(input$add, {
        add_task(input$text)
        update_table(output, session)
    })

    observeEvent(input$done, {
        done_task(input$text)
        update_table(output, session)
    })

    observeEvent(input$remove, {
        remove_task(input$text)
        update_table(output, session)
    })
}

shinyApp(ui = ui, server = server)