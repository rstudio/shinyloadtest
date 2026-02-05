library(shiny)
library(bslib)
library(magrittr)

ui <- page_fluid(
  title = "Reactive Functions Demo",
  shinyjs::useShinyjs(), # Set up shinyjs
  card(
    card_header("Reactive Functions in Shiny"),
    layout_column_wrap(
      width = 1 / 2,
      card(
        # Input file
        fileInput("file1", "Choose a file"),
        # Error
        actionButton("btn_error", "Increment error counter"),
        # Custom message, inputMessages
        actionButton("btn_shinyjsToggle", "shinyjs toggle"),
        # Freeze, inputMessages, values
        actionButton("btn_freezeReactiveValue", "Freeze"),

        # TODO-barret Make request

        actionButton("btn_notification_show", "Show notification"),
        actionButton("btn_modal_show", "Show modal"),
        actionButton("btn_insert_ui", "Insert UI"),
        actionButton("btn_insert_tab", "Insert Tab"),
        actionButton("btn_toggle_tab", "Hide Tab B"),

        actionButton("btn_update_query_string", "Update query string"),
        actionButton("btn_reset_brush", "Reset Brush"),
        actionButton("btn_reload", "Reload app"),
        NULL
      ),
      card(
        verbatimTextOutput("file_output", placeholder = TRUE),
        verbatimTextOutput("error_output", placeholder = TRUE),
        textInput("txt", NULL, "Click toggle button to make me disappear"),
        checkboxGroupInput("freeze_bxs", "Select boxes", c("A", "B", "C"), "A"),
        verbatimTextOutput("freeze_bxs_txt", placeholder = TRUE),
        NULL,
        div(id = "ui_anchor"),
        tabsetPanel(id = "tabs"),
        tabsetPanel(
          id = "showhide_tabs",
          tabPanel("A", "A content"),
          tabPanel("B", "B content")
        ),
        NULL
      )
    )
  )
)

server <- function(input, output, session) {
  # Upload a file to the server
  output$file_output <- renderText({
    if (is.null(input$file1)) {
      return("(No file uploaded)")
    }

    # Return file path name only
    paste0("Temp file base name: ", input$file1$name)
  })

  # shinyjs custom message
  observe(
    {
      req(input$btn_shinyjsToggle)
      shinyjs::toggle("txt")
    },
    label = "shinyjs toggle - observe"
  )

  # Freeze
  observe({
    req(input$btn_freezeReactiveValue)

    freezeReactiveValue(input, "cols")

    if (input$btn_freezeReactiveValue %% 2 == 0) {
      updateCheckboxGroupInput(
        session,
        "freeze_bxs",
        choices = c("A", "B", "C"),
        selected = "A"
      )
    } else {
      updateCheckboxGroupInput(
        session,
        "freeze_bxs",
        choices = c("X", "Y", "Z"),
        selected = "X"
      )
    }
  })

  output$freeze_bxs_txt <- renderText({
    paste("Selected boxes:", paste(input$freeze_bxs, collapse = ", "))
  })

  # Make request

  # Notification
  notification_id <- reactiveVal(NULL)
  observeEvent(input$btn_notification_show, {
    id <- showNotification(
      tagList(
        "Notification",
        tags$br(),
        actionButton("btn_notification_close", "Close notification")
      ),
      duration = NULL,
      closeButton = FALSE
    )
    notification_id(id)
    shinyjs::disable("btn_notification_show")
  })
  observeEvent(input$btn_notification_close, {
    removeNotification(notification_id())
    notification_id(NULL)
    shinyjs::enable("btn_notification_show")
  })

  # Modal
  observeEvent(input$btn_modal_show, {
    showModal(modalDialog(
      "Modal contents",
      tags$br(),
      actionButton("btn_modal_close", "Close modal"),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  observeEvent(input$btn_modal_close, {
    removeModal()
  })

  # Insert UI
  observeEvent(input$btn_insert_ui, {
    insertUI(
      selector = "#add",
      where = "afterEnd",
      ui = textInput(paste0("txt", input$add), "Insert some text")
    )

    insertUI(
      selector = "#ui_anchor",
      ui = actionButton("btn_remove_ui", "Remove UI")
    )
    shinyjs::disable("btn_insert_ui")
  })

  # Remove UI
  observeEvent(input$btn_remove_ui, {
    removeUI(selector = "#ui_anchor > *")
    shinyjs::enable("btn_insert_ui")
  })

  # Insert Tab
  observeEvent(input$btn_insert_tab, {
    insertTab(
      inputId = "tabs",
      tabPanel(
        title = "tab",
        actionButton("btn_remove_tab", "Remove Tab")
      ),
      select = TRUE
    )
    shinyjs::disable("btn_insert_tab")
  })

  # Remove Tab
  observeEvent(input$btn_remove_tab, {
    removeTab(inputId = "tabs", target = "tab")
    shinyjs::enable("btn_insert_tab")
  })

  # Show/Hide Tab
  observeEvent(input$btn_toggle_tab, {
    if (input$btn_toggle_tab %% 2 == 0) {
      showTab(
        inputId = "showhide_tabs",
        target = "B",
      )
      updateActionButton(inputId = "btn_toggle_tab", label = "Hide Tab B")
    } else {
      hideTab(
        inputId = "showhide_tabs",
        target = "B",
      )
      updateActionButton(inputId = "btn_toggle_tab", label = "Show Tab B")
    }
  })

  # Update query string
  observeEvent(input$btn_update_query_string, {
    updateQueryString(
      paste0("#foo=", input$btn_update_query_string),
      mode = "push"
    )
  })

  # Reset Brush
  observeEvent(input$btn_reset_brush, {
    # No actual brush to reset, but the communication will still be fired
    session$resetBrush("does_not_exist")
  })

  # Reload
  observeEvent(input$btn_reload, {
    session$reload()
  })

  # Error counter
  # Put at end due to odd error handling
  output$error_output <- renderText({
    options("show.error.messages" = FALSE)
    later::later(delay = 0, function() {
      options("show.error.messages" = TRUE)
    })
    stop(input$btn_error)
  })
}

shinyApp(ui, server)
