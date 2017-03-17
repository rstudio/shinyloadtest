#' Addin To Setup Load Test
#'
#' @details Runs a Shiny Gadget that helps users build a load test report -
#'   similar to knitting the parameterized report, but with more helpful
#'   defaults
#'
#' @import miniUI
#' @import shiny
#' @export
addinLoadTest <- function() {
  ui <- miniPage(
    miniTitleBar("Load Test",
      left = miniTitleBarCancelButton()),
    miniTabstripPanel(
      miniTabPanel("App Info", icon = icon('gear'),
       textInput("app_file",
         value = ifelse(isShinyApp(), rstudioapi::getActiveDocumentContext()$path, ""),
         label = "Shiny App to Test:",
         width = "75%"),

       uiOutput("test_file"),

       uiOutput("app_url"),

       numericInput('shiny_load_sec',
         "How many seconds does it take for your app to load for one user?",
         min = 5, max = 600,
         value = 10)
      ),

      miniTabPanel("Test Settings", icon = icon("sliders"),
       textInput('output_file',
        label = "path/name.Rmd for results report:",
        placeholder = paste0(getwd(), "/loadtest.Rmd"),
        width = "75%"),

       numericInput('num_concurrent',
        label = "Target number of concurrent connections:",
        value = 10,
        min = 1),

       uiOutput("num_total"),

       numericInput('num_baseline',
        label = "Number of connections to run sequentially during the baseline test:",
        min = 3,
        value = 10),

       checkboxInput('stagger',
        label = "Include stagger to prevent identically concurrent actions? (Recommended)",
        value = TRUE)
      ),
    miniTabPanel("Test Summary & Run", icon = icon("play"),
     tableOutput("test_summary"),
     actionButton("run", "Run")
    )
   )
  )

  server <- function(input, output) {
  # Gather Dynamic Inputs
    output$test_file <- renderUI({
      textInput("test_file",
              label = "Test file:",
              value = ifelse(is.null(guessTestFile(input$app_file)),
                        'No tests found. Try shinytest::recordTest(load_mode = TRUE)',
                        guessTestFile(input$app_file)),
              width = "75%")
    })

    output$app_url <- renderUI({
      textInput("app_url",
              label = paste0(input$app_file, " is accessible at (URL):"),
              value = ifelse(is.null(guessAppURL(input$app_file)),
                        "No URL found",
                        guessAppURL(input$app_file)),
              width = "75%")
    })

    output$num_total <- renderUI({
      numericInput('num_total',
                 label = "Total number of connections during load test:",
                 min = input$num_concurrent,
                 value = input$num_concurrent)
    })


    # Generate Summary Table
    output$test_summary <- renderTable(colnames = FALSE, {
      validate(
        need(isURL(input$app_url), 'Need deployed URL!'),
        need(isRFile(input$test_file), message = "Test file must exist and be a .R file!")
      )
      data.frame(
        Field = c("Num. Concurrent", "Total", "Baseline", "Est. Time (mins)", "URL"),
        Value = c(input$num_concurrent, input$num_total, input$num_baseline, test_dur_min(), input$app_url)
      )
    })

    # Some helper reactives
    test_dur_min <- reactive({
      single_sec <- guessTestDuration(input$test_file) +                    # run duration
        input$shiny_load_sec                                                # load time

      total <- single_sec * ceiling(input$num_concurrent/input$num_total) + # concurrent test
        inferred_timeout() +                                                # phantom startup
        single_sec * input$num_baseline

      total_min <- total / 60

      total_min
    })

    inferred_timeout <- reactive({
      guessTimeout(input$num_concurrent, input$shiny_load_sec)
    })

    # Run render and load test
    observeEvent(input$run, {

      ## Validate some inputs
      if (!isURL(req(input$app_url))) {
        showNotification('Need deployed URL!')
        return(NULL)
      }

      if (!isRFile(req(input$app_file))) {
        showNotification("App file must exist and be a .R file!")
        return(NULL)
      }

      if (!isRFile(req(input$test_file))) {
        showNotification("Test file must exist and be a .R file!")
        return(NULL)
      }

      if (!isRmd(req(input$output_file))) {
        showNotification("Ouput file must include .Rmd extension! Cancelling ... ")
        return(NULL)
      }

      showNotification("Running Report ... ", duration = test_dur_min()*60)

      ## create a report Rmd from template
      output_dir <- dirname(input$output_file)
      output_file <- basename(input$output_file)
      createLoadTestReport(output_dir, output_file)

      ## create parameter list
      params <- list(url = input$app_url,
                     concurrent = input$num_concurrent,
                     total = input$num_total,
                     baseline = input$num_baseline,
                     testFile = input$test_file,
                     stagger = ifelse(input$stagger, guessStagger(inferred_timeout()), 0),
                     loadTimeout = inferred_timeout(),
                     phantomTimeout = inferred_timeout())

      ## render document (how to smartly handle errors?)
      setwd(output_dir)
      rmarkdown::render(input$output_file, params = params, envir = new.env())
      rstudioapi::viewer(input$output_file)
      ## return report
      stopApp(returnValue = list(file  = input$output_file, params = params))
    })
  }
  runGadget(ui, server)
}



#' Guess Test Duration
#'
#' @param test_file full path and filename for .R file containing the load test
#'   script, generated from \code{shinytest::recordTest(load_mode = TRUE)}.
#' @return Expected run time for a single connection in seconds.
#' @details Parses the \code{test_file} to determine the run time for a single connection.
#' @export
guessTestDuration <- function(test_file){
  if (!isRFile(test_file)) {
    return(NULL)
  }
  test_file <- normalizePath(test_file)
  file <- readLines(test_file)

  ## extract timeout information
  ## convert to seconds
  ## and divide by 3 (to undo the 3x safety factor added by the recorder)
  timeout <- regexec(file, pattern = "timeout_ = (\\d+)")
  load <- unlist(regmatches(file, timeout))
  loading_sec <- as.numeric(load[seq(from = 2, to = length(load), by = 2)]) / 1000 / 3

  ## extract sleep time
  sys_sleep <- regexec(file, pattern = "Sys.sleep\\((\\d+\\.\\d+)\\)")
  sleep <- unlist(regmatches(file, sys_sleep))
  sleep_sec <- as.numeric(sleep[seq(from = 2, to = length(sleep), by = 2)])

  sum(loading_sec) + sum(sleep_sec)
 }





## "is" Functions ----------------

isRmd <- function(output_file) {
  grepl(pattern = ".*\\.[Rr]md$", output_file)
}

isRFile <- function(test_file) {
  grepl(pattern = ".*\\.[Rr]$", x = test_file) && file.exists(test_file)
}

## tests to see if the current document context
## is a shiny app
isShinyApp <- function() {
  dc <- rstudioapi::getActiveDocumentContext()
  c <- dc$contents
  shiny <- sum(grepl(c, pattern = "shinyApp"))
  shiny_rmd <- sum(grepl(c, pattern = "runtime:\\s*shiny"))

  shiny | shiny_rmd
}

isURL <- function(app_url){
  grepl(app_url, pattern = "http[s]*:") &&
    !grepl(app_url, pattern = "No URL found", fixed = TRUE)
}

## non-exported "guess" Functions ----------------

## given an app file, scans the directory
## looking for a test sub directory
guessTestFile <- function(app_file) {

  if (!isRFile(app_file)) {
    return(NULL)
  }

  tryCatch({
    dir <- dirname(app_file)
  }, error = function(e) {return(NULL)}
  )

  subdir <- list.dirs(dir, full.names = FALSE, recursive = FALSE)

  curdir <- getwd()
  on.exit(setwd(curdir))

  if (sum(grepl(x = subdir, pattern = "tests"))) {
    setwd(paste0(dir, "/tests"))
    f <- list.files()
    is_r <- grepl(x = f, pattern = "\\.[Rr]$")
    test_file <- normalizePath(paste(dir, "tests", f[is_r][1], sep = "/"))
  } else {
    test_file <- NULL
  }

  test_file
}


## given an app file, scans the rsconnect
## folder to guess the url
guessAppURL <- function(app_file) {
  if (!isRFile(app_file)) {
    return(NULL)
  }

  tryCatch({
    dir <- dirname(app_file)
  }, error = function(e) {return(NULL)
  })

  subdir <- list.dirs(dir, full.names = FALSE, recursive = FALSE)

  curdir <- getwd()
  on.exit(setwd(curdir))

  if (sum(grepl(x = subdir, pattern = "rsconnect"))) {

    ## navigate through rsconnect hiearchy
    setwd(paste0(dir, "/rsconnect"))
    subdir <- list.dirs(full.names = TRUE)

    ## always go to the first server subdirectory of rsconnect
    ## and the first account directory i.e.
    ## app_dir
    ## - + server
    ## - - + account
    ## which is listed as the 3rd overally subdirectorys
    setwd(subdir[3])

    ## parse dcf file for url
    f <- list.files()
    is_dcf <- grepl(x = f, pattern = "\\.dcf$")
    dcf <- readLines(f[is_dcf][1])
    url <- unlist(regmatches(dcf, regexec("url:\\s(.*)$", dcf)))[2]

  } else {
    url <- NULL
  }

  url
}

## Simple hueristic to add random stagger
guessStagger <- function(test_time_sec) {
  max(5, 1/4 * test_time_sec)
}

#' Infer Timeouts
#'
#' @details Uses a hueristic to set phantomJS timeout and shiny load timeout
#'   based on the number of concurrent connections
#'
#' @param num_concurrent number of concurrent connections
guessTimeout <- function(num_concurrent, default = 15) {
  scale <- 1
  if (num_concurrent > 10)
    scale <- 2
  if (num_concurrent > 15)
    scale <- 3
  if (num_concurrent > 20)
    scale <- 4
  max(default * scale, 10)
}
