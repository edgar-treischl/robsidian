library(shiny)
library(bslib)
library(knitr)
library(clipr)
library(rmarkdown)
library(shinyjs)

# Constants
DOCUMENTATION_CHOICES <- c(
  "Code only (level 0)" = 0,
  "Code with chunk headers (level 1)" = 1,
  "Code with all text as roxygen comments (level 2)" = 2
)

# Utility Functions --------------------------------------------------------

#' Safely read file contents
#' @param file_path Path to file
#' @return Character vector of file content
safe_read_file <- function(file_path) {
  tryCatch({
    # Try reading with UTF-8 encoding first
    content <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
    if (length(content) == 0) {
      content <- readLines(file_path, warn = FALSE)  # Try system default encoding
    }
    content
  }, error = function(e) {
    # If all else fails, read raw and convert
    raw_content <- readBin(file_path, "raw", n = file.info(file_path)$size)
    content <- rawToChar(raw_content)
    strsplit(content, "\n")[[1]]
  })
}

#' Extract YAML header from content
#' @param content Character vector of file content
#' @return List with yaml and content components
utils_extract_yaml <- function(content) {
  yaml_lines <- NULL
  if (length(which(content == "---")) >= 2) {
    yaml_start <- which(content == "---")[1]
    yaml_end <- which(content == "---")[2]
    if (!is.na(yaml_start) && !is.na(yaml_end) && yaml_end > yaml_start) {
      yaml_lines <- content[yaml_start:yaml_end]
      content <- content[(yaml_end + 1):length(content)]
    }
  }
  return(list(yaml = yaml_lines, content = content))
}

#' Process R file
#' @param file_path Path to R file
#' @param preserve_yaml Boolean to preserve YAML
#' @return List with content and yaml components
utils_process_r_file <- function(file_path, preserve_yaml) {
  tryCatch({
    # Safely read the file
    r_content <- safe_read_file(file_path)

    # Remove any NULL or NA entries
    r_content <- r_content[!is.na(r_content)]

    # Create a basic markdown version as fallback
    basic_md <- c("```r", r_content, "```")

    # Try to process with spin
    temp_r <- tempfile(fileext = ".R")
    writeLines(r_content, temp_r, useBytes = TRUE)

    rmd_content <- tryCatch({
      temp_spin <- knitr::spin(temp_r, knit = FALSE, format = "Rmd",
                               comment = c("^#+'[ ]?", "^#+'[ ]?"))
      readLines(temp_spin, warn = FALSE)
    }, error = function(e) {
      basic_md  # Return basic markdown if spin fails
    })

    # Set up YAML header
    yaml_header <- c("---", "output: github_document", "---")

    # Try to render to markdown
    temp_output <- tempfile(fileext = ".md")

    tryCatch({
      # Create temporary Rmd
      temp_rmd <- tempfile(fileext = ".Rmd")
      writeLines(c(yaml_header, "", rmd_content), temp_rmd)

      # Render to markdown
      rmarkdown::render(
        temp_rmd,
        output_format = rmarkdown::md_document(variant = "gfm"),
        output_file = temp_output,
        quiet = TRUE
      )

      # Read the rendered content
      content <- readLines(temp_output, warn = FALSE)

    }, error = function(e) {
      # If rendering fails, use basic markdown
      content <- basic_md
    })

    return(list(
      content = content,
      yaml = yaml_header
    ))

  }, error = function(e) {
    # Ultimate fallback: return the file content as plain code block
    r_content <- safe_read_file(file_path)
    return(list(
      content = c("```r", r_content, "```"),
      yaml = c("---", "output: github_document", "---")
    ))
  })
}

#' Process Rmd file
#' @param file_path Path to Rmd file
#' @param preserve_yaml Boolean to preserve YAML
#' @return List with content and yaml components
utils_process_rmd_file <- function(file_path, preserve_yaml) {
  tryCatch({
    # Safely read the file
    original_content <- safe_read_file(file_path)

    # Extract YAML and handle empty content
    yaml_result <- utils_extract_yaml(original_content)

    yaml_header <- if (preserve_yaml && !is.null(yaml_result$yaml)) {
      yaml_result$yaml
    } else {
      c("---", "output: github_document", "---")
    }

    # Create temporary Rmd for rendering
    temp_rmd <- tempfile(fileext = ".Rmd")
    writeLines(c(yaml_header, "", yaml_result$content), temp_rmd)

    # Try to render to markdown
    temp_output <- tempfile(fileext = ".md")
    tryCatch({
      rmarkdown::render(
        temp_rmd,
        output_format = rmarkdown::md_document(variant = "gfm"),
        output_file = temp_output,
        quiet = TRUE
      )
      content <- readLines(temp_output, warn = FALSE)
    }, error = function(e) {
      # If rendering fails, use content as-is
      content <- yaml_result$content
    })

    return(list(
      content = content,
      yaml = yaml_header
    ))

  }, error = function(e) {
    # Ultimate fallback: return the file content as-is
    content <- safe_read_file(file_path)
    return(list(
      content = content,
      yaml = c("---", "output: github_document", "---")
    ))
  })
}

ui <- page_sidebar(
  title = "Obsidian Markdown Creator",
  useShinyjs(),
  sidebar = sidebar(
    fileInput("file", "Upload R or Rmd file",
              accept = c(".R", ".Rmd", ".rmd")),
    tooltip(
      selectInput("documentation", "Documentation Level",
                  choices = DOCUMENTATION_CHOICES,
                  selected = 1),
      "Level 0: Pure code only\nLevel 1: Code with chunk headers\nLevel 2: Code with all text as roxygen comments"
    ),
    checkboxInput("preserve_yaml", "Preserve YAML Header (for Rmd files)",
                  value = TRUE),
    actionButton("convert", "Convert", class = "btn-primary w-100"),
    hr(),
    downloadButton("downloadMD", "Download Markdown", class = "w-100"),
    actionButton("createNew", "Open in RStudio",
                 class = "btn-primary mt-2 w-100",
                 icon = icon("r-project"))
  ),

  card(
    card_header(
      div(
        class = "d-flex justify-content-between align-items-center",
        "Preview",
        div(
          style = "display: inline-flex; gap: 10px; align-items: center;",
          span(textOutput("conversionStatus", inline = TRUE)),
          actionButton("copy", "Copy", icon = icon("copy"),
                       class = "btn-sm btn-outline-primary")
        )
      )
    ),
    card_body(
      verbatimTextOutput("preview")
    )
  )
)

server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(
    content = NULL,
    yaml = NULL,
    processing = FALSE,
    last_conversion = NULL
  )

  # Auto-convert when file is uploaded
  observeEvent(input$file, {
    rv$yaml <- NULL
    if (!is.null(input$file)) {
      shinyjs::click("convert")
    }
  })

  # Process file when convert button is clicked
  observeEvent(input$convert, {
    req(input$file)

    # Set processing state
    rv$processing <- TRUE
    rv$last_conversion <- NULL

    withProgress(message = 'Processing file...', value = 0, {
      tryCatch({
        file_path <- input$file$datapath
        file_ext <- tools::file_ext(input$file$name)

        incProgress(0.3)

        result <- if (tolower(file_ext) == "r") {
          utils_process_r_file(file_path, input$preserve_yaml)
        } else if (tolower(file_ext) %in% c("rmd", "md")) {
          utils_process_rmd_file(file_path, input$preserve_yaml)
        }

        incProgress(0.3)

        if (!is.null(result$content)) {
          rv$content <- paste(result$content, collapse = "\n")
          rv$yaml <- result$yaml
          rv$last_conversion <- Sys.time()
        } else {
          showNotification("Error: Empty content returned", type = "error")
        }

        incProgress(0.4)

      }, error = function(e) {
        showNotification(paste("Error processing file:", e$message),
                         type = "error",
                         duration = NULL)
      })
    })

    # Reset processing state
    rv$processing <- FALSE
  })

  # Conversion status output
  output$conversionStatus <- renderText({
    if (rv$processing) {
      "Converting..."
    } else if (!is.null(rv$last_conversion)) {
      format(rv$last_conversion, "Last converted: %H:%M:%S")
    } else {
      ""
    }
  })

  # Preview output
  output$preview <- renderText({
    req(rv$content)
    if (!is.null(rv$yaml)) {
      paste(c(paste(rv$yaml, collapse = "\n"), "", rv$content), collapse = "\n")
    } else {
      rv$content
    }
  })

  # Copy button handler
  observeEvent(input$copy, {
    req(rv$content)
    tryCatch({
      content_to_copy <- if (!is.null(rv$yaml)) {
        paste(c(paste(rv$yaml, collapse = "\n"), "", rv$content), collapse = "\n")
      } else {
        rv$content
      }
      clipr::write_clip(content_to_copy)
      showNotification("Content copied to clipboard!", type = "message")
    }, error = function(e) {
      showNotification("Failed to copy to clipboard", type = "error")
    })
  })

  # Download handler
  # Download handler
  output$downloadMD <- downloadHandler(
    filename = function() {
      sub("\\.[^.]+$", ".md", input$file$name)
    },
    content = function(file) {
      content_to_save <- if (!is.null(rv$yaml)) {
        paste(c(paste(rv$yaml, collapse = "\n"), "", rv$content), collapse = "\n")
      } else {
        rv$content
      }
      writeLines(content_to_save, file)
    }
  )

  # Create new document in RStudio
  observeEvent(input$createNew, {
    req(rv$content)
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      content_to_open <- if (!is.null(rv$yaml)) {
        paste(c(paste(rv$yaml, collapse = "\n"), "", rv$content), collapse = "\n")
      } else {
        rv$content
      }
      rstudioapi::documentNew(text = content_to_open, type = "r")
    } else {
      showNotification("RStudio API not available", type = "warning")
    }
  })
}

# Run the app
shinyApp(ui, server)
