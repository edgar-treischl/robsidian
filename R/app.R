#' MD Creator App
#'
#' @returns NULL
#' @export
#'
md_creator <- function() {
  # Run the app
  shiny::shinyApp(
    ui = create_apui(),
    server = create_appserver()
  )
}


# Constants
DOCUMENTATION_CHOICES <- c(
  "Code only (level 0)" = 0,
  "Code with chunk headers (level 1)" = 1,
  "Code with all text as roxygen comments (level 2)" = 2
)

# Utility Functions --------------------------------------------------------

# Safely read file contents
safe_read_file <- function(file_path) {
  tryCatch({
    content <- base::readLines(file_path, warn = FALSE, encoding = "UTF-8")
    if (length(content) == 0) {
      content <- base::readLines(file_path, warn = FALSE)
    }
    content
  }, error = function(e) {
    raw_content <- base::readBin(file_path, "raw", n = base::file.info(file_path)$size)
    content <- base::rawToChar(raw_content)
    base::strsplit(content, "\n")[[1]]
  })
}

# Extract YAML header from content
extract_yaml <- function(content) {
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

# Process R file
process_r_file <- function(file_path, preserve_yaml, doc_level) {
  tryCatch({
    r_content <- safe_read_file(file_path)
    r_content <- r_content[!is.na(r_content)]

    if (doc_level == 2) {
      # Convert roxygen comments to text
      r_content <- base::gsub("^#' ", "", r_content)
    } else if (doc_level == 1) {
      # Keep chunk headers
      r_content <- base::gsub("^#' @", "", r_content)
    }

    basic_md <- r_content  # Don't wrap in triple backticks
    temp_r <- base::tempfile(fileext = ".R")
    base::writeLines(r_content, temp_r, useBytes = TRUE)

    rmd_content <- tryCatch({
      temp_spin <- knitr::spin(temp_r, knit = FALSE, format = "Rmd", comment = c("^#+'[ ]?", "^#+'[ ]?"))
      base::readLines(temp_spin, warn = FALSE)
    }, error = function(e) {
      basic_md
    })

    temp_output <- base::tempfile(fileext = ".md")

    tryCatch({
      temp_rmd <- base::tempfile(fileext = ".Rmd")
      base::writeLines(rmd_content, temp_rmd)
      rmarkdown::render(temp_rmd, output_format = rmarkdown::md_document(variant = "gfm"), output_file = temp_output, quiet = TRUE)
      content <- base::readLines(temp_output, warn = FALSE)
    }, error = function(e) {
      content <- basic_md
    })

    return(list(content = content, yaml = NULL))
  }, error = function(e) {
    r_content <- safe_read_file(file_path)
    return(list(content = r_content, yaml = NULL))
  })
}

# Process Rmd file
process_rmd_file <- function(file_path, preserve_yaml) {
  tryCatch({
    original_content <- safe_read_file(file_path)
    yaml_result <- extract_yaml(original_content)

    yaml_header <- if (preserve_yaml && !is.null(yaml_result$yaml)) {
      yaml_result$yaml
    } else {
      NULL
    }

    temp_rmd <- base::tempfile(fileext = ".Rmd")
    base::writeLines(yaml_result$content, temp_rmd)
    temp_output <- base::tempfile(fileext = ".md")

    tryCatch({
      rmarkdown::render(temp_rmd, output_format = rmarkdown::md_document(variant = "gfm"), output_file = temp_output, quiet = TRUE)
      content <- base::readLines(temp_output, warn = FALSE)
    }, error = function(e) {
      content <- yaml_result$content
    })

    return(list(content = content, yaml = yaml_header))
  }, error = function(e) {
    content <- safe_read_file(file_path)
    return(list(content = content, yaml = NULL))
  })
}

create_apui <- function() {
  ui <- bslib::page_sidebar(
    title = "Obsidian Markdown Creator",
    shinyjs::useShinyjs(),
    sidebar = bslib::sidebar(
      shiny::fileInput("file", "Upload R or Rmd file", accept = c(".R", ".Rmd", ".rmd")),
      bslib::tooltip(
        shiny::selectInput("documentation", "Documentation Level", choices = DOCUMENTATION_CHOICES, selected = 1),
        "Level 0: Pure code only\nLevel 1: Code with chunk headers\nLevel 2: Code with all text as roxygen comments"
      ),
      shiny::checkboxInput("preserve_yaml", "Preserve YAML Header (for Rmd files)", value = TRUE),
      shiny::actionButton("convert", "Convert", class = "btn-primary w-100"),
      shiny::hr(),
      shiny::downloadButton("downloadMD", "Download Markdown", class = "w-100"),
      shiny::actionButton("createNew", "Open in RStudio", class = "btn-primary mt-2 w-100", icon = shiny::icon("r-project"))
    ),
    bslib::card(
      bslib::card_header(
        shiny::div(
          class = "d-flex justify-content-between align-items-center",
          "Preview",
          shiny::div(
            style = "display: inline-flex; gap: 10px; align-items: center;",
            shiny::span(shiny::textOutput("conversionStatus", inline = TRUE)),
            shiny::actionButton("copy", "Copy", icon = shiny::icon("copy"), class = "btn-sm btn-outline-primary")
          )
        )
      ),
      bslib::card_body(
        shiny::verbatimTextOutput("preview")
      )
    )
  )
}

create_appserver <- function() {
  server <- function(input, output, session) {
    rv <- shiny::reactiveValues(
      content = NULL,
      yaml = NULL,
      processing = FALSE,
      last_conversion = NULL
    )

    shiny::observeEvent(input$file, {
      rv$yaml <- NULL
      if (!is.null(input$file)) {
        shinyjs::click("convert")
      }
    })

    shiny::observeEvent(input$convert, {
      shiny::req(input$file)
      rv$processing <- TRUE
      rv$last_conversion <- NULL

      shiny::withProgress(message = 'Processing file...', value = 0, {
        tryCatch({
          file_path <- input$file$datapath
          file_ext <- tools::file_ext(input$file$name)

          shiny::incProgress(0.3)

          doc_level <- input$documentation

          result <- if (tolower(file_ext) == "r") {
            process_r_file(file_path, input$preserve_yaml, doc_level)
          } else if (tolower(file_ext) %in% c("rmd", "md")) {
            process_rmd_file(file_path, input$preserve_yaml)
          }

          shiny::incProgress(0.3)

          if (!is.null(result$content)) {
            rv$content <- base::paste(result$content, collapse = "\n")
            rv$yaml <- result$yaml
            rv$last_conversion <- base::Sys.time()
          } else {
            shiny::showNotification("Error: Empty content returned", type = "error")
          }

          shiny::incProgress(0.4)

        }, error = function(e) {
          shiny::showNotification(base::paste("Error processing file:", e$message), type = "error", duration = NULL)
        })
      })

      rv$processing <- FALSE
    })

    output$conversionStatus <- shiny::renderText({
      if (rv$processing) {
        "Converting..."
      } else if (!is.null(rv$last_conversion)) {
        base::format(rv$last_conversion, "Last converted: %H:%M:%S")
      } else {
        ""
      }
    })

    output$preview <- shiny::renderText({
      shiny::req(rv$content)
      rv$content
    })

    shiny::observeEvent(input$copy, {
      shiny::req(rv$content)
      tryCatch({
        content_to_copy <- rv$content
        clipr::write_clip(content_to_copy)
        shiny::showNotification("Content copied to clipboard!", type = "message")
      }, error = function(e) {
        shiny::showNotification("Failed to copy to clipboard", type = "error")
      })
    })

    output$downloadMD <- shiny::downloadHandler(
      filename = function() {
        base::sub("\\.[^.]+$", ".md", input$file$name)
      },
      content = function(file) {
        content_to_save <- rv$content
        base::writeLines(content_to_save, file)
      }
    )

    shiny::observeEvent(input$createNew, {
      shiny::req(rv$content)
      if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        rstudioapi::documentNew(text = rv$content, type = "r")
      } else {
        shiny::showNotification("RStudio API not available", type = "warning")
      }
    })
  }
}

