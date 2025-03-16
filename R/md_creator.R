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
  "Extract (Roxygen) comments" = 1
)


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


# Process R file
process_r_file <- function(file_path, doc_level, include_output = FALSE) {
  tryCatch({
    # Read the R code content
    r_content <- safe_read_file(file_path)
    r_content <- r_content[!is.na(r_content)]  # Remove any NA values

    if (doc_level == 0) {
      # Simply wrap everything in an R code block
      formatted_content <- c("```r", r_content, "```")
      return(list(content = formatted_content, yaml = NULL))
    }
    else if (doc_level == 1) {
      # Code that extracts roxygen comments

      # Find first non-empty, non-comment line (code start)
      code_start_pattern <- "^\\s*[^#\\s]"
      roxygen_pattern <- "^\\s*#'"

      # Start with empty collections
      formatted_content <- character(0)
      actual_code <- character(0)

      # Check if there are roxygen comments at the start
      has_roxygen <- any(grepl(roxygen_pattern, r_content))

      if (has_roxygen) {
        # Extract roxygen comments and convert to markdown
        roxygen_lines <- grep(roxygen_pattern, r_content, value = TRUE)

        # Process roxygen comments to markdown using the new function
        md_lines <- convert_roxygen_to_markdown(roxygen_lines)
        # md_lines <- gsub(roxygen_pattern, "", roxygen_lines)
        # md_lines <- trimws(md_lines)

        # Extract title and add to formatted content
        if (length(md_lines) > 0) {
          title_line <- md_lines[1]
          md_lines <- md_lines[-1]  # Remove title from remaining content

          # Add title as H1
          formatted_content <- c(formatted_content, paste("# ", title_line), "")
        }

        # Add remaining roxygen content
        if (length(md_lines) > 0) {
          formatted_content <- c(formatted_content, md_lines, "")
        }

        # Find the first code line after roxygen comments
        roxygen_indices <- grep(roxygen_pattern, r_content)
        if (length(roxygen_indices) > 0) {
          last_roxygen <- max(roxygen_indices)
          if (last_roxygen < length(r_content)) {
            # Extract actual code (after roxygen)
            actual_code <- r_content[(last_roxygen+1):length(r_content)]

            # Remove any empty lines at the start
            while (length(actual_code) > 0 && trimws(actual_code[1]) == "") {
              actual_code <- actual_code[-1]
            }
          }
        }
      } else {
        # No roxygen comments, treat all content as code
        actual_code <- r_content
      }

      # Process normal comments after code
      after_comments <- character(0)

      # Add code section with or without output
      if (length(actual_code) > 0) {
        # THIS IS THE KEY CHANGE: We need to check include_output here
        formatted_content <- c(formatted_content, "```r", actual_code, "```")
      }

      # Add comments that appear after code
      if (length(after_comments) > 0) {
        formatted_content <- c(formatted_content, "", after_comments)
      }

      return(list(content = formatted_content, yaml = NULL))
    }

  }, error = function(e) {
    # Fallback: return the original content in a code block
    r_content <- safe_read_file(file_path)
    formatted_content <- c("```r", r_content, "```")
    return(list(content = formatted_content, yaml = NULL))
  })
}





# Create the app UI


create_apui <- function() {
  ui <- bslib::page_sidebar(
    title = "R to MD",
    shinyjs::useShinyjs(),
    sidebar = bslib::sidebar(
      shiny::fileInput("file", "Upload R file", accept = c(".R")),
      shiny::selectInput("documentation", "Documentation Level", choices = DOCUMENTATION_CHOICES, selected = 1),
      #shiny::checkboxInput("include_output", "Include R chunk output", value = FALSE),
      shiny::actionButton("convert", "Convert", class = "btn-primary w-100"),
      shiny::hr(),
      shiny::downloadButton("downloadMD", "Download MD", class = "w-100"),
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

# Create the app server
create_appserver <- function() {
  function(input, output, session) {
    rv <- shiny::reactiveValues(
      content = "",
      yaml = NULL,
      processing = FALSE,
      last_conversion = NULL
    )

    # Convert button action
    shiny::observeEvent(input$convert, {
      shiny::req(input$file)
      rv$processing <- TRUE
      rv$last_conversion <- NULL

      # Show a message if including output
      # if(input$include_output) {
      #   shiny::showNotification("Executing R code and capturing output. This may take a moment...",
      #                           type = "message", duration = 3)
      # }

      shiny::withProgress(message = 'Processing file...', value = 0, {
        tryCatch({
          file_path <- input$file$datapath
          file_ext <- tools::file_ext(input$file$name)

          shiny::incProgress(0.3)

          doc_level <- as.numeric(input$documentation)
          include_output <- input$include_output

          # Only process R files
          if (tolower(file_ext) == "r") {
            result <- process_r_file(file_path, doc_level, include_output)

            shiny::incProgress(0.3)

            if (!is.null(result$content)) {
              rv$content <- base::paste(result$content, collapse = "\n")
              rv$yaml <- result$yaml
              rv$last_conversion <- base::Sys.time()
            } else {
              shiny::showNotification("Error: Empty content returned", type = "error")
            }
          } else {
            shiny::showNotification("Only R files are supported", type = "error")
          }

          shiny::incProgress(0.4)

        }, error = function(e) {
          shiny::showNotification(base::paste("Error processing file:", e$message), type = "error", duration = NULL)
        })
      })

      rv$processing <- FALSE
    })

    # Preview output
    output$preview <- shiny::renderText({
      if (rv$content == "") {
        return("Upload an R file and click 'Convert' to see a preview.")
      }
      return(rv$content)
    })

    # Conversion status
    output$conversionStatus <- shiny::renderText({
      if (is.null(rv$last_conversion)) {
        return("")
      }
      return(paste("Last converted:", format(rv$last_conversion, "%H:%M:%S")))
    })

    # Copy button action
    shiny::observeEvent(input$copy, {
      if (rv$content != "") {
        clipr::write_clip(rv$content)
        shiny::showNotification("Content copied to clipboard", type = "message")
      }
    })

    # Download handler
    output$downloadMD <- shiny::downloadHandler(
      filename = function() {
        orig_name <- tools::file_path_sans_ext(input$file$name)
        paste0(orig_name, ".md")
      },
      content = function(file) {
        if (rv$content != "") {
          writeLines(rv$content, file)
        } else {
          writeLines("No content to download. Convert a file first.", file)
        }
      }
    )

    # Create new file in RStudio
    # Create new file in RStudio
    shiny::observeEvent(input$createNew, {
      if (rv$content != "") {
        tryCatch({
          if (rstudioapi::isAvailable()) {
            orig_name <- tools::file_path_sans_ext(input$file$name)

            # Use "rmarkdown" instead of "md" for the type parameter
            new_doc <- rstudioapi::documentNew(
              text = rv$content,
              type = "rmarkdown"  # Changed from "md" to "rmarkdown"
            )

            shiny::showNotification("File opened in RStudio as an R Markdown document. Use 'Save As' to save it with .md extension.",
                                    type = "message")
          } else {
            shiny::showNotification("RStudio API not available", type = "warning")
          }
        }, error = function(e) {
          shiny::showNotification(
            paste("Error creating file in RStudio:", e$message),
            type = "error"
          )
        })
      } else {
        shiny::showNotification("No content to open. Convert a file first.", type = "warning")
      }
    })
  }
}


# Convert roxygen comments to Markdown format
convert_roxygen_to_markdown <- function(roxygen_lines) {
  md_lines <- character(0)

  for (i in seq_along(roxygen_lines)) {
    line <- roxygen_lines[i]
    # Remove the roxygen prefix (#')
    clean_line <- gsub("^\\s*#'\\s*", "", line)

    # Convert @param to "- param: "
    if (grepl("^@param", clean_line)) {
      # Format: @param name description -> - param: name description
      param_content <- sub("^@param\\s+", "", clean_line)
      md_lines <- c(md_lines, paste0("- param: ", param_content))
    }
    # Convert @returns/@return to "- returns: "
    else if (grepl("^@returns?", clean_line)) {
      # Format: @returns description -> - returns: description
      return_desc <- sub("^@returns?\\s+", "", clean_line)
      md_lines <- c(md_lines, paste0("- returns: ", return_desc))
    }
    # Other roxygen tags - remove @ and format as list items
    else if (grepl("^@", clean_line)) {
      # Extract the tag name without @
      tag_name <- sub("^@([a-zA-Z0-9_]+)\\s+.*$", "\\1", clean_line)
      tag_content <- sub("^@[a-zA-Z0-9_]+\\s+", "", clean_line)
      md_lines <- c(md_lines, paste0("- ", tag_name, ": ", tag_content))
    }
    # Title or normal text
    else {
      md_lines <- c(md_lines, clean_line)
    }
  }

  return(md_lines)
}
