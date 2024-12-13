#' The Obsidian Addin
#'
#' @description The `obsidian_addin()` creates a shiny app
#' to manage your gists. Pick a gist, press the copy or the insert
#' button and the code will be insert.
#' Furthermore you can create and delete gists as well.
#'
#' @export
#' @import shiny
#' @import htmltools

obsidian_addin <- function() {
  ui <- bslib::page_sidebar(
    title = tags$a(
      href = "#",
      onclick = "Shiny.setInputValue('reset_to_readme', Math.random())",
      style = "text-decoration: none; color: inherit; cursor: pointer;",
      "Doc"
    ),
    fillable = TRUE,
    padding = 0,

    tags$head(
      # CSS and JS imports
      tags$link(rel = "stylesheet",
                href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/styles/default.min.css"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/highlight.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/languages/r.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/languages/python.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/languages/sql.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/languages/bash.min.js"),

      # Custom CSS styles
      tags$style("
      .folder { font-weight: bold; color: #666; }
      ul {
        list-style-type: none;
        padding-left: 15px !important;
        margin: 0;
      }
      li {
        margin: 2px 0;
        font-size: 0.9rem;
      }
      a { text-decoration: none; color: #337ab7; }
      a:hover { text-decoration: underline; }

      .code-container {
        position: relative;
        margin: 0.5em 0;
      }
      .copy-button {
        position: absolute;
        top: 2px;
        right: 2px;
        padding: 2px 6px;
        background: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 3px;
        cursor: pointer;
        font-size: 0.8rem;
      }
      .copy-button:hover {
        background: #e9ecef;
      }
      pre {
        padding: 0.5em;
        background: #f8f9fa;
        border-radius: 3px;
        margin: 0.5em 0;
        font-size: 0.9rem;
        max-height: 400px;
        overflow-y: auto;
      }
      .sidebar {
        max-width: 250px !important;
        width: 250px !important;
      }
      .card-header {
        padding: 0.5rem 1rem !important;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      .card-body {
        padding: 0.5rem !important;
      }
      #pdf_download {
        padding: 2px 8px;
        font-size: 0.9rem;
        color: #666;
        background: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 3px;
        cursor: pointer;
        text-decoration: none;
      }
      #pdf_download:hover {
        background: #e9ecef;
        text-decoration: none;
      }
      #pdf_download.disabled {
        opacity: 0.5;
        cursor: not-allowed;
      }
      .navbar-toggle-action {
        padding: 4px 8px !important;
        margin-right: 8px !important;
        border: 1px solid #dee2e6 !important;
        border-radius: 4px !important;
        background: #f8f9fa !important;
      }
      .navbar-toggle-action:hover {
        background: #e9ecef !important;
      }
    "),

      # JavaScript functions
      tags$script("
      function copyCode(button) {
        const codeBlock = button.parentElement.querySelector('pre code');
        const text = codeBlock.textContent;

        navigator.clipboard.writeText(text).then(function() {
          const originalText = button.textContent;
          button.textContent = 'Copied!';
          setTimeout(function() {
            button.textContent = originalText;
          }, 2000);
        }).catch(function(err) {
          console.error('Failed to copy text: ', err);
        });
      }

      function addCopyButtons() {
        document.querySelectorAll('pre code').forEach(function(codeBlock) {
          const fenceInfo = codeBlock.textContent.split('\\n')[0];
          if (fenceInfo.startsWith('```')) {
            const lang = fenceInfo.substring(3).trim();
            if (lang) {
              codeBlock.className = 'language-' + lang;
              codeBlock.textContent = codeBlock.textContent.split('\\n').slice(1).join('\\n');
            }
          }

          const container = document.createElement('div');
          container.className = 'code-container';

          const copyButton = document.createElement('button');
          copyButton.className = 'copy-button';
          copyButton.textContent = 'Copy';
          copyButton.onclick = function() { copyCode(this); };

          codeBlock.parentElement.parentNode.insertBefore(container, codeBlock.parentElement);
          container.appendChild(codeBlock.parentElement);
          container.insertBefore(copyButton, codeBlock.parentElement);
        });

        hljs.highlightAll();
      }
    ")
    ),

    # Sidebar
    sidebar = bslib::sidebar(
      width = 250,
      padding = 1,
      uiOutput("directory_tree")
    ),

    # Main content
    bslib::card(
      full_screen = TRUE,
      min_height = "300px",
      bslib::card_header(
        class = "p-2",
        div(
          class = "d-flex justify-content-between align-items-center w-100",
          div(
            class = "d-flex align-items-center gap-2",
            div(class = "navbar-toggle-action"),
            "File Preview"
          ),
          uiOutput("pdf_button")
        )
      ),
      htmlOutput("markdown_preview")
    )
  )



  # Server logic
  server <- function(input, output, session) {
    #setwd("~/Documents/GitHub/documentation")

    # Reactive value for selected file
    selected_file <- reactiveVal(NULL)

    # Handle title click and initial load
    observeEvent(input$reset_to_readme, {
      readme_path <- fs::path_abs("README.md")
      if (file.exists(readme_path)) {
        selected_file(readme_path)
      }
    }, ignoreNULL = FALSE, once = TRUE)  # This handles initial load

    # Handle subsequent title clicks
    observeEvent(input$reset_to_readme, {
      if (!is.null(input$reset_to_readme)) {  # Only handle actual clicks, not initial NULL
        readme_path <- fs::path_abs("README.md")
        if (file.exists(readme_path)) {
          selected_file(readme_path)
        }
      }
    })

    # Update selected file when clicked
    observeEvent(input$selected_file, {
      selected_file(input$selected_file)
    })

    # PDF creation function with error handling
    create_pdf <- function(md_file) {
      temp_dir <- tempdir()
      base_name <- tools::file_path_sans_ext(basename(md_file))
      pdf_path <- file.path(temp_dir, paste0(base_name, ".pdf"))

      create_error_pdf <- function(error_msg) {
        error_tex <- sprintf("\\documentclass{article}
        \\begin{document}
        \\section*{Error Creating PDF}
        \\textbf{The following error occurred while trying to create the PDF:}

        \\begin{verbatim}
        %s
        \\end{verbatim}
        \\end{document}", error_msg)

        error_tex_file <- file.path(temp_dir, "error.tex")
        writeLines(error_tex, error_tex_file)

        system2("pdflatex",
                args = c("-interaction=nonstopmode",
                         "-output-directory", temp_dir,
                         error_tex_file),
                stdout = FALSE)

        file.path(temp_dir, "error.pdf")
      }

      tryCatch({
        rmarkdown::render(md_file,
                          output_format = "pdf_document",
                          output_file = pdf_path,
                          quiet = TRUE)
        pdf_path
      }, error = function(e) {
        error_pdf <- create_error_pdf(as.character(e))
        if (file.exists(error_pdf)) {
          error_pdf
        } else {
          NULL
        }
      })
    }

    # PDF button UI
    output$pdf_button <- renderUI({
      if (!is.null(selected_file())) {
        downloadLink(
          "pdf_download",
          label = "PDF",
          class = "btn-pdf"
        )
      } else {
        tags$a(
          href = "#",
          class = "btn-pdf disabled",
          "PDF"
        )
      }
    })

    # PDF download handler
    output$pdf_download <- downloadHandler(
      filename = function() {
        req(selected_file())
        paste0(tools::file_path_sans_ext(basename(selected_file())), ".pdf")
      },
      content = function(file) {
        req(selected_file())
        pdf_path <- create_pdf(selected_file())
        if (!is.null(pdf_path)) {
          file.copy(pdf_path, file)
        } else {
          stop("Failed to create PDF")
        }
      }
    )

    # Directory tree builder
    build_directory_tree <- function(path, prefix = "") {
      items <- fs::dir_ls(path)
      tags$ul(
        lapply(items, function(item) {
          item_name <- fs::path_file(item)
          if (fs::dir_exists(item)) {
            tags$li(
              tags$span(class = "folder", item_name),
              build_directory_tree(item, paste0(prefix, "/", item_name))
            )
          } else if (tolower(fs::path_ext(item)) == "md") {
            tags$li(
              tags$a(
                href = "#",
                onclick = sprintf("Shiny.setInputValue('selected_file', '%s')",
                                  fs::path_abs(item)),
                item_name
              )
            )
          }
        })
      )
    }

    # Directory tree output
    output$directory_tree <- renderUI({
      build_directory_tree(".")
    })

    # Markdown preview output
    output$markdown_preview <- renderUI({
      req(selected_file())

      tryCatch({
        md_content <- readLines(selected_file(), warn = FALSE)
        md_content <- paste(md_content, collapse = "\n")

        # Process code blocks
        md_content <- gsub("```(\\w+)\n", "```\\1\n", md_content)

        html_content <- commonmark::markdown_html(md_content)

        tagList(
          HTML(html_content),
          tags$script("addCopyButtons();")
        )
      }, error = function(e) {
        HTML("<p class='text-danger'>Error reading or rendering the file.</p>")
      })
    })
  }

  shinyApp(ui = ui, server = server)
}


