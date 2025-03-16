#
#
# obsidian_addin <- function() {
#   ui <- bslib::page_sidebar(
#     title = tags$a(
#       href = "#",
#       onclick = "Shiny.setInputValue('reset_to_readme', Math.random())",
#       style = "text-decoration: none; color: inherit; cursor: pointer;",
#       "Obsedian Preview"
#     ),
#     fillable = TRUE,
#     padding = 0,
#
#     tags$head(
#       # CSS and JS imports
#       tags$link(rel = "stylesheet",
#                 href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/styles/default.min.css"),
#       tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/highlight.min.js"),
#       tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/languages/r.min.js"),
#       tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/languages/python.min.js"),
#       tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/languages/sql.min.js"),
#       tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.7.0/languages/bash.min.js"),
#
#       # Custom CSS styles
#       tags$style("
#       .folder { font-weight: bold; color: #666; }
#       ul {
#         list-style-type: none;
#         padding-left: 15px !important;
#         margin: 0;
#       }
#       li {
#         margin: 2px 0;
#         font-size: 0.9rem;
#       }
#       a { text-decoration: none; color: #337ab7; }
#       a:hover { text-decoration: underline; }
#
#       .code-container {
#         position: relative;
#         margin: 0.5em 0;
#       }
#       .copy-button {
#         position: absolute;
#         top: 2px;
#         right: 2px;
#         padding: 2px 6px;
#         background: #f8f9fa;
#         border: 1px solid #dee2e6;
#         border-radius: 3px;
#         cursor: pointer;
#         font-size: 0.8rem;
#       }
#       .copy-button:hover {
#         background: #e9ecef;
#       }
#       pre {
#         padding: 0.5em;
#         background: #f8f9fa;
#         border-radius: 3px;
#         margin: 0.5em 0;
#         font-size: 0.9rem;
#         max-height: 400px;
#         overflow-y: auto;
#       }
#       .sidebar {
#         max-width: 250px !important;
#         width: 250px !important;
#       }
#       .card-header {
#         padding: 0.5rem 1rem !important;
#         display: flex;
#         justify-content: space-between;
#         align-items: center;
#       }
#       .card-body {
#         padding: 0.5rem !important;
#       }
#       #pdf_download {
#         padding: 2px 8px;
#         font-size: 0.9rem;
#         color: #666;
#         background: #f8f9fa;
#         border: 1px solid #dee2e6;
#         border-radius: 3px;
#         cursor: pointer;
#         text-decoration: none;
#       }
#       #pdf_download:hover {
#         background: #e9ecef;
#         text-decoration: none;
#       }
#       #pdf_download.disabled {
#         opacity: 0.5;
#         cursor: not-allowed;
#       }
#       .navbar-toggle-action {
#         padding: 4px 8px !important;
#         margin-right: 8px !important;
#         border: 1px solid #dee2e6 !important;
#         border-radius: 4px !important;
#         background: #f8f9fa !important;
#       }
#       .navbar-toggle-action:hover {
#         background: #e9ecef !important;
#       }
#     "),
#
#       # JavaScript functions
#       tags$script("
#       function copyCode(button) {
#         const codeBlock = button.parentElement.querySelector('pre code');
#         const text = codeBlock.textContent;
#
#         navigator.clipboard.writeText(text).then(function() {
#           const originalText = button.textContent;
#           button.textContent = 'Copied!';
#           setTimeout(function() {
#             button.textContent = originalText;
#           }, 2000);
#         }).catch(function(err) {
#           console.error('Failed to copy text: ', err);
#         });
#       }
#
#       function addCopyButtons() {
#         document.querySelectorAll('pre code').forEach(function(codeBlock) {
#           const fenceInfo = codeBlock.textContent.split('\\n')[0];
#           if (fenceInfo.startsWith('```')) {
#             const lang = fenceInfo.substring(3).trim();
#             if (lang) {
#               codeBlock.className = 'language-' + lang;
#               codeBlock.textContent = codeBlock.textContent.split('\\n').slice(1).join('\\n');
#             }
#           }
#
#           const container = document.createElement('div');
#           container.className = 'code-container';
#
#           const copyButton = document.createElement('button');
#           copyButton.className = 'copy-button';
#           copyButton.textContent = 'Copy';
#           copyButton.onclick = function() { copyCode(this); };
#
#           codeBlock.parentElement.parentNode.insertBefore(container, codeBlock.parentElement);
#           container.appendChild(codeBlock.parentElement);
#           container.insertBefore(copyButton, codeBlock.parentElement);
#         });
#
#         hljs.highlightAll();
#       }
#     ")
#     ),
#
#     # Sidebar
#     sidebar = bslib::sidebar(
#       width = 250,
#       padding = 1,
#       uiOutput("directory_tree")
#     ),
#
#     # Main content
#     bslib::card(
#       full_screen = TRUE,
#       min_height = "300px",
#       bslib::card_header(
#         class = "p-2",
#         div(
#           class = "d-flex justify-content-between align-items-center w-100",
#           div(
#             class = "d-flex align-items-center gap-2",
#             div(class = "navbar-toggle-action"),
#             "Preview:",
#             textOutput("current_file", inline = TRUE)
#           ),
#           uiOutput("pdf_button")
#         )
#       ),
#       htmlOutput("markdown_preview")
#     )
#   )
#
#   # Server logic
#   server <- function(input, output, session) {
#     # Add resource path for images
#     observe({
#       shiny::addResourcePath("obsidian_vault", home())
#     })
#
#     output$current_file <- renderText({
#       req(selected_file())
#       # Get relative path from home directory
#       rel_path <- fs::path_rel(selected_file(), home())
#     })
#
#     # Store home directory in a reactive value with validation
#     home <- reactiveVal({
#       path <- Sys.getenv("robsidian_dir")
#       if (path == "") {
#         cli::cli_abort(
#           c(
#             "Which directory should be used? The `robsidian_dir` environment variable is not set.",
#             "i" = "You can set the environment variable by running:",
#             "    {cli::cli_code('Sys.setenv(robsidian_dir = \"/path/to/your/obsidian/vault\")')}"
#           )
#         )
#       }
#       norm_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
#       if (!dir.exists(norm_path)) {
#         stop("Directory specified in robsidian_dir does not exist: ", norm_path)
#       }
#       norm_path
#     })
#
#     # Reactive value for selected file
#     selected_file <- reactiveVal(NULL)
#
#     # Function to validate file paths
#     validate_path <- function(path) {
#       if (is.null(path)) return(FALSE)
#       tryCatch({
#         norm_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
#         startsWith(norm_path, home()) && file.exists(norm_path)
#       }, error = function(e) FALSE)
#     }
#
#     # Handle title click and initial load
#     observeEvent(input$reset_to_readme, {
#       readme_path <- file.path(home(), "README.md")
#       if (validate_path(readme_path)) {
#         selected_file(readme_path)
#       }
#     }, ignoreNULL = FALSE, once = TRUE)
#
#     # Handle subsequent title clicks
#     observeEvent(input$reset_to_readme, {
#       if (!is.null(input$reset_to_readme)) {
#         readme_path <- file.path(home(), "README.md")
#         if (validate_path(readme_path)) {
#           selected_file(readme_path)
#         }
#       }
#     })
#
#     # Update selected file when clicked
#     observeEvent(input$selected_file, {
#       if (!is.null(input$selected_file) && validate_path(input$selected_file)) {
#         selected_file(input$selected_file)
#       }
#     })
#
#     # PDF creation function with error handling
#     create_pdf <- function(md_file) {
#       temp_dir <- tempdir()
#       base_name <- tools::file_path_sans_ext(basename(md_file))
#       pdf_path <- file.path(temp_dir, paste0(base_name, ".pdf"))
#
#       create_error_pdf <- function(error_msg) {
#         error_tex <- sprintf("\\documentclass{article}
#       \\begin{document}
#       \\section*{Error Creating PDF}
#       \\textbf{The following error occurred while trying to create the PDF:}
#
#       \\begin{verbatim}
#       %s
#       \\end{verbatim}
#       \\end{document}", error_msg)
#
#         error_tex_file <- file.path(temp_dir, "error.tex")
#         writeLines(error_tex, error_tex_file)
#
#         system2("pdflatex",
#                 args = c("-interaction=nonstopmode",
#                          "-output-directory", temp_dir,
#                          error_tex_file),
#                 stdout = FALSE)
#
#         file.path(temp_dir, "error.pdf")
#       }
#
#       tryCatch({
#         rmarkdown::render(md_file,
#                           output_format = "pdf_document",
#                           output_file = pdf_path,
#                           quiet = TRUE)
#         pdf_path
#       }, error = function(e) {
#         error_pdf <- create_error_pdf(as.character(e))
#         if (file.exists(error_pdf)) {
#           error_pdf
#         } else {
#           NULL
#         }
#       })
#     }
#
#     # PDF button UI
#     output$pdf_button <- renderUI({
#       if (!is.null(selected_file())) {
#         downloadLink(
#           "pdf_download",
#           label = "PDF",
#           class = "btn-pdf"
#         )
#       } else {
#         tags$a(
#           href = "#",
#           class = "btn-pdf disabled",
#           "PDF"
#         )
#       }
#     })
#
#     # PDF download handler
#     output$pdf_download <- downloadHandler(
#       filename = function() {
#         req(selected_file())
#         paste0(tools::file_path_sans_ext(basename(selected_file())), ".pdf")
#       },
#       content = function(file) {
#         req(selected_file())
#         pdf_path <- create_pdf(selected_file())
#         if (!is.null(pdf_path)) {
#           file.copy(pdf_path, file)
#         } else {
#           stop("Failed to create PDF")
#         }
#       }
#     )
#
#     # Directory tree builder with stricter path validation
#     build_directory_tree <- function(path, prefix = "") {
#       if (!validate_path(path)) {
#         return(NULL)
#       }
#
#       tryCatch({
#         items <- fs::dir_ls(path)
#         tags$ul(
#           lapply(items, function(item) {
#             if (!startsWith(normalizePath(item, winslash = "/"), home())) {
#               return(NULL)
#             }
#
#             item_name <- fs::path_file(item)
#             if (fs::dir_exists(item)) {
#               tags$li(
#                 tags$span(class = "folder", item_name),
#                 build_directory_tree(item, paste0(prefix, "/", item_name))
#               )
#             } else if (tolower(fs::path_ext(item)) == "md") {
#               tags$li(
#                 tags$a(
#                   href = "#",
#                   onclick = sprintf("Shiny.setInputValue('selected_file', '%s')",
#                                     fs::path_abs(item)),
#                   item_name
#                 )
#               )
#             }
#           })
#         )
#       }, error = function(e) NULL)
#     }
#
#     # Directory tree output
#     output$directory_tree <- renderUI({
#       build_directory_tree(home())
#     })
#
#     # Markdown preview output
#     # Replace your existing markdown_preview renderUI function with this:
#     # Replace your markdown_preview renderUI function with this:
#     output$markdown_preview <- renderUI({
#       req(selected_file())
#
#       tryCatch({
#         md_content <- readLines(selected_file(), warn = FALSE)
#         md_content <- paste(md_content, collapse = "\n")
#
#         # Pre-process Obsidian-style wiki links for images if needed
#         md_content <- gsub("!\\[\\[([^\\]]+\\.(png|jpg|jpeg|gif|svg))\\]\\]",
#                            "![](\\1)",
#                            md_content)
#
#         # Process code blocks
#         md_content <- gsub("```(\\w+)\n", "```\\1\n", md_content)
#
#         # Convert markdown to HTML
#         html_content <- commonmark::markdown_html(md_content)
#
#         # Get current file directory
#         current_dir <- dirname(selected_file())
#
#         # Extract all image references from the HTML
#         image_matches <- gregexpr('src="([^":/]+(?:/[^":/]+)*\\.(png|jpg|jpeg|gif|svg))"', html_content)
#         image_refs <- regmatches(html_content, image_matches)
#
#         # If we found any images
#         if (length(image_refs) > 0 && length(image_refs[[1]]) > 0) {
#           # Process each image reference
#           for (img_ref in image_refs[[1]]) {
#             # Extract the image path
#             img_path <- sub('src="([^"]+)"', "\\1", img_ref)
#
#             # Create absolute path relative to current file's directory
#             abs_img_path <- file.path(current_dir, img_path)
#
#             # If file exists, create proper URL
#             if (file.exists(abs_img_path)) {
#               # Convert to URL path relative to vault root
#               rel_to_vault <- fs::path_rel(abs_img_path, home())
#               # Create new image reference
#               new_img_ref <- paste0('src="obsidian_vault/', rel_to_vault, '"')
#               # Replace in the HTML content
#               html_content <- gsub(img_ref, new_img_ref, html_content, fixed = TRUE)
#             }
#           }
#         }
#
#         tagList(
#           HTML(html_content),
#           tags$script("addCopyButtons();")
#         )
#       }, error = function(e) {
#         HTML(paste("<p class='text-danger'>Error processing file: ", as.character(e), "</p>"))
#       })
#     })
#
#
#   }
#
#   shinyApp(ui = ui, server = server)
# }
#
#
#
