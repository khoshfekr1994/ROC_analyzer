# HANASH LAB - RODEO ROC CURVE ANALYZER
# Created by Hamid Khoshfekr Rudsari & Ehsan Irajizad, MD Anderson Cancer Center
# Last Update: December, 2025
# For any questions or issues, please contact: hkhoshfekr@mdanderson.org

# Function to check and install required packages
required_packages <- c("shiny", "pROC", "ggplot2", "dplyr", "readxl", "openxlsx", "rhandsontable")

for (package in required_packages) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    message(paste("Installing package:", package))
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

library(shiny)
library(pROC)
library(ggplot2)
library(dplyr)
library(readxl)
library(openxlsx)
library(rhandsontable)

ui <- fluidPage(
  titlePanel(
    div(style = "padding-bottom: 20px;",
        h1("RODEO ROC Curve Analyzer", 
           style = "color: #003366; font-weight: bold; margin-bottom: 10px;"),
        h3("Hanash Lab", 
           style = "color: #555555; font-weight: 600; margin-top: 5px; margin-bottom: 5px;"),
        h5("Biostatistics Team: Hamid Khoshfekr Rudsari & Ehsan Irajizad", 
           style = "color: #777777; font-weight: normal; margin-top: 5px; margin-bottom: 5px;"),
        h5("MD Anderson Cancer Center", 
           style = "color: #777777; font-weight: normal; margin-top: 0px; margin-bottom: 5px;"),
        h6("Last Updated: November 2025", 
           style = "color: #999999; font-style: italic; margin-top: 5px;")
    )
  ),
  
  # Create tabset panel
  tabsetPanel(
    id = "main_tabs",
    
    # Tab 1: File Upload Analysis
    tabPanel("File Upload Analysis",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Upload CSV/TSV/Excel File",
                           accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls")),
                 
                 h4("Filtering Options"),
                 uiOutput("filter1_col_ui"),
                 uiOutput("filter1_values_ui"),
                 
                 uiOutput("filter2_col_ui"),
                 uiOutput("filter2_values_ui"),
                 
                 uiOutput("filter3_col_ui"),
                 uiOutput("filter3_values_ui"),
                 
                 hr(),
                 
                 uiOutput("comparison_col_ui"),
                 uiOutput("case_group_ui"),
                 uiOutput("control_group_ui"),
                 uiOutput("marker_selection_ui"),
                 
                 actionButton("run_analysis", "Run Analysis", class = "btn-primary")
               ),
               
               mainPanel(
                 plotOutput("roc_plot", height = "600px", width = "600px"),
                 br(),
                 uiOutput("plot_note"),
                 br(),
                 downloadButton("download_table", "Download Table (Excel)", class = "btn-success"),
                 br(),
                 br(),
                 tableOutput("roc_table")
               )
             )
    ),
    
    # Tab 2: Paste Data Analysis
    tabPanel("Paste Data Analysis",
             sidebarLayout(
               sidebarPanel(
                 h4("Paste Your Data"),
                 p("Copy your data from other sources (such as Excel) and paste it into the table below. Click in the table and use Ctrl+V (Cmd+V on Mac)."),
                 actionButton("load_sample", "Load Sample Data", class = "btn-info"),
                 actionButton("reset_table", "Reset Table", class = "btn-warning"),
                 hr(),
                 
                 uiOutput("paste_comparison_col_ui"),
                 uiOutput("paste_case_group_ui"),
                 uiOutput("paste_control_group_ui"),
                 uiOutput("paste_marker_selection_ui"),
                 
                 actionButton("run_paste_analysis", "Run Analysis", class = "btn-primary")
               ),
               
               mainPanel(
                 h4("Editable Data Table"),
                 rHandsontableOutput("hot_table"),
                 br(),
                 plotOutput("paste_roc_plot", height = "600px", width = "600px"),
                 br(),
                 uiOutput("paste_plot_note"),
                 br(),
                 downloadButton("download_paste_table", "Download Table (Excel)", class = "btn-success"),
                 br(),
                 br(),
                 tableOutput("paste_roc_table")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if(ext == "csv") {
      read.csv(input$file$datapath, stringsAsFactors = FALSE)
    } else if(ext %in% c("xlsx", "xls")) {
      read_excel(input$file$datapath)
    } else {
      read.delim(input$file$datapath, stringsAsFactors = FALSE)
    }
  })
  
  # Filter 1
  output$filter1_col_ui <- renderUI({
    req(data())
    cols <- c("None", names(data()))
    selectInput("filter1_col", "Filter 1 - Column:", choices = cols, selected = "None")
  })
  
  output$filter1_values_ui <- renderUI({
    req(data(), input$filter1_col)
    
    if(input$filter1_col == "None") {
      return(NULL)
    }
    
    col_data <- data()[[input$filter1_col]]
    
    if(is.numeric(col_data)) {
      tagList(
        numericInput("filter1_min", "Minimum Value:", value = min(col_data, na.rm = TRUE)),
        numericInput("filter1_max", "Maximum Value:", value = max(col_data, na.rm = TRUE))
      )
    } else {
      unique_vals <- unique(col_data)
      unique_vals_non_empty <- unique_vals[!is.na(unique_vals) & unique_vals != ""]
      unique_vals_with_na <- c(unique_vals_non_empty, "NA (missing/blank values)")
      
      tagList(
        actionButton("filter1_select_all", "Select All", style = "margin-bottom: 5px;"),
        actionButton("filter1_deselect_all", "Deselect All", style = "margin-bottom: 10px;"),
        checkboxGroupInput("filter1_categories", "Select Values:", 
                           choices = unique_vals_with_na,
                           selected = unique_vals_with_na)
      )
    }
  })
  
  observeEvent(input$filter1_select_all, {
    req(data(), input$filter1_col)
    if(input$filter1_col != "None") {
      col_data <- data()[[input$filter1_col]]
      if(!is.numeric(col_data)) {
        unique_vals <- unique(col_data)
        unique_vals_non_empty <- unique_vals[!is.na(unique_vals) & unique_vals != ""]
        unique_vals_with_na <- c(unique_vals_non_empty, "NA (missing/blank values)")
        updateCheckboxGroupInput(session, "filter1_categories", selected = unique_vals_with_na)
      }
    }
  })
  
  observeEvent(input$filter1_deselect_all, {
    updateCheckboxGroupInput(session, "filter1_categories", selected = character(0))
  })
  
  # Filter 2
  output$filter2_col_ui <- renderUI({
    req(data())
    cols <- c("None", names(data()))
    selectInput("filter2_col", "Filter 2 - Column:", choices = cols, selected = "None")
  })
  
  output$filter2_values_ui <- renderUI({
    req(data(), input$filter2_col)
    
    if(input$filter2_col == "None") {
      return(NULL)
    }
    
    col_data <- data()[[input$filter2_col]]
    
    if(is.numeric(col_data)) {
      tagList(
        numericInput("filter2_min", "Minimum Value:", value = min(col_data, na.rm = TRUE)),
        numericInput("filter2_max", "Maximum Value:", value = max(col_data, na.rm = TRUE))
      )
    } else {
      unique_vals <- unique(col_data)
      unique_vals_non_empty <- unique_vals[!is.na(unique_vals) & unique_vals != ""]
      unique_vals_with_na <- c(unique_vals_non_empty, "NA (missing/blank values)")
      
      tagList(
        actionButton("filter2_select_all", "Select All", style = "margin-bottom: 5px;"),
        actionButton("filter2_deselect_all", "Deselect All", style = "margin-bottom: 10px;"),
        checkboxGroupInput("filter2_categories", "Select Values:", 
                           choices = unique_vals_with_na,
                           selected = unique_vals_with_na)
      )
    }
  })
  
  observeEvent(input$filter2_select_all, {
    req(data(), input$filter2_col)
    if(input$filter2_col != "None") {
      col_data <- data()[[input$filter2_col]]
      if(!is.numeric(col_data)) {
        unique_vals <- unique(col_data)
        unique_vals_non_empty <- unique_vals[!is.na(unique_vals) & unique_vals != ""]
        unique_vals_with_na <- c(unique_vals_non_empty, "NA (missing/blank values)")
        updateCheckboxGroupInput(session, "filter2_categories", selected = unique_vals_with_na)
      }
    }
  })
  
  observeEvent(input$filter2_deselect_all, {
    updateCheckboxGroupInput(session, "filter2_categories", selected = character(0))
  })
  
  # Filter 3
  output$filter3_col_ui <- renderUI({
    req(data())
    cols <- c("None", names(data()))
    selectInput("filter3_col", "Filter 3 - Column:", choices = cols, selected = "None")
  })
  
  output$filter3_values_ui <- renderUI({
    req(data(), input$filter3_col)
    
    if(input$filter3_col == "None") {
      return(NULL)
    }
    
    col_data <- data()[[input$filter3_col]]
    
    if(is.numeric(col_data)) {
      tagList(
        numericInput("filter3_min", "Minimum Value:", value = min(col_data, na.rm = TRUE)),
        numericInput("filter3_max", "Maximum Value:", value = max(col_data, na.rm = TRUE))
      )
    } else {
      unique_vals <- unique(col_data)
      unique_vals_non_empty <- unique_vals[!is.na(unique_vals) & unique_vals != ""]
      unique_vals_with_na <- c(unique_vals_non_empty, "NA (missing/blank values)")
      
      tagList(
        actionButton("filter3_select_all", "Select All", style = "margin-bottom: 5px;"),
        actionButton("filter3_deselect_all", "Deselect All", style = "margin-bottom: 10px;"),
        checkboxGroupInput("filter3_categories", "Select Values:", 
                           choices = unique_vals_with_na,
                           selected = unique_vals_with_na)
      )
    }
  })
  
  observeEvent(input$filter3_select_all, {
    req(data(), input$filter3_col)
    if(input$filter3_col != "None") {
      col_data <- data()[[input$filter3_col]]
      if(!is.numeric(col_data)) {
        unique_vals <- unique(col_data)
        unique_vals_non_empty <- unique_vals[!is.na(unique_vals) & unique_vals != ""]
        unique_vals_with_na <- c(unique_vals_non_empty, "NA (missing/blank values)")
        updateCheckboxGroupInput(session, "filter3_categories", selected = unique_vals_with_na)
      }
    }
  })
  
  observeEvent(input$filter3_deselect_all, {
    updateCheckboxGroupInput(session, "filter3_categories", selected = character(0))
  })
  
  filtered_data <- reactive({
    req(data())
    df <- data()
    
    # Apply Filter 1
    if(!is.null(input$filter1_col) && input$filter1_col != "None") {
      col_data <- df[[input$filter1_col]]
      
      if(is.numeric(col_data)) {
        req(input$filter1_min, input$filter1_max)
        df <- df[!is.na(col_data) & col_data >= input$filter1_min & col_data <= input$filter1_max, ]
      } else {
        req(input$filter1_categories)
        
        filter_has_na <- "NA (missing/blank values)" %in% input$filter1_categories
        filter_cats <- input$filter1_categories[input$filter1_categories != "NA (missing/blank values)"]
        
        if(filter_has_na && length(filter_cats) > 0) {
          df <- df[is.na(col_data) | col_data == "" | col_data %in% filter_cats, ]
        } else if(filter_has_na) {
          df <- df[is.na(col_data) | col_data == "", ]
        } else {
          df <- df[!is.na(col_data) & col_data != "" & col_data %in% filter_cats, ]
        }
      }
    }
    
    # Apply Filter 2
    if(!is.null(input$filter2_col) && input$filter2_col != "None") {
      col_data <- df[[input$filter2_col]]
      
      if(is.numeric(col_data)) {
        req(input$filter2_min, input$filter2_max)
        df <- df[!is.na(col_data) & col_data >= input$filter2_min & col_data <= input$filter2_max, ]
      } else {
        req(input$filter2_categories)
        
        filter_has_na <- "NA (missing/blank values)" %in% input$filter2_categories
        filter_cats <- input$filter2_categories[input$filter2_categories != "NA (missing/blank values)"]
        
        if(filter_has_na && length(filter_cats) > 0) {
          df <- df[is.na(col_data) | col_data == "" | col_data %in% filter_cats, ]
        } else if(filter_has_na) {
          df <- df[is.na(col_data) | col_data == "", ]
        } else {
          df <- df[!is.na(col_data) & col_data != "" & col_data %in% filter_cats, ]
        }
      }
    }
    
    # Apply Filter 3
    if(!is.null(input$filter3_col) && input$filter3_col != "None") {
      col_data <- df[[input$filter3_col]]
      
      if(is.numeric(col_data)) {
        req(input$filter3_min, input$filter3_max)
        df <- df[!is.na(col_data) & col_data >= input$filter3_min & col_data <= input$filter3_max, ]
      } else {
        req(input$filter3_categories)
        
        filter_has_na <- "NA (missing/blank values)" %in% input$filter3_categories
        filter_cats <- input$filter3_categories[input$filter3_categories != "NA (missing/blank values)"]
        
        if(filter_has_na && length(filter_cats) > 0) {
          df <- df[is.na(col_data) | col_data == "" | col_data %in% filter_cats, ]
        } else if(filter_has_na) {
          df <- df[is.na(col_data) | col_data == "", ]
        } else {
          df <- df[!is.na(col_data) & col_data != "" & col_data %in% filter_cats, ]
        }
      }
    }
    
    df
  })
  
  output$comparison_col_ui <- renderUI({
    req(filtered_data())
    cols <- names(filtered_data())
    selectInput("comparison_col", "Select Comparison Column:", choices = cols)
  })
  
  output$case_group_ui <- renderUI({
    req(filtered_data(), input$comparison_col)
    groups <- unique(filtered_data()[[input$comparison_col]])
    groups_non_empty <- groups[!is.na(groups) & groups != ""]
    groups_with_na <- c(groups_non_empty, "NA (missing/blank values)")
    checkboxGroupInput("case_group", "Select Case Group(s):", choices = groups_with_na)
  })
  
  output$control_group_ui <- renderUI({
    req(filtered_data(), input$comparison_col)
    groups <- unique(filtered_data()[[input$comparison_col]])
    groups_non_empty <- groups[!is.na(groups) & groups != ""]
    groups_with_na <- c(groups_non_empty, "NA (missing/blank values)")
    checkboxGroupInput("control_group", "Select Control Group(s):", 
                       choices = groups_with_na, 
                       selected = "NA (missing/blank values)")
  })
  
  output$marker_selection_ui <- renderUI({
    req(filtered_data(), input$comparison_col)
    numeric_cols <- names(filtered_data())[sapply(filtered_data(), is.numeric)]
    tagList(
      actionButton("markers_select_all", "Select All", style = "margin-bottom: 5px;"),
      actionButton("markers_deselect_all", "Deselect All", style = "margin-bottom: 10px;"),
      checkboxGroupInput("markers", "Select Markers:", choices = numeric_cols)
    )
  })
  
  observeEvent(input$markers_select_all, {
    req(filtered_data(), input$comparison_col)
    numeric_cols <- names(filtered_data())[sapply(filtered_data(), is.numeric)]
    updateCheckboxGroupInput(session, "markers", selected = numeric_cols)
  })
  
  observeEvent(input$markers_deselect_all, {
    updateCheckboxGroupInput(session, "markers", selected = character(0))
  })
  
  roc_results <- eventReactive(input$run_analysis, {
    req(filtered_data(), input$comparison_col, input$case_group, input$control_group, input$markers)
    
    df <- filtered_data()
    comparison_col <- input$comparison_col
    case_groups <- input$case_group
    control_groups <- input$control_group
    markers <- input$markers
    
    case_has_na <- "NA (missing/blank values)" %in% case_groups
    control_has_na <- "NA (missing/blank values)" %in% control_groups
    
    if(case_has_na && control_has_na) {
      stop("Case and control groups cannot both include NA")
    }
    
    if(length(case_groups) == 0) {
      stop("Please select at least one case group")
    }
    
    if(length(control_groups) == 0) {
      stop("Please select at least one control group")
    }
    
    case_groups_non_na <- case_groups[case_groups != "NA (missing/blank values)"]
    control_groups_non_na <- control_groups[control_groups != "NA (missing/blank values)"]
    
    if(case_has_na && !control_has_na) {
      df <- df[(is.na(df[[comparison_col]]) | df[[comparison_col]] == "") | 
                 (df[[comparison_col]] %in% control_groups_non_na), ]
    } else if(!case_has_na && control_has_na) {
      df <- df[(is.na(df[[comparison_col]]) | df[[comparison_col]] == "") | 
                 (df[[comparison_col]] %in% case_groups_non_na), ]
    } else {
      df <- df[(!is.na(df[[comparison_col]]) & df[[comparison_col]] != "") & 
                 ((df[[comparison_col]] %in% case_groups_non_na) | 
                    (df[[comparison_col]] %in% control_groups_non_na)), ]
    }
    
    if(case_has_na) {
      df$outcome <- ifelse(is.na(df[[comparison_col]]) | df[[comparison_col]] == "", 1,
                           ifelse(df[[comparison_col]] %in% control_groups_non_na, 0, NA))
    } else if(control_has_na) {
      df$outcome <- ifelse(df[[comparison_col]] %in% case_groups_non_na, 1,
                           ifelse(is.na(df[[comparison_col]]) | df[[comparison_col]] == "", 0, NA))
    } else {
      df$outcome <- ifelse(df[[comparison_col]] %in% case_groups_non_na, 1,
                           ifelse(df[[comparison_col]] %in% control_groups_non_na, 0, NA))
    }
    
    df <- df[!is.na(df$outcome), ]
    
    if(sum(df$outcome == 1) == 0) stop("No case observations found")
    if(sum(df$outcome == 0) == 0) stop("No control observations found")
    
    results_list <- list()
    roc_objects <- list()
    
    spec_points <- c(0.60, 0.70, 0.80, 0.86, 0.90, 0.95, 0.985)
    
    for(marker in markers) {
      if(all(is.na(df[[marker]]))) next
      
      df_marker <- df[!is.na(df[[marker]]), ]
      
      # Check if there are at least 2 cases and 2 controls
      n_cases <- sum(df_marker$outcome == 1)
      n_controls <- sum(df_marker$outcome == 0)
      
      if(n_cases < 2 || n_controls < 2) {
        warning(paste("Skipping marker", marker, "- insufficient samples. Need at least 2 cases and 2 controls. Found:", n_cases, "cases and", n_controls, "controls."))
        next
      }
      
      roc_obj <- roc(df_marker$outcome, df_marker[[marker]], 
                     levels = c(0, 1), direction = "<", quiet = TRUE)
      
      roc_objects[[marker]] <- roc_obj
      
      ci_obj <- ci.auc(roc_obj, conf.level = 0.95)
      
      # Calculate P-value testing if AUC is significantly different from 0.5
      auc_value <- as.numeric(roc_obj$auc)
      auc_var <- var(roc_obj)
      
      p_value <- tryCatch({
        if(!is.na(auc_var) && auc_var > 0) {
          z_score <- (auc_value - 0.5) / sqrt(auc_var)
          2 * pnorm(-abs(z_score))
        } else {
          NA
        }
      }, error = function(e) {
        NA
      })
      
      sens_at_spec <- sapply(spec_points, function(sp) {
        coords_obj <- coords(roc_obj, x = sp, input = "specificity", 
                             ret = "sensitivity", transpose = FALSE)
        if(nrow(coords_obj) > 0) coords_obj$sensitivity[1] else NA
      })
      
      result_row <- data.frame(
        Marker = marker,
        N_Cases = n_cases,
        N_Controls = n_controls,
        P_value = ifelse(is.na(p_value), NA, round(p_value, 4)),
        AUC = round(as.numeric(roc_obj$auc), 4),
        AUC_CI_Lower = round(ci_obj[1], 4),
        AUC_CI_Upper = round(ci_obj[3], 4),
        Sens_60 = round(sens_at_spec[1], 4),
        Sens_70 = round(sens_at_spec[2], 4),
        Sens_80 = round(sens_at_spec[3], 4),
        Sens_86 = round(sens_at_spec[4], 4),
        Sens_90 = round(sens_at_spec[5], 4),
        Sens_95 = round(sens_at_spec[6], 4),
        Sens_98.5 = round(sens_at_spec[7], 4),
        stringsAsFactors = FALSE
      )
      
      results_list[[marker]] <- result_row
    }
    
    list(table = do.call(rbind, results_list), roc_objects = roc_objects)
  })
  
  output$roc_plot <- renderPlot({
    req(roc_results())
    
    roc_objs <- roc_results()$roc_objects
    
    # Limit to first 10 markers for plotting
    markers_to_plot <- head(names(roc_objs), 10)
    roc_objs_plot <- roc_objs[markers_to_plot]
    
    colors <- rainbow(length(roc_objs_plot))
    
    par(pty = "s")
    
    plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
         xlab = "1 - Specificity (False Positive Rate)",
         ylab = "Sensitivity (True Positive Rate)",
         main = "ROC Curves Comparison",
         cex.lab = 1.2, cex.main = 1.4, font.main = 2,
         xaxs = "i", yaxs = "i")
    
    abline(a = 0, b = 1, lty = 2, col = "gray50", lwd = 2)
    
    grid(col = "gray90", lty = 1)
    
    for(i in seq_along(roc_objs_plot)) {
      marker <- names(roc_objs_plot)[i]
      roc_obj <- roc_objs_plot[[marker]]
      
      lines(1 - roc_obj$specificities, roc_obj$sensitivities,
            col = colors[i], lwd = 2.5)
    }
    
    legend("bottomright", 
           legend = names(roc_objs_plot),
           col = colors,
           lwd = 2.5,
           bty = "n",
           cex = 1.1)
    
    box(lwd = 1.5)
  })
  
  output$plot_note <- renderUI({
    req(roc_results())
    
    roc_objs <- roc_results()$roc_objects
    n_markers <- length(roc_objs)
    
    if(n_markers > 10) {
      h5(paste("Note: Only the first 10 markers are displayed in the ROC plot. All", 
               n_markers, "markers are included in the table below."),
         style = "color: #d9534f; font-weight: bold;")
    }
  })
  
  output$roc_table <- renderTable({
    req(roc_results())
    
    result_table <- roc_results()$table
    
    names(result_table) <- c(
      "Marker",
      "N Cases",
      "N Controls",
      "P-value",
      "AUC",
      "95% CI Lower",
      "95% CI Upper",
      "Sens @ 60% Spec",
      "Sens @ 70% Spec",
      "Sens @ 80% Spec",
      "Sens @ 86% Spec",
      "Sens @ 90% Spec",
      "Sens @ 95% Spec",
      "Sens @ 98.5% Spec"
    )
    
    result_table
  }, rownames = FALSE, digits = 4)
  
  output$download_table <- downloadHandler(
    filename = function() {
      paste0("ROC_Analysis_Results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(roc_results())
      
      result_table <- roc_results()$table
      
      names(result_table) <- c(
        "Marker",
        "N Cases",
        "N Controls",
        "P-value",
        "AUC",
        "95% CI Lower",
        "95% CI Upper",
        "Sens @ 60% Spec",
        "Sens @ 70% Spec",
        "Sens @ 80% Spec",
        "Sens @ 86% Spec",
        "Sens @ 90% Spec",
        "Sens @ 95% Spec",
        "Sens @ 98.5% Spec"
      )
      
      write.xlsx(result_table, file, rowNames = FALSE)
    }
  )
  
  # ============================================
  # TAB 2: PASTE DATA ANALYSIS (NO FILTERING)
  # ============================================
  
  # Initialize empty data frame for pasted data
  paste_data_values <- reactiveValues(df = NULL, headers_processed = FALSE)
  
  # Load sample data
  observeEvent(input$load_sample, {
    sample_df <- data.frame(
      ID = 1:10,
      Group = c("Case", "Case", "Control", "Control", "Case", 
                "Control", "Case", "Control", "Case", "Control"),
      Age = c(45, 52, 38, 61, 49, 44, 55, 40, 58, 47),
      Marker1 = runif(10, 0, 100),
      Marker2 = runif(10, 0, 100),
      Marker3 = runif(10, 0, 100),
      stringsAsFactors = FALSE
    )
    paste_data_values$df <- sample_df
    paste_data_values$headers_processed <- TRUE
  })
  
  # Reset table
  observeEvent(input$reset_table, {
    df <- data.frame(matrix("", nrow = 20, ncol = 6), stringsAsFactors = FALSE)
    names(df) <- paste0("Column_", 1:6)
    paste_data_values$df <- df
    paste_data_values$headers_processed <- FALSE
  })
  
  # Render handsontable
  output$hot_table <- renderRHandsontable({
    if(is.null(paste_data_values$df)) {
      # Create empty table with 20 rows and 6 columns
      df <- data.frame(matrix("", nrow = 20, ncol = 6), stringsAsFactors = FALSE)
      names(df) <- paste0("Column_", 1:6)
      paste_data_values$df <- df
      paste_data_values$headers_processed <- FALSE
    }
    
    # Get the current data
    df <- paste_data_values$df
    
    rhandsontable(df, rowHeaders = TRUE, stretchH = "all", useTypes = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
  })
  
  # Update reactive values when table is edited
  observeEvent(input$hot_table, {
    new_df <- hot_to_r(input$hot_table)
    
    # Check if this is new data being pasted (headers not yet processed)
    # OR if we're still in the process of pasting multiple columns
    if(nrow(new_df) > 0) {
      first_row <- as.character(new_df[1, ])
      
      # Check each column to see if it might contain a header in the first row
      for(i in seq_along(first_row)) {
        current_colname <- names(new_df)[i]
        first_cell <- first_row[i]
        
        # If the first cell is non-empty and the column name is a default name
        # then the first cell is likely a header
        if(!is.na(first_cell) && first_cell != "" && 
           grepl("^Column_", current_colname)) {
          
          # Check if this looks like a header (text) vs data (numeric)
          # If there are more rows, check if row 2 onwards contains different types of data
          if(nrow(new_df) > 1) {
            # Use the first cell as the column name
            new_colname <- make.names(first_cell, unique = TRUE)
            
            # Make sure it's unique compared to existing names
            existing_names <- names(new_df)[-i]
            counter <- 1
            original_name <- new_colname
            while(new_colname %in% existing_names) {
              new_colname <- paste0(original_name, ".", counter)
              counter <- counter + 1
            }
            
            names(new_df)[i] <- new_colname
            
            # Remove the first cell from this column by shifting data up
            new_df[1:(nrow(new_df)-1), i] <- new_df[2:nrow(new_df), i]
            new_df[nrow(new_df), i] <- ""
          }
        }
      }
    }
    
    paste_data_values$df <- new_df
  })
  
  # Get current paste data (cleaned, headers already processed)
  paste_data <- reactive({
    req(paste_data_values$df)
    df <- paste_data_values$df
    
    # Convert empty strings to NA for easier processing
    df[df == ""] <- NA
    
    # Remove completely empty rows
    non_empty_rows <- rowSums(!is.na(df)) > 0
    if(!any(non_empty_rows)) return(NULL)
    df <- df[non_empty_rows, , drop = FALSE]
    
    # Remove completely empty columns
    non_empty_cols <- colSums(!is.na(df)) > 0
    if(!any(non_empty_cols)) return(NULL)
    df <- df[, non_empty_cols, drop = FALSE]
    
    if(nrow(df) == 0 || ncol(df) == 0) return(NULL)
    
    # Try to convert columns to numeric where appropriate
    for(col in names(df)) {
      # Try numeric conversion
      numeric_col <- suppressWarnings(as.numeric(df[[col]]))
      # If more than 50% of non-NA values are successfully converted, make it numeric
      non_na_original <- sum(!is.na(df[[col]]))
      non_na_converted <- sum(!is.na(numeric_col))
      if(non_na_original > 0 && non_na_converted > 0.5 * non_na_original) {
        df[[col]] <- numeric_col
      }
    }
    
    df
  })
  
  # Paste Comparison column and groups
  output$paste_comparison_col_ui <- renderUI({
    req(paste_data())
    cols <- names(paste_data())
    selectInput("paste_comparison_col", "Comparison Column (Case vs Control):", choices = cols)
  })
  
  output$paste_case_group_ui <- renderUI({
    req(paste_data(), input$paste_comparison_col)
    
    unique_vals <- unique(paste_data()[[input$paste_comparison_col]])
    unique_vals_non_empty <- unique_vals[!is.na(unique_vals) & unique_vals != ""]
    unique_vals_with_na <- c(unique_vals_non_empty, "NA (missing/blank values)")
    
    tagList(
      actionButton("paste_case_select_all", "Select All Cases", style = "margin-bottom: 5px;"),
      actionButton("paste_case_deselect_all", "Deselect All Cases", style = "margin-bottom: 10px;"),
      checkboxGroupInput("paste_case_group", "Select Case Group(s):", 
                         choices = unique_vals_with_na,
                         selected = character(0))
    )
  })
  
  output$paste_control_group_ui <- renderUI({
    req(paste_data(), input$paste_comparison_col)
    
    unique_vals <- unique(paste_data()[[input$paste_comparison_col]])
    unique_vals_non_empty <- unique_vals[!is.na(unique_vals) & unique_vals != ""]
    unique_vals_with_na <- c(unique_vals_non_empty, "NA (missing/blank values)")
    
    tagList(
      actionButton("paste_control_select_all", "Select All Controls", style = "margin-bottom: 5px;"),
      actionButton("paste_control_deselect_all", "Deselect All Controls", style = "margin-bottom: 10px;"),
      checkboxGroupInput("paste_control_group", "Select Control Group(s):", 
                         choices = unique_vals_with_na,
                         selected = character(0))
    )
  })
  
  observeEvent(input$paste_case_select_all, {
    req(paste_data(), input$paste_comparison_col)
    unique_vals <- unique(paste_data()[[input$paste_comparison_col]])
    unique_vals_non_empty <- unique_vals[!is.na(unique_vals) & unique_vals != ""]
    unique_vals_with_na <- c(unique_vals_non_empty, "NA (missing/blank values)")
    updateCheckboxGroupInput(session, "paste_case_group", selected = unique_vals_with_na)
  })
  
  observeEvent(input$paste_case_deselect_all, {
    updateCheckboxGroupInput(session, "paste_case_group", selected = character(0))
  })
  
  observeEvent(input$paste_control_select_all, {
    req(paste_data(), input$paste_comparison_col)
    unique_vals <- unique(paste_data()[[input$paste_comparison_col]])
    unique_vals_non_empty <- unique_vals[!is.na(unique_vals) & unique_vals != ""]
    unique_vals_with_na <- c(unique_vals_non_empty, "NA (missing/blank values)")
    updateCheckboxGroupInput(session, "paste_control_group", selected = unique_vals_with_na)
  })
  
  observeEvent(input$paste_control_deselect_all, {
    updateCheckboxGroupInput(session, "paste_control_group", selected = character(0))
  })
  
  # Paste Marker selection
  output$paste_marker_selection_ui <- renderUI({
    req(paste_data())
    
    numeric_cols <- names(paste_data())[sapply(paste_data(), is.numeric)]
    
    tagList(
      actionButton("paste_markers_select_all", "Select All Markers", style = "margin-bottom: 5px;"),
      actionButton("paste_markers_deselect_all", "Deselect All Markers", style = "margin-bottom: 10px;"),
      checkboxGroupInput("paste_markers", "Select Markers for ROC Analysis:", 
                         choices = numeric_cols,
                         selected = character(0))
    )
  })
  
  observeEvent(input$paste_markers_select_all, {
    req(paste_data())
    numeric_cols <- names(paste_data())[sapply(paste_data(), is.numeric)]
    updateCheckboxGroupInput(session, "paste_markers", selected = numeric_cols)
  })
  
  observeEvent(input$paste_markers_deselect_all, {
    updateCheckboxGroupInput(session, "paste_markers", selected = character(0))
  })
  
  paste_roc_results <- eventReactive(input$run_paste_analysis, {
    req(paste_data(), input$paste_comparison_col, input$paste_case_group, input$paste_control_group, input$paste_markers)
    
    df <- paste_data()
    comparison_col <- input$paste_comparison_col
    case_groups <- input$paste_case_group
    control_groups <- input$paste_control_group
    markers <- input$paste_markers
    
    case_has_na <- "NA (missing/blank values)" %in% case_groups
    control_has_na <- "NA (missing/blank values)" %in% control_groups
    
    if(case_has_na && control_has_na) {
      stop("Case and control groups cannot both include NA")
    }
    
    if(length(case_groups) == 0) {
      stop("Please select at least one case group")
    }
    
    if(length(control_groups) == 0) {
      stop("Please select at least one control group")
    }
    
    case_groups_non_na <- case_groups[case_groups != "NA (missing/blank values)"]
    control_groups_non_na <- control_groups[control_groups != "NA (missing/blank values)"]
    
    if(case_has_na && !control_has_na) {
      df <- df[(is.na(df[[comparison_col]]) | df[[comparison_col]] == "") | 
                 (df[[comparison_col]] %in% control_groups_non_na), ]
    } else if(!case_has_na && control_has_na) {
      df <- df[(is.na(df[[comparison_col]]) | df[[comparison_col]] == "") | 
                 (df[[comparison_col]] %in% case_groups_non_na), ]
    } else {
      df <- df[(!is.na(df[[comparison_col]]) & df[[comparison_col]] != "") & 
                 ((df[[comparison_col]] %in% case_groups_non_na) | 
                    (df[[comparison_col]] %in% control_groups_non_na)), ]
    }
    
    if(case_has_na) {
      df$outcome <- ifelse(is.na(df[[comparison_col]]) | df[[comparison_col]] == "", 1,
                           ifelse(df[[comparison_col]] %in% control_groups_non_na, 0, NA))
    } else if(control_has_na) {
      df$outcome <- ifelse(df[[comparison_col]] %in% case_groups_non_na, 1,
                           ifelse(is.na(df[[comparison_col]]) | df[[comparison_col]] == "", 0, NA))
    } else {
      df$outcome <- ifelse(df[[comparison_col]] %in% case_groups_non_na, 1,
                           ifelse(df[[comparison_col]] %in% control_groups_non_na, 0, NA))
    }
    
    df <- df[!is.na(df$outcome), ]
    
    if(sum(df$outcome == 1) == 0) stop("No case observations found")
    if(sum(df$outcome == 0) == 0) stop("No control observations found")
    
    results_list <- list()
    roc_objects <- list()
    
    spec_points <- c(0.60, 0.70, 0.80, 0.86, 0.90, 0.95, 0.985)
    
    for(marker in markers) {
      if(all(is.na(df[[marker]]))) next
      
      df_marker <- df[!is.na(df[[marker]]), ]
      
      # Check if there are at least 2 cases and 2 controls
      n_cases <- sum(df_marker$outcome == 1)
      n_controls <- sum(df_marker$outcome == 0)
      
      if(n_cases < 2 || n_controls < 2) {
        warning(paste("Skipping marker", marker, "- insufficient samples. Need at least 2 cases and 2 controls. Found:", n_cases, "cases and", n_controls, "controls."))
        next
      }
      
      roc_obj <- roc(df_marker$outcome, df_marker[[marker]], 
                     levels = c(0, 1), direction = "<", quiet = TRUE)
      
      roc_objects[[marker]] <- roc_obj
      
      ci_obj <- ci.auc(roc_obj, conf.level = 0.95)
      
      # Calculate P-value testing if AUC is significantly different from 0.5
      auc_value <- as.numeric(roc_obj$auc)
      auc_var <- var(roc_obj)
      
      p_value <- tryCatch({
        if(!is.na(auc_var) && auc_var > 0) {
          z_score <- (auc_value - 0.5) / sqrt(auc_var)
          2 * pnorm(-abs(z_score))
        } else {
          NA
        }
      }, error = function(e) {
        NA
      })
      
      sens_at_spec <- sapply(spec_points, function(sp) {
        coords_obj <- coords(roc_obj, x = sp, input = "specificity", 
                             ret = "sensitivity", transpose = FALSE)
        if(nrow(coords_obj) > 0) coords_obj$sensitivity[1] else NA
      })
      
      result_row <- data.frame(
        Marker = marker,
        N_Cases = n_cases,
        N_Controls = n_controls,
        P_value = ifelse(is.na(p_value), NA, round(p_value, 4)),
        AUC = round(as.numeric(roc_obj$auc), 4),
        AUC_CI_Lower = round(ci_obj[1], 4),
        AUC_CI_Upper = round(ci_obj[3], 4),
        Sens_60 = round(sens_at_spec[1], 4),
        Sens_70 = round(sens_at_spec[2], 4),
        Sens_80 = round(sens_at_spec[3], 4),
        Sens_86 = round(sens_at_spec[4], 4),
        Sens_90 = round(sens_at_spec[5], 4),
        Sens_95 = round(sens_at_spec[6], 4),
        Sens_98.5 = round(sens_at_spec[7], 4),
        stringsAsFactors = FALSE
      )
      
      results_list[[marker]] <- result_row
    }
    
    list(table = do.call(rbind, results_list), roc_objects = roc_objects)
  })
  
  output$paste_roc_plot <- renderPlot({
    req(paste_roc_results())
    
    roc_objs <- paste_roc_results()$roc_objects
    
    # Limit to first 10 markers for plotting
    markers_to_plot <- head(names(roc_objs), 10)
    roc_objs_plot <- roc_objs[markers_to_plot]
    
    colors <- rainbow(length(roc_objs_plot))
    
    par(pty = "s")
    
    plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
         xlab = "1 - Specificity (False Positive Rate)",
         ylab = "Sensitivity (True Positive Rate)",
         main = "ROC Curves Comparison",
         cex.lab = 1.2, cex.main = 1.4, font.main = 2,
         xaxs = "i", yaxs = "i")
    
    abline(a = 0, b = 1, lty = 2, col = "gray50", lwd = 2)
    
    grid(col = "gray90", lty = 1)
    
    for(i in seq_along(roc_objs_plot)) {
      marker <- names(roc_objs_plot)[i]
      roc_obj <- roc_objs_plot[[marker]]
      
      lines(1 - roc_obj$specificities, roc_obj$sensitivities,
            col = colors[i], lwd = 2.5)
    }
    
    legend("bottomright", 
           legend = names(roc_objs_plot),
           col = colors,
           lwd = 2.5,
           bty = "n",
           cex = 1.1)
    
    box(lwd = 1.5)
  })
  
  output$paste_plot_note <- renderUI({
    req(paste_roc_results())
    
    roc_objs <- paste_roc_results()$roc_objects
    n_markers <- length(roc_objs)
    
    if(n_markers > 10) {
      h5(paste("Note: Only the first 10 markers are displayed in the ROC plot. All", 
               n_markers, "markers are included in the table below."),
         style = "color: #d9534f; font-weight: bold;")
    }
  })
  
  output$paste_roc_table <- renderTable({
    req(paste_roc_results())
    
    result_table <- paste_roc_results()$table
    
    names(result_table) <- c(
      "Marker",
      "N Cases",
      "N Controls",
      "P-value",
      "AUC",
      "95% CI Lower",
      "95% CI Upper",
      "Sens @ 60% Spec",
      "Sens @ 70% Spec",
      "Sens @ 80% Spec",
      "Sens @ 86% Spec",
      "Sens @ 90% Spec",
      "Sens @ 95% Spec",
      "Sens @ 98.5% Spec"
    )
    
    result_table
  }, rownames = FALSE, digits = 4)
  
  output$download_paste_table <- downloadHandler(
    filename = function() {
      paste0("ROC_Analysis_Results_Pasted_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(paste_roc_results())
      
      result_table <- paste_roc_results()$table
      
      names(result_table) <- c(
        "Marker",
        "N Cases",
        "N Controls",
        "P-value",
        "AUC",
        "95% CI Lower",
        "95% CI Upper",
        "Sens @ 60% Spec",
        "Sens @ 70% Spec",
        "Sens @ 80% Spec",
        "Sens @ 86% Spec",
        "Sens @ 90% Spec",
        "Sens @ 95% Spec",
        "Sens @ 98.5% Spec"
      )
      
      write.xlsx(result_table, file, rowNames = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)