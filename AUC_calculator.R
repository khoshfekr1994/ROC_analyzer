# HANASH LAB - RODEO ROC CURVE ANALYZER
# Created by Hamid Khoshfekr Rudsari & Ehsan Irajizad, MD Anderson Cancer Center
# Last Update: November, 2025
# For any questions or issues, please contact: hkhoshfekr@mdanderson.org

# Function to check and install required packages
required_packages <- c("shiny", "pROC", "ggplot2", "dplyr", "readxl")

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

ui <- fluidPage(
  titlePanel(
    div(
      h2("RODEO ROC Curve Analyzer"),
      h4("Hanash Lab", style = "color: gray; font-weight: normal;"),
      h4("Created by Biostat team:", style = "color: gray; font-weight: normal;"),
      h4("Hamid Khoshfekr Rudsari & Ehsan Irajizad, MD Anderson Cancer Center", style = "color: gray; font-weight: normal;"),
      h4("Last Update: November, 2025", style = "color: gray; font-weight: normal;")
    )
  ),
  
  
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
      tableOutput("roc_table")
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
    checkboxGroupInput("markers", "Select Markers:", choices = numeric_cols)
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
      
      if(sum(df_marker$outcome == 1) == 0 || sum(df_marker$outcome == 0) == 0) next
      
      n_cases <- sum(df_marker$outcome == 1)
      n_controls <- sum(df_marker$outcome == 0)
      
      roc_obj <- roc(df_marker$outcome, df_marker[[marker]], 
                    levels = c(0, 1), direction = "<", quiet = TRUE)
      
      roc_objects[[marker]] <- roc_obj
      
      ci_obj <- ci.auc(roc_obj, conf.level = 0.95)
      
      sens_at_spec <- sapply(spec_points, function(sp) {
        coords_obj <- coords(roc_obj, x = sp, input = "specificity", 
                            ret = "sensitivity", transpose = FALSE)
        if(nrow(coords_obj) > 0) coords_obj$sensitivity[1] else NA
      })
      
      result_row <- data.frame(
        Marker = marker,
        N_Cases = n_cases,
        N_Controls = n_controls,
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
    
    colors <- rainbow(length(roc_objs))
    
    par(pty = "s")
    
    plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
         xlab = "1 - Specificity (False Positive Rate)",
         ylab = "Sensitivity (True Positive Rate)",
         main = "ROC Curves Comparison",
         cex.lab = 1.2, cex.main = 1.4, font.main = 2,
         xaxs = "i", yaxs = "i")
    
    abline(a = 0, b = 1, lty = 2, col = "gray50", lwd = 2)
    
    grid(col = "gray90", lty = 1)
    
    for(i in seq_along(roc_objs)) {
      marker <- names(roc_objs)[i]
      roc_obj <- roc_objs[[marker]]
      
      lines(1 - roc_obj$specificities, roc_obj$sensitivities,
            col = colors[i], lwd = 2.5)
    }
    
    legend("bottomright", 
           legend = names(roc_objs),
           col = colors,
           lwd = 2.5,
           bty = "n",
           cex = 1.1)
    
    box(lwd = 1.5)
  })
  
  output$roc_table <- renderTable({
    req(roc_results())
    
    result_table <- roc_results()$table
    
    names(result_table) <- c(
      "Marker",
      "N Cases",
      "N Controls",
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
}

shinyApp(ui = ui, server = server)