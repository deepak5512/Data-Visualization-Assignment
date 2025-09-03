# dashboard/app.R
# Shiny dashboard integrating filters, tabs, and dynamic plots.

library(shiny)
library(shinythemes)
library(shinydashboard)
shinyjs_available <- requireNamespace("shinyjs", quietly = TRUE)
if (shinyjs_available) {
  library(shinyjs)
}
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(ggthemes)
library(scales)
library(corrplot)
library(ggridges)
library(fmsb)
library(networkD3)
library(stringr)
library(grid)

# Load processed data
data <- readRDS("../../data/processed_data.rds")  # Adjust path if run from dashboard/

# Consistent color palette function for all plots
get_consistent_colors <- function(n_cols) {
  grDevices::colorRampPalette(RColorBrewer::brewer.pal(min(8, n_cols), "Set3"))(n_cols)
}

# UI (shinydashboard)
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "CSISG Credit Card Analysis", titleWidth = 320),
  dashboardSidebar(width = 320,
    div(style = "height: 100vh; overflow-y: auto; padding-right: 5px;",
      sidebarMenu(
        id = "tabs",
        menuItem("Market Landscape", tabName = "landscape", icon = icon("globe-asia")),
        menuItem("Competitive Overview", tabName = "overview", icon = icon("chart-bar")),
        menuItem("Satisfaction Drivers", tabName = "drivers", icon = icon("thumbs-up")),
        menuItem("Merchant & Customer", tabName = "merchant", icon = icon("users")),
        menuItem("Download Plots", tabName = "download", icon = icon("download"))
      ),
      selectizeInput("issuer", "Issuer", choices = sort(unique(data$Issuer)), multiple = TRUE, selected = head(sort(unique(data$Issuer)), 11), options = list(plugins = list("remove_button")), width = "100%"),
      sliderInput("age_range", "Age Range", min = min(data$age, na.rm = TRUE), max = max(data$age, na.rm = TRUE), value = c(25, 45)),
      selectInput("pincome", "Personal Income", choices = c("Under SGD 2K" = 1, "SGD 2K - Under SGD 3K" = 2, "SGD 3K - Under SGD 4K" = 3, "SGD 4K - Under SGD 6K" = 4, "SGD 6K - Under SGD 8K" = 5, "SGD 8K - Under SGD 10K" = 6, "SGD 10K - Under SGD 15K" = 7, "SGD 15K - Under SGD 20K" = 8, "SGD 20K or over" = 9), multiple = TRUE, selected = 1:9),
      selectInput("income", "Household Income", choices = c("Under SGD 2K" = 1, "SGD 2K - Under SGD 3K" = 2, "SGD 3K - Under SGD 4K" = 3, "SGD 4K - Under SGD 6K" = 4, "SGD 6K - Under SGD 8K" = 5, "SGD 8K - Under SGD 10K" = 6, "SGD 10K - Under SGD 15K" = 7, "SGD 15K - Under SGD 20K" = 8, "SGD 20K or over" = 9), multiple = TRUE, selected = 1:9),
      selectInput("gender", "Gender", choices = c("Male" = 1, "Female" = 2), multiple = TRUE, selected = c(1,2)),
      selectInput("race", "Race", choices = c("Chinese" = 1, "Malay" = 2, "Indian" = 3, "Eurasian" = 4, "Others" = 5), multiple = TRUE, selected = 1:5),
      selectInput("work", "Employment Status", choices = c("Working full-time" = 1, "Working part-time" = 2, "Homemaker" = 3, "Retired" = 4, "Student" = 5, "Unemployed" = 6), multiple = TRUE, selected = 1:6),
      selectInput("educat", "Education", choices = c("None" = 1, "PSLE & below" = 2, "GCE N Level" = 3, "GCE O Level" = 4, "GCE A Level / Post-Secondary" = 5, "ITE / Vocational Institute" = 6, "Polytechnic Diploma / Professional Cert" = 7, "University Degree" = 8, "University Post-Graduate Degree" = 9), multiple = TRUE, selected = 1:9),
      selectInput("marital", "Marital Status", choices = c("Single" = 1, "Married" = 2, "Divorced" = 3, "Widowed" = 4, "Separated" = 5, "Domestic Partnership" = 6), multiple = TRUE, selected = 1:6),
      selectInput("childsupp", "Children Dependent", choices = c("0" = 1, "1" = 2, "2" = 3, "3" = 4, "4 or more" = 5), multiple = TRUE, selected = 1:5),
      selectInput("house", "Housing Type", choices = c("HDB 1-2 RM" = 1, "HDB 3 RM" = 2, "HDB 4 RM" = 3, "HDB 5 RM / Executive Flat" = 4, "Condo / Pte Apartment" = 5, "Landed Property" = 6), multiple = TRUE, selected = 1:6),
      checkboxGroupInput("credit_card_products", "Credit Card Products You Have:",
                        choices = c(
                          "Current account or savings account" = "vn_7002_T24_1",
                          "Investment" = "vn_7002_T24_2", 
                          "Insurance" = "vn_7002_T24_3",
                          "Loans" = "vn_7002_T24_4",
                          "Others" = "vn_7002_T24_5"
                        ),
                        selected = character(0),
                        inline = FALSE),
      actionButton("reset_filters", "Reset Filters", icon = icon("rotate-left"), width = "85%"),
      div(style = "height: 20px;")  # Bottom spacing
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(
      ".shiny-plot-output{overflow:hidden; padding:10px; box-sizing:border-box;} .tab-content{overflow:hidden;}\n" ,
      ".skin-blue .main-sidebar .sidebar{padding:10px 10px 20px; overflow-y: auto; max-height: 100vh; scrollbar-width: none; -ms-overflow-style: none;}\n",
      ".skin-blue .main-sidebar .sidebar::-webkit-scrollbar{display: none;}\n",
      ".main-sidebar .form-group{margin-bottom:8px;}\n",
      ".main-sidebar .selectize-input{min-height:30px;}\n",
      ".main-sidebar .irs{margin-top:4px;}\n",
      ".selectize-input{background-color:#000;border-color:#333;}\n",
      ".selectize-input input{color:#ddd;}\n",
      ".selectize-control.multi .selectize-input>div{background:#222;border-color:#444;color:#fff;}\n",
      ".main-sidebar .sidebar-menu{margin-bottom:15px;}\n",
      ".main-sidebar .form-group label{font-size:12px; margin-bottom:4px;}\n",
      ".main-sidebar select, .main-sidebar .selectize-input{font-size:11px;}\n",
      ".main-sidebar .btn{font-size:11px; padding:6px 12px;}\n",
      "/* Hide scrollbars for entire dashboard */\n",
      "html, body{scrollbar-width: none; -ms-overflow-style: none;}\n",
      "html::-webkit-scrollbar, body::-webkit-scrollbar{display: none;}\n",
      ".content-wrapper, .main-content{scrollbar-width: none; -ms-overflow-style: none;}\n",
      ".content-wrapper::-webkit-scrollbar, .main-content::-webkit-scrollbar{display: none;}\n",
      ".tab-content{scrollbar-width: none; -ms-overflow-style: none;}\n",
      ".tab-content::-webkit-scrollbar{display: none;}\n",
      ".fluid-row{scrollbar-width: none; -ms-overflow-style: none;}\n",
      ".fluid-row::-webkit-scrollbar{display: none;}\n"
      
    ))),
    tabItems(
      tabItem(tabName = "landscape",
        fluidRow(
          column(6, plotOutput("overall_market_pie", height = "360px")),
          column(6, plotOutput("overall_satis_hist", height = "360px"))
        ),
        br(),
        fluidRow(
          column(6, tagList(uiOutput("issuer_cardmix_ui"), plotOutput("market_share_plot", height = "360px"))),
          column(6, tagList(uiOutput("issuer_satis_ui"), plotOutput("issuer_satis_plot", height = "360px")))
        )
      ),
      tabItem(tabName = "overview",
        fluidRow(column(12, plotOutput("core_metrics_plot", height = "380px"))),
        fluidRow(column(12, div(style = "padding:10px;", plotOutput("spend_dist_plot", height = "380px")))),
        br(),
        # Banking Channel Usage by Issuer
        fluidRow(
          column(12, 
            h4("Banking Channel Usage by Issuer", style = "text-align: center; margin-bottom: 15px; color: #34495e; font-weight: bold;")
          )
        ),
        fluidRow(column(12, plotOutput("channel_usage_plot", height = "400px"))),
        br(),
        # Number of Competing Cards by Issuer
        fluidRow(
          column(12, 
            h4("Number of Competing Cards by Issuer", style = "text-align: center; margin-bottom: 15px; color: #34495e; font-weight: bold;")
          )
        ),
        fluidRow(column(12, plotOutput("competing_cards_histogram", height = "400px"))),
        br(),
        # Most Frequently Used Cards by Issuer
        fluidRow(
          column(12, 
            h4("Most Frequently Used Cards by Issuer", style = "text-align: center; margin-bottom: 15px; color: #34495e; font-weight: bold;")
          )
        ),
        fluidRow(
          column(6,
            selectInput("frequent_cards_issuer", "Select Issuer:", 
              choices = sort(unique(data$Issuer)),
              selected = "OCBC CREDIT CARDS",
              width = "100%"
            )
          ),
          column(6,
            selectInput("frequent_cards_type", "Card Display Type:", 
              choices = c("Card Name" = "vn_7002_T21_creditcard", "Card Code" = "vn_7002_T21_creditcard_code"),
              selected = "vn_7002_T21_creditcard",
              width = "100%"
            )
          )
        ),
        fluidRow(column(12, plotOutput("frequent_cards_pie", height = "500px"))),
        fluidRow(column(12, div(style = "height: 120px;")))
      ),
      tabItem(tabName = "drivers",
        # Satisfaction & Expectations Table Analysis
        fluidRow(
          column(12, 
            h3("Satisfaction & Expectations Analysis", style = "text-align: center; margin-bottom: 20px; color: #2c3e50;")
          )
        ),
        fluidRow(
          column(12,
                         selectInput("table_column_selector", "Select Variable for Table Analysis:", 
               choices = c(
                 "Customer Satisfaction" = "satis",
                 "Confirmation to Expectations" = "confirm",
                 "Close to Ideal Product/Service" = "ideal",
                 "Expectations about Overall Quality" = "overallx",
                 "Expectations about Customization" = "customx",
                 "Expectations about Reliability" = "wrongx",
                 "Overall Product Quality" = "poverq",
                 "Overall Service Quality" = "soverq",
                 "Product Customization" = "pcustq",
                 "Service Customization" = "scustq",
                 "Product Reliability" = "pwrongq",
                 "Service Reliability" = "swrongq",
                 "Price Given Quality" = "pq",
                 "Quality Given Price" = "qp",
                 "Willing to say positive things" = "Q21"
               ),
              selected = "satis",
              width = "400px"
            )
          )
        ),
        fluidRow(
          column(12, 
            h4("Statistical Summary Table", style = "text-align: center; margin-bottom: 15px; color: #34495e; font-weight: bold;")
          )
        ),
        fluidRow(
          column(12, 
            tableOutput("satisfaction_table")
          )
        ),
        br(),
        # Service Channel Ratings and Merchant Tie-ups Side by Side (2:1 ratio)
        fluidRow(
          column(8, 
            h4("Service Channel Ratings by Issuer", style = "text-align: center; margin-bottom: 15px; color: #34495e; font-weight: bold;")
          ),
          column(4, 
            h4("Merchant Tie-ups vs Customer Satisfaction", style = "text-align: center; margin-bottom: 15px; color: #34495e; font-weight: bold;")
          )
        ),
        fluidRow(
          column(8, 
            plotOutput("service_channel_dot_plot", height = "400px")
          ),
          column(4, 
            plotOutput("scatter_plot", height = "400px")
          )
        ),
        br(),
        # Brand Perception Radar Chart
        fluidRow(
          column(12, 
            h4("Brand Perception Radar Chart", style = "text-align: center; margin-bottom: 15px; color: #34495e; font-weight: bold;")
          )
        ),
        fluidRow(
          column(6,
            selectInput("radar_bank_selector", "Select Bank for Radar Chart:", 
              choices = sort(unique(data$Issuer)),
              selected = "OCBC CREDIT CARDS",
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(12, 
            plotOutput("radar_plot")
          )
        ),
        br()
      ),
      tabItem(tabName = "merchant",
        # Category-wise Analysis Section
        fluidRow(
          column(12, 
            h3("Category-wise Analysis", style = "text-align: center; margin-bottom: 20px; color: #2c3e50;")
          )
        ),
        fluidRow(
          column(12,
            selectInput("category_selector", "Select Product Category:", 
              choices = c(
                "Dining" = "L1_1",
                "Travel" = "L1_2", 
                "Groceries" = "L1_3",
                "Shopping" = "L1_4",
                "Entertainment" = "L1_5",
                "Transport" = "L1_6",
                "Utilities" = "L1_7",
                "Insurance" = "L1_8",
                "Education" = "L1_9",
                "Healthcare" = "L1_10",
                "Investment" = "L1_11",
                "Others" = "L1_12"
              ),
              selected = "L1_1",
              width = "300px"
            )
          )
        ),
        fluidRow(
          column(6, 
            plotOutput("category_issuer_pie", height = "400px")
          ),
          column(6, 
            plotOutput("category_card_pie", height = "400px")
          )
        ),
        br(),
        # Privilege Categories Satisfaction Section
        fluidRow(
          column(12, 
            h3("Privilege Categories Satisfaction", style = "text-align: center; margin-bottom: 20px; color: #2c3e50;")
          )
        ),
        fluidRow(
          column(12, 
            h4("Most Satisfying Privilege Categories by Issuer", style = "text-align: center; margin-bottom: 15px; color: #34495e; font-weight: bold;")
          )
        ),
        fluidRow(
          column(12, 
            plotOutput("privilege_categories_plot", height = "500px")
          )
        ),
        br(),
        # Filter-based Analysis Section
        fluidRow(
          column(12, 
            h3("Filter-based Analysis", style = "text-align: center; margin-bottom: 20px; color: #2c3e50;")
          )
        ),
        fluidRow(
          column(6,
            selectInput("filter_category", "Product Category of Recent Purchase:", 
              choices = c(
                "Dining" = "L1_1",
                "Travel" = "L1_2", 
                "Groceries" = "L1_3",
                "Shopping" = "L1_4",
                "Entertainment" = "L1_5",
                "Transport" = "L1_6",
                "Utilities" = "L1_7",
                "Insurance" = "L1_8",
                "Education" = "L1_9",
                "Healthcare" = "L1_10",
                "Investment" = "L1_11",
                "Others" = "L1_12"
              ),
              selected = "L1_1",
              width = "100%"
            )
          ),
          column(6,
            selectInput("filter_bank", "Select Bank:", 
              choices = sort(unique(data$Issuer)),
              selected = "OCBC CREDIT CARDS",
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(12, 
            plotOutput("filtered_card_distribution", height = "400px")
          )
        ),
        br(),
        # Existing plots
                 plotOutput("ridge_plot")
      ),
      tabItem(tabName = "download",
        fluidRow(
          column(12, 
            h3("Download Dashboard Plots", style = "text-align: center; margin-bottom: 30px;"),
            div(style = "text-align: center; margin-bottom: 30px;",
              selectInput("plot_selector", "Select Plot to Download:", 
                choices = c(
                  "Core Metrics by Issuer" = "core_metrics_plot",
                  "Monthly Spend Distribution by Issuer" = "spend_dist_plot",
                  "Banking Channel Usage by Issuer" = "channel_usage_plot",
                  "Number of Competing Cards by Issuer" = "competing_cards_histogram",
                  "Most Frequently Used Cards by Issuer" = "frequent_cards_pie",
                  "Overall Market Share by Issuer" = "overall_market_pie",
                  "Overall Customer Satisfaction" = "overall_satis_hist",
                  "Card Mix by Issuer" = "market_share_plot",
                  "Issuer-specific Satisfaction" = "issuer_satis_plot",
                                     "Satisfaction & Expectations Table" = "satisfaction_table",
                  "Brand Perceptions Heatmap" = "heatmap_plot",
                  "Service Channel Ratings by Issuer" = "service_channel_dot_plot",
                  "Merchant Tie-ups vs Satisfaction" = "scatter_plot",
                  "Brand Perception Radar" = "radar_plot",
                  "Category-wise Issuer Split" = "category_issuer_pie",
                  "Category-wise Card Split" = "category_card_pie",
                  "Most Satisfying Privilege Categories by Issuer" = "privilege_categories_plot",
                  "Filtered Card Distribution" = "filtered_card_distribution",
                  "Satisfaction by Age Group" = "ridge_plot",
                  "Competing Cards Distribution" = "competing_cards_plot"
                ),
                selected = "core_metrics_plot",
                width = "400px"
              )
            ),
            div(style = "text-align: center; margin-bottom: 20px;",
              downloadButton("download_plot", "Download Plot", 
                style = "background-color: #337ab7; color: white; border: none; padding: 10px 20px; font-size: 16px; border-radius: 5px;"
              )
            ),
            div(style = "text-align: center;",
              plotOutput("preview_plot", height = "500px")
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Defaults for reset
  defaults <- list(
    issuer = head(sort(unique(data$Issuer)), 7),
    age_range = c(25, 45),
    pincome = 1:9,
    income = 1:9,
    gender = c(1, 2),
    race = 1:5,
    work = 1:6,
    educat = 1:9,
    marital = 1:6,
    childsupp = 1:5,
    house = 1:6,
    credit_card_products = character(0)
  )
  # Removed dynamic layout; using shinydashboard static layout
  
  # Reactive filtered data
  filtered_data <- reactive({
    df <- data %>%
      filter(Issuer %in% input$issuer,
             age >= input$age_range[1] & age <= input$age_range[2],
             pincome %in% input$pincome,
             income %in% input$income,
             gender %in% input$gender,
             race %in% input$race,
             work %in% input$work,
             educat %in% input$educat,
             marital %in% input$marital,
             childsupp %in% input$childsupp,
             house %in% input$house)
    
    # Apply binary filtering for credit card products
    selected_products <- input$credit_card_products
    if (length(selected_products) > 0) {
      # Filter for rows where selected products = 1
      for (product in selected_products) {
        df <- df %>% filter(!!sym(product) == 1);
      }
      
      # Filter for rows where unselected products = 0
      all_products <- c("vn_7002_T24_1", "vn_7002_T24_2", "vn_7002_T24_3", "vn_7002_T24_4", "vn_7002_T24_5")
      unselected_products <- setdiff(all_products, selected_products)
      for (product in unselected_products) {
        df <- df %>% filter(!!sym(product) == 0);
      }
    }
    
    df %>%
      mutate(
        Issuer = fct_reorder(Issuer, satis, .fun = mean, .desc = FALSE, .na_rm = TRUE),
        spend_bin = factor(spend_bin, levels = c("<500", "500-1000", "1000-2000", "2000-5000", "5000+"))
      )
  })

  # Reset filters handler
  observeEvent(input$reset_filters, {
    updateSelectInput(session = getDefaultReactiveDomain(), inputId = "pincome", selected = defaults$pincome)
    updateSelectInput(session = getDefaultReactiveDomain(), inputId = "income", selected = defaults$income)
    updateSelectInput(session = getDefaultReactiveDomain(), inputId = "gender", selected = defaults$gender)
    updateSelectInput(session = getDefaultReactiveDomain(), inputId = "race", selected = defaults$race)
    updateSelectInput(session = getDefaultReactiveDomain(), inputId = "work", selected = defaults$work)
    updateSelectInput(session = getDefaultReactiveDomain(), inputId = "educat", selected = defaults$educat)
    updateSelectInput(session = getDefaultReactiveDomain(), inputId = "marital", selected = defaults$marital)
    updateSelectInput(session = getDefaultReactiveDomain(), inputId = "childsupp", selected = defaults$childsupp)
    updateSelectInput(session = getDefaultReactiveDomain(), inputId = "house", selected = defaults$house)
    updateCheckboxGroupInput(session = getDefaultReactiveDomain(), inputId = "credit_card_products", selected = defaults$credit_card_products)
    updateSliderInput(session = getDefaultReactiveDomain(), inputId = "age_range", value = defaults$age_range)
    updateSelectInput(session = getDefaultReactiveDomain(), inputId = "issuer", selected = defaults$issuer)
  })

  # UI for issuer subset (max 3)
  output$issuer_cardmix_ui <- renderUI({
    choices <- sort(unique(data$Issuer))
    default_choice <- if ("OCBC CREDIT CARDS" %in% choices) "OCBC CREDIT CARDS" else head(choices, 1)
    selectInput("issuer_cardmix", "Select Issuer for Card Mix (pie):",
                choices = choices, selected = default_choice)
  })

  # UI for issuer-specific satisfaction
  output$issuer_satis_ui <- renderUI({
    choices <- sort(unique(data$Issuer))
    selectInput("issuer_satis", "Select Issuer for Satisfaction Distribution:",
                choices = choices, selected = choices[1])
  })

  # issuer_spend_ui no longer rendered dynamically under shinydashboard
  
  # Custom theme with generous margins and facet spacing
  custom_theme <- theme_economist() +
    theme(
      legend.position = "bottom",
      plot.margin = margin(15, 20, 15, 20),
      panel.spacing = unit(10, "pt")
    )
  
  # Tab 1: Competitive Overview
  output$core_metrics_plot <- renderPlot({
    df <- filtered_data()
    if (!all(c("Issuer","satis","repur","recomm") %in% names(df))) return(NULL)
    data_long <- df %>%
      select(Issuer, satis, repur, recomm) %>%
      tidyr::pivot_longer(cols = c(satis, repur, recomm), names_to = "metric", values_to = "score") %>%
      mutate(metric = case_when(
        metric == "satis" ~ "Satisfaction",
        metric == "repur" ~ "Repurchase",
        metric == "recomm" ~ "Recommendation",
        TRUE ~ metric
      ))
    ggplot(data_long, aes(Issuer, score, fill = metric)) +
      geom_violin(color = NA, trim = TRUE, alpha = 0.85) +
      geom_boxplot(width = 0.12, outlier.alpha = 0.25, aes(fill = metric)) +
      coord_flip() +
      facet_wrap(~ metric, nrow = 1) +
      scale_fill_brewer(palette = "Set2", name = "Metric") +
      labs(title = "Core Metrics by Issuer", x = NULL, y = "Score (1–10)") +
      custom_theme +
      theme(legend.position = "bottom")
  })

  # Overall market share by issuer (pie)
  output$overall_market_pie <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No data available"))
    pie_df <- df %>% count(Issuer, name = "n") %>%
      mutate(pct = n / sum(n)) %>% arrange(desc(pct))
    n_cols <- nrow(pie_df)
    cols <- get_consistent_colors(n_cols)
    ggplot(pie_df, aes(x = "", y = pct, fill = Issuer)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      scale_fill_manual(values = cols, labels = paste0(pie_df$Issuer, " — ", scales::percent(pie_df$pct)), name = "Issuer") +
      labs(title = "Overall Market Share by Issuer", x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text = element_blank(), axis.title = element_blank(),
            panel.grid = element_blank(), plot.margin = margin(10, 16, 10, 16),
            legend.position = "right")
  })

  output$market_share_plot <- renderPlot({
    df <- filtered_data()
    sel <- input$issuer_cardmix
    if (!is.null(sel)) df <- df %>% filter(Issuer == sel)
    validate(need(nrow(df) > 0, "No data for selected issuer"))
    pie_df <- df %>% count(Q34_Creditcard_code, name = "n") %>%
      mutate(pct = n / sum(n)) %>% arrange(desc(pct))
    pie_df$card <- pie_df$Q34_Creditcard_code
    pie_df$label <- paste0(pie_df$card, " — ", scales::percent(pie_df$pct))
    pie_df$card <- factor(pie_df$card, levels = pie_df$card)  # preserve order

    # Build a vivid color palette long enough for all slices
    n_cols <- nrow(pie_df)
    cols <- get_consistent_colors(n_cols)

    ggplot(pie_df, aes(x = "", y = pct, fill = card)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      scale_fill_manual(values = cols, labels = pie_df$label, name = paste0("Cards of ", sel)) +
      labs(title = paste0("Card Mix – ", sel), x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5),
            axis.text = element_blank(), axis.title = element_blank(),
            panel.grid = element_blank(), plot.margin = margin(5,5,5,5),
            legend.position = "right",
            legend.title = element_text(face = "bold"),
            legend.text = element_text(size = 10))
  })
  
  output$satis_dist_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = satis)) +
      geom_histogram(bins = 30, fill = "#2c7fb8", color = "white") +
      labs(title = "Customer Satisfaction Distribution",
           x = "Satisfaction (1–10)", y = "Count") +
      custom_theme +
      theme(plot.margin = margin(10, 15, 10, 15))
  })

  # Overall satisfaction across all issuers (counts 1..10)
  output$overall_satis_hist <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No data available"))
    ggplot(df, aes(x = round(satis))) +
      geom_histogram(binwidth = 1, boundary = 0.5, closed = "right",
                     fill = "#4E79A7", color = "white") +
      scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
      labs(title = "Overall Customer Satisfaction (All Issuers)",
           x = "Satisfaction score (1–10)", y = "Number of respondents") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text = element_text(size = 11),
            axis.title = element_text(size = 12),
            plot.margin = margin(10, 16, 10, 16))
  })

  # Issuer-specific satisfaction distribution (density + rug)
  output$issuer_satis_plot <- renderPlot({
    df <- filtered_data()
    sel <- input$issuer_satis
    if (!is.null(sel)) df <- df %>% filter(Issuer == sel)
    validate(need(nrow(df) > 0, "No data for selected issuer"))
    ggplot(df, aes(x = satis)) +
      geom_histogram(aes(y = after_stat(density)), binwidth = 1, boundary = 0.5, closed = "right",
                     fill = "#59A14F", color = "white", alpha = 0.85) +
      geom_density(color = "#2C6B2F", linewidth = 1) +
      scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
      labs(title = paste0("Satisfaction Distribution – ", sel),
           x = "Satisfaction score (1–10)", y = "Density") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text = element_text(size = 11),
            axis.title = element_text(size = 12),
            plot.margin = margin(10, 16, 10, 16))
  })
  
  output$violin_plot <- renderPlot({
    data_long <- filtered_data() %>% pivot_longer(cols = c(satis, repur, recomm), names_to = "metric", values_to = "value")
    ggplot(data_long, aes(x = Issuer, y = value, fill = metric)) +
      geom_violin(trim = TRUE, alpha = 0.8) +
      labs(title = "Satisfaction, Reuse, and Recommend by Issuer",
           x = "Issuer", y = "Score (1–10)", fill = "Metric") +
      scale_fill_manual(values = get_consistent_colors(3)) +
      custom_theme +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
      scale_x_discrete(labels = function(x) str_wrap(x, 12)) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1),
            legend.position = "bottom",
            legend.text = element_text(size = 9),
            plot.margin = margin(10, 15, 10, 15))
  })
  
  output$spend_dist_plot <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No data available"))
    # Get unique spend bins for color palette
    unique_spend_bins <- unique(df$spend_bin)
    n_cols <- length(unique_spend_bins)
    cols <- get_consistent_colors(n_cols)
    
    ggplot(df, aes(x = spend_bin, fill = spend_bin)) +
      geom_bar(color = "white") +
      scale_fill_manual(values = cols, guide = "none") +
      facet_wrap(~Issuer, nrow = 3, scales = "free_y") +
      labs(title = "Monthly Spend Distribution by Issuer",
           x = "Monthly spend band (SGD)", y = "Count") +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        strip.text = element_text(size = 10, face = "bold", hjust = 0.5),
        strip.placement = "outside",
        plot.margin = margin(12, 14, 20, 14)
      )
  })

  output$spend_dist_pct_plot <- renderPlot({
    df <- filtered_data()
    df <- df %>% filter(!is.na(spend_bin)) %>%
      count(Issuer, spend_bin) %>%
      group_by(Issuer) %>% mutate(pct = n/sum(n))
    
    # Get unique issuers for color palette
    unique_issuers <- unique(df$Issuer)
    n_cols <- length(unique_issuers)
    cols <- get_consistent_colors(n_cols)
    
    ggplot(df, aes(spend_bin, pct, fill = Issuer)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = cols) +
      scale_y_continuous(labels = percent) +
      labs(title = "Monthly Spend Share by Issuer",
           x = "Monthly spend band (SGD)", y = "Share within issuer", fill = "Issuer") +
      custom_theme +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  # Banking Channel Usage by Issuer
  output$channel_usage_plot <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No data available"))
    
    # Check if required columns exist
    required_cols <- c("Issuer", "vn_7002_T25_1", "vn_7002_T25_2", "vn_7002_T25_3", "vn_7002_T25_99")
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      validate(need(FALSE, paste("Missing required columns:", paste(missing_cols, collapse = ", "))))
    }
    
    # Prepare data for channel usage
    channel_data <- df %>%
      select(Issuer, vn_7002_T25_1, vn_7002_T25_2, vn_7002_T25_3, vn_7002_T25_99) %>%
      pivot_longer(cols = -Issuer, names_to = "channel", values_to = "used") %>%
      filter(!is.na(used)) %>%
      group_by(Issuer, channel) %>%
      summarise(
        total_users = n(),
        users_used = sum(used == 1, na.rm = TRUE),
        usage_rate = users_used / total_users,
        .groups = "drop"
      ) %>%
      mutate(
        channel_label = case_when(
          channel == "vn_7002_T25_1" ~ "Contact Centre",
          channel == "vn_7002_T25_2" ~ "Mobile App",
          channel == "vn_7002_T25_3" ~ "Internet Banking",
          channel == "vn_7002_T25_99" ~ "None of the above",
          TRUE ~ channel
        )
      )
    
    # Get unique issuers for color palette
    unique_issuers <- unique(channel_data$Issuer)
    n_cols <- length(unique_issuers)
    cols <- get_consistent_colors(n_cols)
    
    # Create the plot
    ggplot(channel_data, aes(x = reorder(channel_label, usage_rate), y = usage_rate, fill = Issuer)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = cols) +
      scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
      labs(
        title = "Banking Channel Usage by Issuer",
        subtitle = "Proportion of customers using each banking channel",
        x = "Banking Channel",
        y = "Usage Rate",
        fill = "Issuer"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray60"),
        axis.title = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 15, 10, 15)
      ) +
      coord_flip()
  })
  
  # Tab 2: Satisfaction Drivers
  output$heatmap_plot <- renderPlot({
    oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar))
    par(mar = c(3.5, 3.5, 3, 1))
    cor_vars <- filtered_data() %>% select(satis, vn_7002_T01:vn_7002_T10)
    cor_matrix <- cor(cor_vars, use = "complete.obs")
    corrplot(cor_matrix, method = "shade", tl.cex = 0.8)
  })
  
  output$scatter_plot <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No data available"))
    
    ggplot(df, aes(x = vn_7002_T08, y = satis)) +
      geom_point(alpha = 0.6, size = 2, color = "#2E86AB") +
      geom_smooth(method = "lm", se = TRUE, color = "#E74C3C", linewidth = 1.5) +
      labs(title = "Merchant Tie-ups vs Customer Satisfaction",
           x = "Merchant Tie-ups Rating (1-10)",
           y = "Customer Satisfaction (1-10)",
           subtitle = "Higher merchant tie-ups ratings correlate with higher satisfaction") +
      scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
      scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray60"),
            axis.title = element_text(size = 11),
            panel.grid.minor = element_blank(),
            plot.margin = margin(10, 15, 10, 15))
  })
  
                 # Satisfaction & Expectations Table Analysis
    output$satisfaction_table <- renderTable({
      df <- filtered_data()
      selected_column <- input$table_column_selector
      
      if (is.null(selected_column) || !(selected_column %in% names(df))) return(NULL)
      
      # Get the human-readable name for the selected column
      column_names <- c(
        "satis" = "Customer Satisfaction",
        "confirm" = "Confirmation to Expectations",
        "ideal" = "Close to Ideal Product/Service",
        "overallx" = "Expectations about Overall Quality",
        "customx" = "Expectations about Customization",
        "wrongx" = "Expectations about Reliability",
        "poverq" = "Overall Product Quality",
        "soverq" = "Overall Service Quality",
        "pcustq" = "Product Customization",
        "scustq" = "Service Customization",
        "pwrongq" = "Product Reliability",
        "swrongq" = "Service Reliability",
        "pq" = "Price Given Quality",
        "qp" = "Quality Given Price",
        "Q21" = "Willing to say positive things"
      )
      
      column_name <- column_names[selected_column]
      
      # Compute issuer-level statistics for the selected variable
      table_data <- df %>%
        group_by(Issuer) %>%
        summarise(
          Average = round(mean(!!sym(selected_column), na.rm = TRUE), 2),
          Median = round(median(!!sym(selected_column), na.rm = TRUE), 2),
          Mode = as.numeric(names(sort(table(!!sym(selected_column)), decreasing = TRUE)[1])),
          Observations = n(),
          `% ≤ 5` = round(100 * sum(!!sym(selected_column) <= 5, na.rm = TRUE) / n(), 1),
          .groups = "drop") %>%
        arrange(desc(Average))
      
      if (nrow(table_data) == 0) {
        return(data.frame(Message = "No data available for selected variable"))
      }
      
      # Rename columns for better display
      colnames(table_data) <- c("Bank", "Average", "Median", "Mode", "Observations", "% ≤ 5")
      
      return(table_data)
    }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")
  
  output$radar_plot <- renderPlot({
    df <- filtered_data()
    selected_bank <- input$radar_bank_selector
    
    validate(
      need(nrow(df) > 0, "No data available"),
      need(!is.null(selected_bank), "Please select a bank")
    )
    
    # Filter data for selected bank only
    df_selected <- df %>% filter(Issuer == selected_bank)
    
    if (nrow(df_selected) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                       label = paste0("No data available for ", selected_bank), size = 5) +
               theme_void())
    }
    
    # Define the radar variables and their labels
    vars <- paste0("vn_7002_T", sprintf("%02d", 1:10))
    var_labels <- c(
      "Feel comfortable and safe when using",
      "Card benefits are presented clearly", 
      "Brand image complements personality",
      "Has a good reputation",
      "Flexibility of policies",
      "Ease of accessing card balance",
      "Card benefits (cashbacks, rewards)",
      "Merchant tie-ups meeting needs",
      "Ease of reward redemption",
      "Redemption catalogue meeting needs"
    )
    
    # Compute means for the selected bank
    radar_means <- df_selected %>%
      summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

    # Prepare max/min rows expected by fmsb::radarchart
    max_min <- data.frame(matrix(c(rep(10, length(vars)), rep(0, length(vars))),
                                 nrow = 2, byrow = TRUE))
    colnames(max_min) <- vars

    # Bind with the bank mean values
    radar_values <- radar_means %>% select(all_of(vars))
    radar_df <- rbind(max_min, radar_values)

    # Use a single color for the selected bank
    bank_color <- "#2E86AB"  # Professional blue color
    
    # Draw radar chart with simple, clear formatting
    radarchart(radar_df,
               axistype = 1,
               pcol = bank_color,
               plty = 0,  # No lines
               plwd = 0.1,  # Very thin line width
               cglty = 1,
               cglcol = "gray",
               cglwd = 0.8,
               vlcex = 0.8,
               caxislabels = seq(0, 10, 2),
               calcex = 0.7,
               title = paste0("Brand Perception Profile - ", selected_bank),
               vlabels = c("T01", "T02", "T03", "T04", "T05", "T06", "T07", "T08", "T09", "T10"))
    
    # Add legend with parameter meanings
    legend("topright", 
           legend = c("T01: Feel comfortable and safe when using",
                     "T02: Card benefits are presented clearly",
                     "T03: Brand image complements personality",
                     "T04: Has a good reputation",
                     "T05: Flexibility of policies",
                     "T06: Ease of accessing card balance",
                     "T07: Card benefits (cashbacks, rewards)",
                     "T08: Merchant tie-ups meeting needs",
                     "T09: Ease of reward redemption",
                     "T10: Redemption catalogue meeting needs"),
           cex = 1,
           bty = "n",
           x.intersp = 0.5,
           y.intersp = 0.8)
  })
  
  # Tab 3: Merchant and Customer Insights
  # Category-wise Analysis Section
  output$category_issuer_pie <- renderPlot({
    df <- filtered_data()
    category_col <- input$category_selector
    
    if (is.null(category_col) || !(category_col %in% names(df))) return(NULL)
    
    # Filter data for the selected category (where L1_* = 1)
    category_data <- df %>% 
      filter(!!sym(category_col) == 1) %>%
      count(Issuer, name = "n") %>%
      mutate(pct = n / sum(n)) %>%
      arrange(desc(pct))
    
    if (nrow(category_data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available for selected category", size = 5) +
               theme_void())
    }
    
    # Create color palette
    n_cols <- nrow(category_data)
    cols <- get_consistent_colors(n_cols)
    
    # Get category name for title
    category_names <- c(
      "L1_1" = "Dining", "L1_2" = "Travel", "L1_3" = "Groceries", "L1_4" = "Shopping",
      "L1_5" = "Entertainment", "L1_6" = "Transport", "L1_7" = "Utilities", "L1_8" = "Insurance",
      "L1_9" = "Education", "L1_10" = "Healthcare", "L1_11" = "Investment", "L1_12" = "Others"
    )
    category_name <- category_names[category_col]
    
    ggplot(category_data, aes(x = "", y = pct, fill = Issuer)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      scale_fill_manual(values = cols, 
                        labels = paste0(category_data$Issuer, " — ", scales::percent(category_data$pct)), 
                        name = "Issuer") +
      labs(title = paste0(category_name, " Category\nIssuer Split"), 
           x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10, lineheight = 1.2),
            axis.text = element_blank(), 
            axis.title = element_blank(),
            panel.grid = element_blank(), 
            plot.margin = margin(5, 10, 5, 10),
            legend.position = "right",
            legend.title = element_text(face = "bold"))
  })
  
  # Category-wise Analysis: Credit card-wise split for bank with maximum share
  output$category_card_pie <- renderPlot({
    df <- filtered_data()
    category_col <- input$category_selector
    
    if (is.null(category_col) || !(category_col %in% names(df))) return(NULL)
    
    # Filter data for the selected category (where L1_* = 1)
    category_data <- df %>% 
      filter(!!sym(category_col) == 1) %>%
      count(Issuer, name = "n") %>%
      mutate(pct = n / sum(n)) %>%
      arrange(desc(pct))
    
    if (nrow(category_data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available for selected category", size = 5) +
               theme_void())
    }
    
    # Get the bank with maximum share
    max_share_bank <- category_data$Issuer[1]
    
    # Get card distribution for the bank with maximum share
    card_data <- df %>% 
      filter(Issuer == max_share_bank) %>%
      count(Q34_Creditcard_code, name = "n") %>%
      mutate(pct = n / sum(n)) %>%
      arrange(desc(pct))
    
    if (nrow(card_data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No card data available", size = 5) +
               theme_void())
    }
    
    # Create color palette
    n_cols <- nrow(card_data)
    cols <- get_consistent_colors(n_cols)
    
    # Get category name for title
    category_names <- c(
      "L1_1" = "Dining", "L1_2" = "Travel", "L1_3" = "Groceries", "L1_4" = "Shopping",
      "L1_5" = "Entertainment", "L1_6" = "Transport", "L1_7" = "Utilities", "L1_8" = "Insurance",
      "L1_9" = "Education", "L1_10" = "Healthcare", "L1_11" = "Investment", "L1_12" = "Others"
    )
    category_name <- category_names[category_col]
    
    ggplot(card_data, aes(x = "", y = pct, fill = Q34_Creditcard_code)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      scale_fill_manual(values = cols, 
                        labels = paste0(card_data$Q34_Creditcard_code, " — ", scales::percent(card_data$pct)), 
                        name = "Credit Card") +
      labs(title = paste0("Card Split\n", max_share_bank), 
           x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10, lineheight = 1.2),
            axis.text = element_blank(), 
            axis.title = element_blank(),
            panel.grid = element_blank(), 
            plot.margin = margin(5, 10, 5, 10),
            legend.position = "right",
            legend.title = element_text(face = "bold"))
  })
  
  # Filter-based Analysis: Credit card distribution based on category and bank selection
  output$filtered_card_distribution <- renderPlot({
    df <- filtered_data()
    selected_category <- input$filter_category
    selected_bank <- input$filter_bank
    
    if (is.null(selected_category) || is.null(selected_bank) || 
        !(selected_category %in% names(df))) return(NULL)
    
    # Filter data for the selected category and bank
    filtered_data_subset <- df %>% 
      filter(!!sym(selected_category) == 1, Issuer == selected_bank)
    
    if (nrow(filtered_data_subset) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = paste0("No data available for ", selected_bank, " in selected category"), 
                        size = 5) +
               theme_void())
    }
    
    # Get card distribution
    card_data <- filtered_data_subset %>%
      count(Q34_Creditcard_code, name = "n") %>%
      mutate(pct = n / sum(n)) %>%
      arrange(desc(pct))
    
    if (nrow(card_data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No card data available", size = 5) +
               theme_void())
    }
    
    # Create color palette
    n_cols <- nrow(card_data)
    cols <- get_consistent_colors(n_cols)
    
    # Get category name for title
    category_names <- c(
      "L1_1" = "Dining", "L1_2" = "Travel", "L1_3" = "Groceries", "L1_4" = "Shopping",
      "L1_5" = "Entertainment", "L1_6" = "Transport", "L1_7" = "Utilities", "L1_8" = "Insurance",
      "L1_9" = "Education", "L1_10" = "Healthcare", "L1_11" = "Investment", "L1_12" = "Others"
    )
    category_name <- category_names[selected_category]
    
    ggplot(card_data, aes(x = "", y = pct, fill = Q34_Creditcard_code)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      scale_fill_manual(values = cols, 
                        labels = paste0(card_data$Q34_Creditcard_code, " — ", scales::percent(card_data$pct)), 
                        name = "Credit Card") +
      labs(title = paste0("Credit Card Distribution for ", selected_bank, "\n(", category_name, " Category)"), 
           x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            axis.text = element_blank(), 
            axis.title = element_blank(),
            panel.grid = element_blank(), 
            plot.margin = margin(10, 16, 10, 16),
            legend.position = "right",
            legend.title = element_text(face = "bold"))
  })
  

  
  # Service Channel Ratings Dot Plot
  output$service_channel_dot_plot <- renderPlot({
    df <- filtered_data()
    required <- c("Issuer","vn_7002_T16","vn_7002_T18","vn_7002_T19")
    validate(
      need(nrow(df) > 0, "No data available"),
      need(all(required %in% names(df)), paste0("Required columns missing: ", paste(setdiff(required, names(df)), collapse=", ")))
    )
    
    # Prepare data for service channel ratings
    channel_data <- df %>%
      select(Issuer, vn_7002_T16, vn_7002_T18, vn_7002_T19) %>%
      pivot_longer(cols = -Issuer, names_to = "channel", values_to = "rating") %>%
      filter(!is.na(rating)) %>%
      group_by(Issuer, channel) %>%
      summarise(
        mean_rating = mean(rating, na.rm = TRUE),
        n_ratings = n(),
        .groups = "drop"
      ) %>%
      mutate(
        channel_label = case_when(
          channel == "vn_7002_T16" ~ "Contact Centre",
          channel == "vn_7002_T18" ~ "Mobile App",
          channel == "vn_7002_T19" ~ "Internet Banking",
          TRUE ~ channel
        )
      )
    
    # Get unique issuers for color palette
    unique_issuers <- unique(channel_data$Issuer)
    n_cols <- length(unique_issuers)
    cols <- get_consistent_colors(n_cols)
    
    # Create the dot plot
    ggplot(channel_data, aes(x = reorder(channel_label, mean_rating), y = mean_rating, color = Issuer)) +
      geom_point(size = 5, alpha = 0.8) +
      scale_color_manual(values = cols) +
      scale_y_continuous(limits = c(1, 10), breaks = 1:10) +
      labs(
        title = "Service Channel Ratings by Issuer",
        subtitle = "Mean customer experience ratings across different service channels",
        x = "Service Channel",
        y = "Mean Rating (1-10)",
        color = "Issuer"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray60"),
        axis.title = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 15, 10, 15)
      ) +
      coord_flip()
  })
  

  

  
  output$ridge_plot <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No data available"))
    # Derive age_group if missing
    if (!("age_group" %in% names(df)) && "age" %in% names(df)) {
      df <- df %>% mutate(
        age_group = cut(age,
                        breaks = c(-Inf, 24, 34, 44, 54, 64, Inf),
                        labels = c("<25", "25-34", "35-44", "45-54", "55-64", "65+"),
                        right = TRUE, ordered_result = TRUE)
      )
    }
    validate(
      need(all(c("satis","age_group","Issuer") %in% names(df)), "Required columns missing: satis, age_group (or age), Issuer")
    )
    df <- df %>% filter(!is.na(satis), !is.na(age_group), !is.na(Issuer))
    
    # Get unique issuers for color palette
    unique_issuers <- unique(df$Issuer)
    n_cols <- length(unique_issuers)
    cols <- get_consistent_colors(n_cols)
    
    ggplot(df, aes(x = satis, y = age_group, fill = Issuer)) +
      geom_density_ridges() +
      scale_fill_manual(values = cols) +
      labs(title = "Satisfaction Distribution by Age Group", x = "Satisfaction", y = "Age Group") +
      custom_theme +
      theme(plot.margin = margin(12, 16, 12, 16))
  })
  
  # removed sankey plot per request
  
  output$competing_cards_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = vn_7002_T20, fill = Issuer)) +
      geom_histogram(position = "dodge") +
      labs(title = "Competing Cards Histogram", x = "Score", y = "Count") +
      custom_theme +
      theme(plot.margin = margin(12, 16, 12, 16))
  })
  
  # Number of Competing Cards by Issuer
  output$competing_cards_histogram <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No data available"))
    
    # Check if required column exists
    if (!("vn_7002_T20" %in% names(df))) {
      validate(need(FALSE, "Required column vn_7002_T20 is missing"))
    }
    
    # Prepare data for competing cards histogram
    competing_data <- df %>%
      select(Issuer, vn_7002_T20) %>%
      filter(!is.na(vn_7002_T20)) %>%
      mutate(
        competing_cards = as.numeric(vn_7002_T20),
        competing_cards_grouped = case_when(
          competing_cards == 0 ~ "0 cards",
          competing_cards == 1 ~ "1 card",
          competing_cards == 2 ~ "2 cards",
          competing_cards == 3 ~ "3 cards",
          competing_cards >= 4 ~ "4+ cards",
          TRUE ~ "Unknown"
        )
      ) %>%
      filter(competing_cards_grouped != "Unknown")
    
    # Get unique issuers for color palette
    unique_issuers <- unique(competing_data$Issuer)
    n_cols <- length(unique_issuers)
    cols <- get_consistent_colors(n_cols)
    
    # Create the histogram
    ggplot(competing_data, aes(x = competing_cards, fill = Issuer)) +
      geom_histogram(binwidth = 1, position = "dodge", alpha = 0.8, color = "white") +
      scale_fill_manual(values = cols) +
      scale_x_continuous(breaks = 0:max(competing_data$competing_cards, na.rm = TRUE)) +
      labs(
        title = "Number of Competing Cards by Issuer",
        subtitle = "Distribution of additional credit cards owned (excluding supplementary cards)",
        x = "Number of Competing Cards",
        y = "Number of Customers",
        fill = "Issuer"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray60"),
        axis.title = element_text(size = 11),
        legend.position = "right",
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 15, 10, 15)
      )
  })
  
  # Most Frequently Used Cards by Issuer
  output$frequent_cards_pie <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No data available"))
    
    selected_issuer <- input$frequent_cards_issuer
    card_type <- input$frequent_cards_type
    
    # Check if required columns exist
    required_cols <- c("Issuer", card_type)
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      validate(need(FALSE, paste("Missing required columns:", paste(missing_cols, collapse = ", "))))
    }
    
    # Prepare data for frequent cards pie chart
    frequent_data <- df %>%
      filter(Issuer == selected_issuer) %>%
      select(all_of(card_type)) %>%
      filter(!is.na(!!sym(card_type))) %>%
      count(!!sym(card_type), name = "count") %>%
      mutate(
        percentage = (count / sum(count)) * 100,
        card_label = case_when(
          card_type == "vn_7002_T21_creditcard" ~ as.character(!!sym(card_type)),
          card_type == "vn_7002_T21_creditcard_code" ~ as.character(!!sym(card_type)),
          TRUE ~ as.character(!!sym(card_type))
        )
      ) %>%
      arrange(desc(count))
    
    if (nrow(frequent_data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available for selected issuer", size = 5) +
               theme_void())
    }
    
    # Create color palette
    n_cols <- nrow(frequent_data)
    cols <- get_consistent_colors(n_cols)
    
    # Create the pie chart
    ggplot(frequent_data, aes(x = "", y = count, fill = card_label)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      scale_fill_manual(values = cols, name = "Credit Card") +
      labs(
        title = paste0("Additional Cards Distribution - ", selected_issuer),
        subtitle = paste0("What other cards do ", selected_issuer, " customers own?"),
        x = NULL, 
        y = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray60"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(10, 15, 10, 15),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 10)
      )
  })
  
  # Privilege Categories Satisfaction by Issuer
  output$privilege_categories_plot <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No data available"))
    
    # Check if required column exists
    if (!("vn_7002_T11" %in% names(df))) {
      validate(need(FALSE, "Required column vn_7002_T11 is missing"))
    }
    
    # Prepare data for privilege categories
    privilege_data <- df %>%
      select(Issuer, vn_7002_T11) %>%
      filter(!is.na(vn_7002_T11)) %>%
      count(Issuer, vn_7002_T11, name = "count") %>%
      group_by(Issuer) %>%
      mutate(
        total = sum(count),
        percentage = (count / total) * 100
      ) %>%
      ungroup() %>%
      mutate(
        category_label = case_when(
          vn_7002_T11 == 1 ~ "Dining",
          vn_7002_T11 == 2 ~ "Food Delivery",
          vn_7002_T11 == 3 ~ "Retail Fashion",
          vn_7002_T11 == 4 ~ "Online Marketplace",
          vn_7002_T11 == 5 ~ "Travel",
          vn_7002_T11 == 6 ~ "Local Transport",
          vn_7002_T11 == 7 ~ "Groceries",
          vn_7002_T11 == 8 ~ "Petrol",
          vn_7002_T11 == 9 ~ "Entertainment",
          vn_7002_T11 == 10 ~ "Healthcare",
          vn_7002_T11 == 11 ~ "Data Communication & Online TV Streaming",
          TRUE ~ paste("Category", vn_7002_T11)
        )
      )
    
    # Get unique issuers for color palette
    unique_issuers <- unique(privilege_data$Issuer)
    n_cols <- length(unique_issuers)
    cols <- get_consistent_colors(n_cols)
    
    # Create the bar chart
    ggplot(privilege_data, aes(x = reorder(category_label, percentage), y = percentage, fill = Issuer)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = cols) +
      scale_y_continuous(labels = percent_format(scale = 1)) +
      labs(
        title = "Most Satisfying Privilege Categories by Issuer",
        subtitle = "Distribution of privilege categories customers are most satisfied with",
        x = "Privilege Category",
        y = "Percentage of Customers",
        fill = "Issuer"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray60"),
        axis.title = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "right",
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 15, 10, 15)
      ) +
      coord_flip()
  })
  
  # Download Plots Tab
  output$preview_plot <- renderPlot({
    # This will show the selected plot for preview
    plot_name <- input$plot_selector
    if (is.null(plot_name)) return(NULL)
    
    # Special handling for radar plot (base R plot)
    if (plot_name == "radar_plot") {
      df <- filtered_data()
      selected_bank <- input$radar_bank_selector
      
      if (nrow(df) > 0 && !is.null(selected_bank)) {
        df_selected <- df %>% filter(Issuer == selected_bank)
        
        if (nrow(df_selected) > 0) {
          vars <- paste0("vn_7002_T", sprintf("%02d", 1:10))
          radar_means <- df_selected %>%
            summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
          
          max_min <- data.frame(matrix(c(rep(10, length(vars)), rep(0, length(vars))),
                                     nrow = 2, byrow = TRUE))
          colnames(max_min) <- vars
          radar_values <- radar_means %>% select(all_of(vars))
          radar_df <- rbind(max_min, radar_values)
          
          bank_color <- "#2E86AB"
          
          radarchart(radar_df,
                     axistype = 1,
                     pcol = bank_color,
                     plty = 0,
                     plwd = 0.1,
                     cglty = 1,
                     cglcol = "gray",
                     cglwd = 0.8,
                     vlcex = 0.8,
                     caxislabels = seq(0, 10, 2),
                     calcex = 0.7,
                     title = paste0("Brand Perception Profile - ", selected_bank),
                     vlabels = c("T01", "T02", "T03", "T04", "T05", "T06", "T07", "T08", "T09", "T10"))
          
          legend("topright", 
                 legend = c("T01: Feel comfortable and safe when using",
                           "T02: Card benefits are presented clearly",
                           "T03: Brand image complements personality",
                           "T04: Has a good reputation",
                           "T05: Flexibility of policies",
                           "T06: Ease of accessing card balance",
                           "T07: Card benefits (cashbacks, rewards)",
                           "T08: Merchant tie-ups meeting needs",
                           "T09: Ease of reward redemption",
                           "T10: Redemption catalogue meeting needs"),
                 cex = 0.8,
                 bty = "n",
                 x.intersp = 0.5,
                 y.intersp = 0.8)
        } else {
          plot.new()
          text(0.5, 0.5, paste0("No data available for ", selected_bank), cex = 1.5)
        }
      } else {
        plot.new()
        text(0.5, 0.5, "Please select a bank", cex = 1.5)
      }
      return(NULL)  # Return NULL since we've already plotted
    }
    
    # Render the selected plot based on the input
    switch(plot_name,
           "core_metrics_plot" = {
             df <- filtered_data()
             if (!all(c("Issuer","satis","repur","recomm") %in% names(df))) return(NULL)
             data_long <- df %>%
               select(Issuer, satis, repur, recomm) %>%
               tidyr::pivot_longer(cols = c(satis, repur, recomm), names_to = "metric", values_to = "score")
             data_long <- data_long %>%
               mutate(metric = case_when(
                 metric == "satis" ~ "Satisfaction",
                 metric == "repur" ~ "Repurchase",
                 metric == "recomm" ~ "Recommendation",
                 TRUE ~ metric
               ))
             ggplot(data_long, aes(Issuer, score, fill = metric)) +
               geom_violin(color = NA, trim = TRUE, alpha = 0.85) +
               geom_boxplot(width = 0.12, outlier.alpha = 0.25, aes(fill = metric)) +
               coord_flip() +
               facet_wrap(~ metric, nrow = 1) +
               scale_fill_manual(values = get_consistent_colors(3), name = "Metric") +
               labs(title = "Core Metrics by Issuer", x = NULL, y = "Score (1–10)") +
               custom_theme +
               theme(legend.position = "bottom")
           },
           "spend_dist_plot" = {
             df <- filtered_data()
             validate(need(nrow(df) > 0, "No data available"))
             # Get unique spend bins for color palette
             unique_spend_bins <- unique(df$spend_bin)
             n_cols <- length(unique_spend_bins)
             cols <- get_consistent_colors(n_cols)
             
             ggplot(df, aes(x = spend_bin, fill = spend_bin)) +
               geom_bar(color = "white") +
               scale_fill_manual(values = cols, guide = "none") +
               facet_wrap(~Issuer, nrow = 3, scales = "free_y") +
               labs(title = "Monthly Spend Distribution by Issuer",
                    x = "Monthly spend band (SGD)", y = "Count") +
               theme_minimal(base_size = 10) +
               theme(
                 plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
                 axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
                 axis.text.y = element_text(size = 8),
                 strip.text = element_text(size = 10, face = "bold", hjust = 0.5),
                 strip.placement = "outside",
                 plot.margin = margin(12, 14, 20, 14)
               )
           },
           "overall_market_pie" = {
             df <- filtered_data()
             validate(need(nrow(df) > 0, "No data available"))
             pie_df <- df %>% count(Issuer, name = "n") %>%
               mutate(pct = n / sum(n)) %>% arrange(desc(pct))
             n_cols <- nrow(pie_df)
             cols <- get_consistent_colors(n_cols)
             ggplot(pie_df, aes(x = "", y = pct, fill = Issuer)) +
               geom_col(width = 1, color = "white") +
               coord_polar(theta = "y") +
               scale_fill_manual(values = cols, labels = paste0(pie_df$Issuer, " — ", scales::percent(pie_df$pct)), name = "Issuer") +
               labs(title = "Overall Market Share by Issuer", x = NULL, y = NULL) +
               theme_minimal(base_size = 12) +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                     axis.text = element_blank(), axis.title = element_blank(),
                     panel.grid = element_blank(), plot.margin = margin(10, 16, 10, 16),
                     legend.position = "right")
           },
           "overall_satis_hist" = {
             df <- filtered_data()
             validate(need(nrow(df) > 0, "No data available"))
             ggplot(df, aes(x = round(satis))) +
               geom_histogram(binwidth = 1, boundary = 0.5, closed = "right",
                            fill = "#4E79A7", color = "white") +
               scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
               labs(title = "Overall Customer Satisfaction (All Issuers)",
                    x = "Satisfaction score (1–10)", y = "Number of respondents") +
               theme_minimal(base_size = 13) +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                     axis.text = element_text(size = 11),
                     axis.title = element_text(size = 12),
                     plot.margin = margin(10, 16, 10, 16))
           },
           "market_share_plot" = {
             df <- filtered_data()
             sel <- input$issuer_cardmix
             if (!is.null(sel)) df <- df %>% filter(Issuer == sel)
             validate(need(nrow(df) > 0, "No data for selected issuer"))
             pie_df <- df %>% count(Q34_Creditcard_code, name = "n") %>%
               mutate(pct = n / sum(n)) %>% arrange(desc(pct))
             pie_df$card <- pie_df$Q34_Creditcard_code
             pie_df$label <- paste0(pie_df$card, " — ", scales::percent(pie_df$pct))
             pie_df$card <- factor(pie_df$card, levels = pie_df$card)
             n_cols <- nrow(pie_df)
             cols <- get_consistent_colors(n_cols)
             ggplot(pie_df, aes(x = "", y = pct, fill = card)) +
               geom_col(width = 1, color = "white") +
               coord_polar(theta = "y") +
               scale_fill_manual(values = cols, labels = pie_df$label, name = paste0("Cards of ", sel)) +
               labs(title = paste0("Card Mix – ", sel), x = NULL, y = NULL) +
               theme_minimal(base_size = 12) +
               theme(plot.title = element_text(face = "bold", hjust = 0.5),
                     axis.text = element_blank(), axis.title = element_blank(),
                     panel.grid = element_blank(), plot.margin = margin(5,5,5,5),
                     legend.position = "right",
                     legend.title = element_text(face = "bold"),
                     legend.text = element_text(size = 10))
           },
           "issuer_satis_plot" = {
             df <- filtered_data()
             sel <- input$issuer_satis
             if (!is.null(sel)) df <- df %>% filter(Issuer == sel)
             validate(need(nrow(df) > 0, "No data for selected issuer"))
             ggplot(df, aes(x = satis)) +
               geom_histogram(aes(y = after_stat(density)), binwidth = 1, boundary = 0.5, closed = "right",
                            fill = "#59A14F", color = "white", alpha = 0.85) +
               geom_density(color = "#2C6B2F", linewidth = 1) +
               scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
               labs(title = paste0("Satisfaction Distribution – ", sel),
                    x = "Satisfaction score (1–10)", y = "Density") +
               theme_minimal(base_size = 13) +
               theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                     axis.text = element_text(size = 11),
                     axis.title = element_text(size = 12),
                     plot.margin = margin(10, 16, 10, 16))
           },
           "heatmap_plot" = {
             oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar))
             par(mar = c(3.5, 3.5, 3, 1))
             cor_vars <- filtered_data() %>% select(satis, vn_7002_T01:vn_7002_T10)
             cor_matrix <- cor(cor_vars, use = "complete.obs")
             corrplot(cor_matrix, method = "shade", tl.cex = 0.8)
           },
           "scatter_plot" = {
             ggplot(filtered_data(), aes(x = vn_7002_T08, y = satis, color = Issuer)) +
               geom_point() +
               geom_smooth(method = "lm") +
               custom_theme
           },

                       "satisfaction_table" = {
              df <- filtered_data()
              selected_column <- input$table_column_selector
              
              if (is.null(selected_column) || !(selected_column %in% names(df))) return(NULL)
              
              # Get the human-readable name for the selected column
              column_names <- c(
                "satis" = "Customer Satisfaction",
                "confirm" = "Confirmation to Expectations",
                "ideal" = "Close to Ideal Product/Service",
                "overallx" = "Expectations about Overall Quality",
                "customx" = "Expectations about Customization",
                "wrongx" = "Expectations about Reliability",
                "poverq" = "Overall Product Quality",
                "soverq" = "Overall Service Quality",
                "pcustq" = "Product Customization",
                "scustq" = "Service Customization",
                "pwrongq" = "Product Reliability",
                "swrongq" = "Service Reliability",
                "pq" = "Price Given Quality",
                "qp" = "Quality Given Price",
                "Q21" = "Willing to say positive things"
              )
              
              column_name <- column_names[selected_column]
              
              # Compute issuer-level statistics for the selected variable
              table_data <- df %>%
                group_by(Issuer) %>%
                summarise(
                  Average = round(mean(!!sym(selected_column), na.rm = TRUE), 2),
                  Median = round(median(!!sym(selected_column), na.rm = TRUE), 2),
                  Mode = as.numeric(names(sort(table(!!sym(selected_column)), decreasing = TRUE)[1])),
                  Observations = n(),
                  `% ≤ 5` = round(100 * sum(!!sym(selected_column) <= 5, na.rm = TRUE) / n(), 1),
                  .groups = "drop") %>%
                arrange(desc(Average))
              
              if (nrow(table_data) == 0) {
                return(ggplot() + 
                         annotate("text", x = 0.5, y = 0.5, label = "No data available for selected variable", size = 5) +
                         theme_void())
              }
              
              # Create a table visualization using ggplot
              colnames(table_data) <- c("Bank", "Average", "Median", "Mode", "Observations", "% ≤ 5")
              
              # Convert to long format for plotting
              table_long <- table_data %>%
                pivot_longer(cols = c(Average, Median, Mode), 
                           names_to = "Statistic", values_to = "Value")
              
              ggplot(table_long, aes(x = Bank, y = Value, fill = Statistic)) +
                geom_col(position = position_dodge(width = 0.8), width = 0.7) +
                scale_fill_brewer(palette = "Set2") +
                labs(title = paste0(column_name, " Statistics by Bank"), 
                     x = "Bank", y = "Score", fill = "Statistic") +
                theme_minimal(base_size = 12) +
                theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
                      axis.text.y = element_text(size = 10),
                      axis.title = element_text(size = 12, face = "bold"),
                      plot.margin = margin(10, 16, 10, 16),
                      legend.position = "bottom")
            },
           "category_issuer_pie" = {
             df <- filtered_data()
             category_col <- input$category_selector
             
             if (is.null(category_col) || !(category_col %in% names(df))) return(NULL)
             
             category_data <- df %>% 
               filter(!!sym(category_col) == 1) %>%
               count(Issuer, name = "n") %>%
               mutate(pct = n / sum(n)) %>%
               arrange(desc(pct))
             
             if (nrow(category_data) == 0) {
               return(ggplot() + 
                        annotate("text", x = 0.5, y = 0.5, label = "No data available for selected category", size = 5) +
                        theme_void())
             }
             
             n_cols <- nrow(category_data)
             cols <- get_consistent_colors(n_cols)
             
             category_names <- c(
               "L1_1" = "Dining", "L1_2" = "Travel", "L1_3" = "Groceries", "L1_4" = "Shopping",
               "L1_5" = "Entertainment", "L1_6" = "Transport", "L1_7" = "Utilities", "L1_8" = "Insurance",
               "L1_9" = "Education", "L1_10" = "Healthcare", "L1_11" = "Investment", "L1_12" = "Others"
             )
             category_name <- category_names[category_col]
             
             ggplot(category_data, aes(x = "", y = pct, fill = Issuer)) +
               geom_col(width = 1, color = "white") +
               coord_polar(theta = "y") +
               scale_fill_manual(values = cols, 
                                labels = paste0(category_data$Issuer, " — ", scales::percent(category_data$pct)), 
                                name = "Issuer") +
               labs(title = paste0("Credit Card Company Split for ", category_name, " Category"), 
                    x = NULL, y = NULL) +
               theme_minimal(base_size = 12) +
               theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                     axis.text = element_blank(), 
                     axis.title = element_blank(),
                     panel.grid = element_blank(), 
                     plot.margin = margin(10, 16, 10, 16),
                     legend.position = "right",
                     legend.title = element_text(face = "bold"))
           },
           "category_card_pie" = {
             df <- filtered_data()
             category_col <- input$category_selector
             
             if (is.null(category_col) || !(category_col %in% names(df))) return(NULL)
             
             category_data <- df %>% 
               filter(!!sym(category_col) == 1) %>%
               count(Issuer, name = "n") %>%
               mutate(pct = n / sum(n)) %>%
               arrange(desc(pct))
             
             if (nrow(category_data) == 0) {
               return(ggplot() + 
                        annotate("text", x = 0.5, y = 0.5, label = "No data available for selected category", size = 5) +
                        theme_void())
             }
             
             max_share_bank <- category_data$Issuer[1]
             
             card_data <- df %>% 
               filter(Issuer == max_share_bank) %>%
               count(Q34_Creditcard_code, name = "n") %>%
               mutate(pct = n / sum(n)) %>%
               arrange(desc(pct))
             
             if (nrow(card_data) == 0) {
               return(ggplot() + 
                        annotate("text", x = 0.5, y = 0.5, label = "No card data available", size = 5) +
                        theme_void())
             }
             
             n_cols <- nrow(card_data)
             cols <- get_consistent_colors(n_cols)
             
             category_names <- c(
               "L1_1" = "Dining", "L1_2" = "Travel", "L1_3" = "Groceries", "L1_4" = "Shopping",
               "L1_5" = "Entertainment", "L1_6" = "Transport", "L1_7" = "Utilities", "L1_8" = "Insurance",
               "L1_9" = "Education", "L1_10" = "Healthcare", "L1_11" = "Investment", "L1_12" = "Others"
             )
             category_name <- category_names[category_col]
             
             ggplot(card_data, aes(x = "", y = pct, fill = Q34_Creditcard_code)) +
               geom_col(width = 1, color = "white") +
               coord_polar(theta = "y") +
               scale_fill_manual(values = cols, 
                                labels = paste0(card_data$Q34_Creditcard_code, " — ", scales::percent(card_data$pct)), 
                                name = "Credit Card") +
               labs(title = paste0("Credit Card Split for ", max_share_bank, "\n(Dominant in ", category_name, " Category)"), 
                    x = NULL, y = NULL) +
               theme_minimal(base_size = 12) +
               theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                     axis.text = element_blank(), 
                     axis.title = element_blank(),
                     panel.grid = element_blank(), 
                     plot.margin = margin(10, 16, 10, 16),
                     legend.position = "right",
                     legend.title = element_text(face = "bold"))
           },
           "filtered_card_distribution" = {
             df <- filtered_data()
             selected_category <- input$filter_category
             selected_bank <- input$filter_bank
             
             if (is.null(selected_category) || is.null(selected_bank) || 
                 !(selected_category %in% names(df))) return(NULL)
             
             filtered_data_subset <- df %>% 
               filter(!!sym(selected_category) == 1, Issuer == selected_bank)
             
             if (nrow(filtered_data_subset) == 0) {
               return(ggplot() + 
                        annotate("text", x = 0.5, y = 0.5, 
                                 label = paste0("No data available for ", selected_bank, " in selected category"), 
                                 size = 5) +
                        theme_void())
             }
             
             card_data <- filtered_data_subset %>%
               count(Q34_Creditcard_code, name = "n") %>%
               mutate(pct = n / sum(n)) %>%
               arrange(desc(pct))
             
             if (nrow(card_data) == 0) {
               return(ggplot() + 
                        annotate("text", x = 0.5, y = 0.5, label = "No card data available", size = 5) +
                        theme_void())
             }
             
             n_cols <- nrow(card_data)
             cols <- get_consistent_colors(n_cols)
             
             category_names <- c(
               "L1_1" = "Dining", "L1_2" = "Travel", "L1_3" = "Groceries", "L1_4" = "Shopping",
               "L1_5" = "Entertainment", "L1_6" = "Transport", "L1_7" = "Utilities", "L1_8" = "Insurance",
               "L1_9" = "Education", "L1_10" = "Healthcare", "L1_11" = "Investment", "L1_12" = "Others"
             )
             category_name <- category_names[selected_category]
             
             ggplot(card_data, aes(x = "", y = pct, fill = Q34_Creditcard_code)) +
               geom_col(width = 1, color = "white") +
               coord_polar(theta = "y") +
               scale_fill_manual(values = cols, 
                                labels = paste0(card_data$Q34_Creditcard_code, " — ", scales::percent(card_data$pct)), 
                                name = "Credit Card") +
               labs(title = paste0("Credit Card Distribution for ", selected_bank, "\n(", category_name, " Category)"), 
                    x = NULL, y = NULL) +
               theme_minimal(base_size = 12) +
               theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                     axis.text = element_blank(), 
                     axis.title = element_blank(),
                     panel.grid = element_blank(), 
                     plot.margin = margin(10, 16, 10, 16),
                     legend.position = "right",
                     legend.title = element_text(face = "bold"))
           },



           "ridge_plot" = {
             df <- filtered_data()
             validate(need(nrow(df) > 0, "No data available"))
             if (!("age_group" %in% names(df)) && "age" %in% names(df)) {
               df <- df %>% mutate(
                 age_group = cut(age,
                               breaks = c(-Inf, 24, 34, 44, 54, 64, Inf),
                               labels = c("<25", "25-34", "35-44", "45-54", "55-64", "65+"),
                               right = TRUE, ordered_result = TRUE)
               )
             }
             validate(
               need(all(c("satis","age_group","Issuer") %in% names(df)), "Required columns missing: satis, age_group (or age), Issuer")
             )
             df <- df %>% filter(!is.na(satis), !is.na(age_group), !is.na(Issuer))
             
             # Get unique issuers for color palette
             unique_issuers <- unique(df$Issuer)
             n_cols <- length(unique_issuers)
             cols <- get_consistent_colors(n_cols)
             
             ggplot(df, aes(x = satis, y = age_group, fill = Issuer)) +
               geom_density_ridges() +
               scale_fill_manual(values = cols) +
               labs(title = "Satisfaction Distribution by Age Group", x = "Satisfaction", y = "Age Group") +
               custom_theme +
               theme(plot.margin = margin(12, 16, 12, 16))
           },
           "competing_cards_plot" = {
             df <- filtered_data()
             # Get unique issuers for color palette
             unique_issuers <- unique(df$Issuer)
             n_cols <- length(unique_issuers)
             cols <- get_consistent_colors(n_cols)
             
             ggplot(df, aes(x = vn_7002_T20, fill = Issuer)) +
               geom_histogram(position = "dodge") +
               scale_fill_manual(values = cols) +
               labs(title = "Competing Cards Histogram", x = "Score", y = "Count") +
               custom_theme +
               theme(plot.margin = margin(12, 16, 12, 16))
           }
    )
  })
  
  # Download handler for plots
  output$download_plot <- downloadHandler(
    filename = function() {
      plot_name <- input$plot_selector
      if (is.null(plot_name)) return("plot.png")
      paste0(plot_name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      plot_name <- input$plot_selector
      if (is.null(plot_name)) return(NULL)
      
      # Create the plot based on selection
      p <- switch(plot_name,
                  "core_metrics_plot" = {
                    df <- filtered_data()
                    if (!all(c("Issuer","satis","repur","recomm") %in% names(df))) return(NULL)
                    data_long <- df %>%
                      select(Issuer, satis, repur, recomm) %>%
                      tidyr::pivot_longer(cols = c(satis, repur, recomm), names_to = "metric", values_to = "score") %>%
                      mutate(metric = case_when(
                        metric == "satis" ~ "Satisfaction",
                        metric == "repur" ~ "Repurchase",
                        metric == "recomm" ~ "Recommendation",
                        TRUE ~ metric
                      ))
                    ggplot(data_long, aes(Issuer, score, fill = metric)) +
                      geom_violin(color = NA, trim = TRUE, alpha = 0.85) +
                      geom_boxplot(width = 0.12, outlier.alpha = 0.25, aes(fill = metric)) +
                      coord_flip() +
                      facet_wrap(~ metric, nrow = 1) +
                      scale_fill_manual(values = get_consistent_colors(3), name = "Metric") +
                      labs(title = "Core Metrics by Issuer", x = NULL, y = "Score (1–10)") +
                      custom_theme +
                      theme(legend.position = "bottom")
                  },
                  "spend_dist_plot" = {
                    df <- filtered_data()
                    validate(need(nrow(df) > 0, "No data available"))
                    # Get unique spend bins for color palette
                    unique_spend_bins <- unique(df$spend_bin)
                    n_cols <- length(unique_spend_bins)
                    cols <- get_consistent_colors(n_cols)
                    
                    ggplot(df, aes(x = spend_bin, fill = spend_bin)) +
                      geom_bar(color = "white") +
                      scale_fill_manual(values = cols, guide = "none") +
                      facet_wrap(~Issuer, nrow = 3, scales = "free_y") +
                      labs(title = "Monthly Spend Distribution by Issuer",
                           x = "Monthly spend band (SGD)", y = "Count") +
                      theme_minimal(base_size = 10) +
                      theme(
                        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
                        axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
                        axis.text.y = element_text(size = 8),
                        strip.text = element_text(size = 10, face = "bold", hjust = 0.5),
                        strip.placement = "outside",
                        plot.margin = margin(12, 14, 20, 14)
                      )
                  },
                  "channel_usage_plot" = {
                    df <- filtered_data()
                    validate(need(nrow(df) > 0, "No data available"))
                    
                    # Check if required columns exist
                    required_cols <- c("Issuer", "vn_7002_T25_1", "vn_7002_T25_2", "vn_7002_T25_3", "vn_7002_T25_99")
                    missing_cols <- setdiff(required_cols, names(df))
                    if (length(missing_cols) > 0) {
                      validate(need(FALSE, paste("Missing required columns:", paste(missing_cols, collapse = ", "))))
                    }
                    
                    # Prepare data for channel usage
                    channel_data <- df %>%
                      select(Issuer, vn_7002_T25_1, vn_7002_T25_2, vn_7002_T25_3, vn_7002_T25_99) %>%
                      pivot_longer(cols = -Issuer, names_to = "channel", values_to = "used") %>%
                      filter(!is.na(used)) %>%
                      group_by(Issuer, channel) %>%
                      summarise(
                        total_users = n(),
                        users_used = sum(used == 1, na.rm = TRUE),
                        usage_rate = users_used / total_users,
                        .groups = "drop"
                      ) %>%
                      mutate(
                        channel_label = case_when(
                          channel == "vn_7002_T25_1" ~ "Contact Centre",
                          channel == "vn_7002_T25_2" ~ "Mobile App",
                          channel == "vn_7002_T25_3" ~ "Internet Banking",
                          channel == "vn_7002_T25_99" ~ "None of the above",
                          TRUE ~ channel
                        )
                      )
                    
                    # Get unique issuers for color palette
                    unique_issuers <- unique(channel_data$Issuer)
                    n_cols <- length(unique_issuers)
                    cols <- get_consistent_colors(n_cols)
                    
                    # Create the plot
                    ggplot(channel_data, aes(x = reorder(channel_label, usage_rate), y = usage_rate, fill = Issuer)) +
                      geom_col(position = "dodge", alpha = 0.8) +
                      scale_fill_manual(values = cols) +
                      scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
                      labs(
                        title = "Banking Channel Usage by Issuer",
                        subtitle = "Proportion of customers using each banking channel",
                        x = "Banking Channel",
                        y = "Usage Rate",
                        fill = "Issuer"
                      ) +
                      theme_minimal(base_size = 12) +
                      theme(
                        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                        plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray60"),
                        axis.title = element_text(size = 11),
                        axis.text.x = element_text(angle = 45, hjust = 1),
                        legend.position = "right",
                        panel.grid.minor = element_blank(),
                        plot.margin = margin(10, 15, 10, 15)
                      ) +
                      coord_flip()
                  },
                  "competing_cards_histogram" = {
                    df <- filtered_data()
                    validate(need(nrow(df) > 0, "No data available"))
                    
                    # Check if required column exists
                    if (!("vn_7002_T20" %in% names(df))) {
                      validate(need(FALSE, "Required column vn_7002_T20 is missing"))
                    }
                    
                    # Prepare data for competing cards histogram
                    competing_data <- df %>%
                      select(Issuer, vn_7002_T20) %>%
                      filter(!is.na(vn_7002_T20)) %>%
                      mutate(
                        competing_cards = as.numeric(vn_7002_T20),
                        competing_cards_grouped = case_when(
                          competing_cards == 0 ~ "0 cards",
                          competing_cards == 1 ~ "1 card",
                          competing_cards == 2 ~ "2 cards",
                          competing_cards == 3 ~ "3 cards",
                          competing_cards >= 4 ~ "4+ cards",
                          TRUE ~ "Unknown"
                        )
                      ) %>%
                      filter(competing_cards_grouped != "Unknown")
                    
                    # Get unique issuers for color palette
                    unique_issuers <- unique(competing_data$Issuer)
                    n_cols <- length(unique_issuers)
                    cols <- get_consistent_colors(n_cols)
                    
                    # Create the histogram
                    ggplot(competing_data, aes(x = competing_cards, fill = Issuer)) +
                      geom_histogram(binwidth = 1, position = "dodge", alpha = 0.8, color = "white") +
                      scale_fill_manual(values = cols) +
                      scale_x_continuous(breaks = 0:max(competing_data$competing_cards, na.rm = TRUE)) +
                      labs(
                        title = "Number of Competing Cards by Issuer",
                        subtitle = "Distribution of additional credit cards owned (excluding supplementary cards)",
                        x = "Number of Competing Cards",
                        y = "Number of Customers",
                        fill = "Issuer"
                      ) +
                      theme_minimal(base_size = 12) +
                      theme(
                        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                        plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray60"),
                        axis.title = element_text(size = 11),
                        legend.position = "right",
                        panel.grid.minor = element_blank(),
                        plot.margin = margin(10, 15, 10, 15)
                      )
                  },
                  "frequent_cards_pie" = {
                    df <- filtered_data()
                    validate(need(nrow(df) > 0, "No data available"))
                    
                    selected_issuer <- input$frequent_cards_issuer
                    card_type <- input$frequent_cards_type
                    
                    # Check if required columns exist
                    required_cols <- c("Issuer", card_type)
                    missing_cols <- setdiff(required_cols, names(df))
                    if (length(missing_cols) > 0) {
                      validate(need(FALSE, paste("Missing required columns:", paste(missing_cols, collapse = ", "))))
                    }
                    
                    # Prepare data for frequent cards pie chart
                    frequent_data <- df %>%
                      filter(Issuer == selected_issuer) %>%
                      select(all_of(card_type)) %>%
                      filter(!is.na(!!sym(card_type))) %>%
                      count(!!sym(card_type), name = "count") %>%
                      mutate(
                        percentage = (count / sum(count)) * 100,
                        card_label = case_when(
                          card_type == "vn_7002_T21_creditcard" ~ as.character(!!sym(card_type)),
                          card_type == "vn_7002_T21_creditcard_code" ~ as.character(!!sym(card_type)),
                          TRUE ~ as.character(!!sym(card_type))
                        )
                      ) %>%
                      arrange(desc(count))
                    
                    if (nrow(frequent_data) == 0) {
                      return(ggplot() + 
                               annotate("text", x = 0.5, y = 0.5, label = "No data available for selected issuer", size = 5) +
                               theme_void())
                    }
                    
                    # Create color palette
                    n_cols <- nrow(frequent_data)
                    cols <- get_consistent_colors(n_cols)
                    
                    # Create the pie chart
                    ggplot(frequent_data, aes(x = "", y = count, fill = card_label)) +
                      geom_col(width = 1, color = "white") +
                      coord_polar(theta = "y") +
                      scale_fill_manual(values = cols, name = "Credit Card") +
                      labs(
                        title = paste0("Additional Cards Distribution - ", selected_issuer),
                        subtitle = paste0("What other cards do ", selected_issuer, " customers own?"),
                        x = NULL, 
                        y = NULL
                      ) +
                      theme_minimal(base_size = 12) +
                      theme(
                        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                        plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray60"),
                        axis.text = element_blank(),
                        axis.title = element_blank(),
                        panel.grid = element_blank(),
                        plot.margin = margin(10, 15, 10, 15),
                        legend.position = "right",
                        legend.title = element_text(face = "bold"),
                        legend.text = element_text(size = 10)
                      )
                  },
                  "overall_market_pie" = {
                    df <- filtered_data()
                    validate(need(nrow(df) > 0, "No data available"))
                    pie_df <- df %>% count(Issuer, name = "n") %>%
                      mutate(pct = n / sum(n)) %>% arrange(desc(pct))
                    n_cols <- nrow(pie_df)
                    cols <- get_consistent_colors(n_cols)
                    ggplot(pie_df, aes(x = "", y = pct, fill = Issuer)) +
                      geom_col(width = 1, color = "white") +
                      coord_polar(theta = "y") +
                      scale_fill_manual(values = cols, labels = paste0(pie_df$Issuer, " — ", scales::percent(pie_df$pct)), name = "Issuer") +
                      labs(title = "Overall Market Share by Issuer", x = NULL, y = NULL) +
                      theme_minimal(base_size = 12) +
                      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                            axis.text = element_blank(), axis.title = element_blank(),
                            panel.grid = element_blank(), plot.margin = margin(10, 16, 10, 16),
                            legend.position = "right")
                  },
                  "overall_satis_hist" = {
                    df <- filtered_data()
                    validate(need(nrow(df) > 0, "No data available"))
                    ggplot(df, aes(x = round(satis))) +
                      geom_histogram(binwidth = 1, boundary = 0.5, closed = "right",
                                   fill = "#4E79A7", color = "white") +
                      scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
                      labs(title = "Overall Customer Satisfaction (All Issuers)",
                           x = "Satisfaction score (1–10)", y = "Number of respondents") +
                      theme_minimal(base_size = 13) +
                      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                            axis.text = element_text(size = 11),
                            axis.title = element_text(size = 12),
                            plot.margin = margin(10, 16, 10, 16))
                  },
                  "market_share_plot" = {
                    df <- filtered_data()
                    sel <- input$issuer_cardmix
                    if (!is.null(sel)) df <- df %>% filter(Issuer == sel)
                    validate(need(nrow(df) > 0, "No data for selected issuer"))
                    pie_df <- df %>% count(Q34_Creditcard_code, name = "n") %>%
                      mutate(pct = n / sum(n)) %>% arrange(desc(pct))
                    pie_df$card <- pie_df$Q34_Creditcard_code
                    pie_df$label <- paste0(pie_df$card, " — ", scales::percent(pie_df$pct))
                    pie_df$card <- factor(pie_df$card, levels = pie_df$card)
                    n_cols <- nrow(pie_df)
                    cols <- get_consistent_colors(n_cols)
                    ggplot(pie_df, aes(x = "", y = pct, fill = card)) +
                      geom_col(width = 1, color = "white") +
                      coord_polar(theta = "y") +
                      scale_fill_manual(values = cols, labels = pie_df$label, name = paste0("Cards of ", sel)) +
                      labs(title = paste0("Card Mix – ", sel), x = NULL, y = NULL) +
                      theme_minimal(base_size = 12) +
                      theme(plot.title = element_text(face = "bold", hjust = 0.5),
                            axis.text = element_blank(), axis.title = element_blank(),
                            panel.grid = element_blank(), plot.margin = margin(5,5,5,5),
                            legend.position = "right",
                            legend.title = element_text(face = "bold"),
                            legend.text = element_text(size = 10))
                  },
                                     "issuer_satis_plot" = {
                     df <- filtered_data()
                     sel <- input$issuer_satis
                     if (!is.null(sel)) df <- df %>% filter(Issuer == sel)
                     validate(need(nrow(df) > 0, "No data for selected issuer"))
                     ggplot(df, aes(x = satis)) +
                       geom_histogram(aes(y = after_stat(density)), binwidth = 1, boundary = 0.5, closed = "right",
                                    fill = "#59A14F", color = "white", alpha = 0.85) +
                       geom_density(color = "#2C6B2F", linewidth = 1) +
                       scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
                       labs(title = paste0("Satisfaction Distribution – ", sel),
                            x = "Satisfaction score (1–10)", y = "Density") +
                       theme_minimal(base_size = 13) +
                       theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                             axis.text = element_text(size = 11),
                             axis.title = element_text(size = 12),
                             plot.margin = margin(10, 16, 10, 16))
                   },
                  "heatmap_plot" = {
                    oldpar <- par(no.readonly = TRUE); on.exit(par(oldpar))
                    par(mar = c(3.5, 3.5, 3, 1))
                    cor_vars <- filtered_data() %>% select(satis, vn_7002_T01:vn_7002_T10)
                    cor_matrix <- cor(cor_vars, use = "complete.obs")
                    corrplot(cor_matrix, method = "shade", tl.cex = 0.8)
                  },
                  "service_channel_dot_plot" = {
                    df <- filtered_data()
                    required <- c("Issuer","vn_7002_T16","vn_7002_T18","vn_7002_T19")
                    validate(
                      need(nrow(df) > 0, "No data available"),
                      need(all(required %in% names(df)), paste0("Required columns missing: ", paste(setdiff(required, names(df)), collapse=", ")))
                    )
                    
                    # Prepare data for service channel ratings
                    channel_data <- df %>%
                      select(Issuer, vn_7002_T16, vn_7002_T18, vn_7002_T19) %>%
                      pivot_longer(cols = -Issuer, names_to = "channel", values_to = "rating") %>%
                      filter(!is.na(rating)) %>%
                      group_by(Issuer, channel) %>%
                      summarise(
                        mean_rating = mean(rating, na.rm = TRUE),
                        n_ratings = n(),
                        .groups = "drop"
                      ) %>%
                      mutate(
                        channel_label = case_when(
                          channel == "vn_7002_T16" ~ "Contact Centre",
                          channel == "vn_7002_T18" ~ "Mobile App",
                          channel == "vn_7002_T19" ~ "Internet Banking",
                          TRUE ~ channel
                        )
                      )
                    
                    # Get unique issuers for color palette
                    unique_issuers <- unique(channel_data$Issuer)
                    n_cols <- length(unique_issuers)
                    cols <- get_consistent_colors(n_cols)
                    
                    # Create the dot plot
                    ggplot(channel_data, aes(x = reorder(channel_label, mean_rating), y = mean_rating, color = Issuer)) +
                      geom_point(size = 5, alpha = 0.8) +
                      scale_color_manual(values = cols) +
                      scale_y_continuous(limits = c(1, 10), breaks = 1:10) +
                      labs(
                        title = "Service Channel Ratings by Issuer",
                        subtitle = "Mean customer experience ratings across different service channels",
                        x = "Service Channel",
                        y = "Mean Rating (1-10)",
                        color = "Issuer"
                      ) +
                      theme_minimal(base_size = 12) +
                      theme(
                        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                        plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray60"),
                        axis.title = element_text(size = 11),
                        axis.text.x = element_text(angle = 45, hjust = 1),
                        legend.position = "right",
                        panel.grid.minor = element_blank(),
                        plot.margin = margin(10, 15, 10, 15)
                      ) +
                      coord_flip()
                  },
                  "scatter_plot" = {
                    df <- filtered_data()
                    validate(need(nrow(df) > 0, "No data available"))
                    
                    ggplot(df, aes(x = vn_7002_T08, y = satis)) +
                      geom_point(alpha = 0.6, size = 2, color = "#2E86AB") +
                      geom_smooth(method = "lm", se = TRUE, color = "#E74C3C", linewidth = 1.5) +
                      labs(title = "Merchant Tie-ups vs Customer Satisfaction",
                           x = "Merchant Tie-ups Rating (1-10)",
                           y = "Customer Satisfaction (1-10)",
                           subtitle = "Higher merchant tie-ups ratings correlate with higher satisfaction") +
                      scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
                      scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
                      theme_minimal(base_size = 12) +
                      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                            plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray60"),
                            axis.title = element_text(size = 11),
                            panel.grid.minor = element_blank(),
                            plot.margin = margin(10, 15, 10, 15))
                  },
                  "radar_plot" = {
                    df <- filtered_data()
                    selected_bank <- input$radar_bank_selector
                    
                    validate(
                      need(nrow(df) > 0, "No data available"),
                      need(!is.null(selected_bank), "Please select a bank")
                    )
                    
                    # Filter data for selected bank only
                    df_selected <- df %>% filter(Issuer == selected_bank)
                    
                    if (nrow(df_selected) == 0) {
                      return(ggplot() + 
                               annotate("text", x = 0.5, y = 0.5, 
                                       label = paste0("No data available for ", selected_bank), size = 5) +
                               theme_void())
                    }
                    
                    vars <- paste0("vn_7002_T", sprintf("%02d", 1:10))
                    radar_means <- df_selected %>%
                      summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
                    
                    max_min <- data.frame(matrix(c(rep(10, length(vars)), rep(0, length(vars))),
                                               nrow = 2, byrow = TRUE))
                    colnames(max_min) <- vars
                    radar_values <- radar_means %>% select(all_of(vars))
                    radar_df <- rbind(max_min, radar_values)
                    
                    bank_color <- "#2E86AB"  # Professional blue color
                    
                    radarchart(radar_df,
                               axistype = 1,
                               pcol = bank_color,
                               plty = 0,  # No lines
                               plwd = 0.1,  # Very thin line width
                               cglty = 1,
                               cglcol = "gray",
                               cglwd = 0.8,
                               vlcex = 0.8,
                               caxislabels = seq(0, 10, 2),
                               calcex = 0.7,
                               title = paste0("Brand Perception Profile - ", selected_bank),
                               vlabels = c("T01", "T02", "T03", "T04", "T05", "T06", "T07", "T08", "T09", "T10"))
                    
                    # Add legend with parameter meanings
                    legend("topright", 
                           legend = c("T01: Feel comfortable and safe when using",
                                     "T02: Card benefits are presented clearly",
                                     "T03: Brand image complements personality",
                                     "T04: Has a good reputation",
                                     "T05: Flexibility of policies",
                                     "T06: Ease of accessing card balance",
                                     "T07: Card benefits (cashbacks, rewards)",
                                     "T08: Merchant tie-ups meeting needs",
                                     "T09: Ease of reward redemption",
                                     "T10: Redemption catalogue meeting needs"),
                           cex = 0.8,
                           bty = "n",
                           x.intersp = 0.5,
                           y.intersp = 0.8)
                  },
                  "satisfaction_table" = {
                    df <- filtered_data()
                    selected_column <- input$table_column_selector
                    
                    if (is.null(selected_column) || !(selected_column %in% names(df))) return(NULL)
                    
                    # Get the human-readable name for the selected column
                    column_names <- c(
                      "satis" = "Customer Satisfaction",
                      "confirm" = "Confirmation to Expectations",
                      "ideal" = "Close to Ideal Product/Service",
                      "overallx" = "Expectations about Overall Quality",
                      "customx" = "Expectations about Customization",
                      "wrongx" = "Expectations about Reliability",
                      "poverq" = "Overall Product Quality",
                      "soverq" = "Overall Service Quality",
                      "pcustq" = "Product Customization",
                      "scustq" = "Service Customization",
                      "pwrongq" = "Product Reliability",
                      "swrongq" = "Service Reliability",
                      "pq" = "Price Given Quality",
                      "qp" = "Quality Given Price",
                      "Q21" = "Willing to say positive things"
                    )
                    
                    column_name <- column_names[selected_column]
                    
                    # Compute issuer-level statistics for the selected variable
                    table_data <- df %>%
                      group_by(Issuer) %>%
                      summarise(
                        Average = round(mean(!!sym(selected_column), na.rm = TRUE), 2),
                        Median = round(median(!!sym(selected_column), na.rm = TRUE), 2),
                        Mode = as.numeric(names(sort(table(!!sym(selected_column)), decreasing = TRUE)[1])),
                        Observations = n(),
                        `% ≤ 5` = round(100 * sum(!!sym(selected_column) <= 5, na.rm = TRUE) / n(), 1),
                        .groups = "drop") %>%
                      arrange(desc(Average))
                    
                    if (nrow(table_data) == 0) {
                      return(ggplot() + 
                               annotate("text", x = 0.5, y = 0.5, label = "No data available for selected variable", size = 5) +
                               theme_void())
                    }
                    
                    # Create a table visualization using ggplot
                    colnames(table_data) <- c("Bank", "Average", "Median", "Mode", "Observations", "% ≤ 5")
                    
                    # Convert to long format for plotting
                    table_long <- table_data %>%
                      pivot_longer(cols = c(Average, Median, Mode), 
                                 names_to = "Statistic", values_to = "Value")
                    
                    ggplot(table_long, aes(x = Bank, y = Value, fill = Statistic)) +
                      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
                      scale_fill_manual(values = get_consistent_colors(3)) +
                      labs(title = paste0(column_name, " Statistics by Bank"), 
                           x = "Bank", y = "Score", fill = "Statistic") +
                      theme_minimal(base_size = 12) +
                      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                            axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
                            axis.text.y = element_text(size = 10),
                            axis.title = element_text(size = 12, face = "bold"),
                            plot.margin = margin(10, 16, 10, 16),
                            legend.position = "bottom")
                  },
                  "category_issuer_pie" = {
                    df <- filtered_data()
                    category_col <- input$category_selector
                    
                    if (is.null(category_col) || !(category_col %in% names(df))) return(NULL)
                    
                    category_data <- df %>% 
                      filter(!!sym(category_col) == 1) %>%
                      count(Issuer, name = "n") %>%
                      mutate(pct = n / sum(n)) %>%
                      arrange(desc(pct))
                    
                    if (nrow(category_data) == 0) {
                      return(ggplot() + 
                               annotate("text", x = 0.5, y = 0.5, label = "No data available for selected category", size = 5) +
                               theme_void())
                    }
                    
                    n_cols <- nrow(category_data)
                    cols <- get_consistent_colors(n_cols)
                    
                    category_names <- c(
                      "L1_1" = "Dining", "L1_2" = "Travel", "L1_3" = "Groceries", "L1_4" = "Shopping",
                      "L1_5" = "Entertainment", "L1_6" = "Transport", "L1_7" = "Utilities", "L1_8" = "Insurance",
                      "L1_9" = "Education", "L1_10" = "Healthcare", "L1_11" = "Investment", "L1_12" = "Others"
                    )
                    category_name <- category_names[category_col]
                    
                    ggplot(category_data, aes(x = "", y = pct, fill = Issuer)) +
                      geom_col(width = 1, color = "white") +
                      coord_polar(theta = "y") +
                      scale_fill_manual(values = cols, 
                                       labels = paste0(category_data$Issuer, " — ", scales::percent(category_data$pct)), 
                                       name = "Issuer") +
                      labs(title = paste0("Credit Card Company Split for ", category_name, " Category"), 
                           x = NULL, y = NULL) +
                      theme_minimal(base_size = 12) +
                      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                            axis.text = element_blank(), 
                            axis.title = element_blank(),
                            panel.grid = element_blank(), 
                            plot.margin = margin(10, 16, 10, 16),
                            legend.position = "right",
                            legend.title = element_text(face = "bold"))
                  },
                  "category_card_pie" = {
                    df <- filtered_data()
                    category_col <- input$category_selector
                    
                    if (is.null(category_col) || !(category_col %in% names(df))) return(NULL)
                    
                    category_data <- df %>% 
                      filter(!!sym(category_col) == 1) %>%
                      count(Issuer, name = "n") %>%
                      mutate(pct = n / sum(n)) %>%
                      arrange(desc(pct))
                    
                    if (nrow(category_data) == 0) {
                      return(ggplot() + 
                               annotate("text", x = 0.5, y = 0.5, label = "No data available for selected category", size = 5) +
                               theme_void())
                    }
                    
                    max_share_bank <- category_data$Issuer[1]
                    
                    card_data <- df %>% 
                      filter(Issuer == max_share_bank) %>%
                      count(Q34_Creditcard_code, name = "n") %>%
                      mutate(pct = n / sum(n)) %>%
                      arrange(desc(pct))
                    
                    if (nrow(card_data) == 0) {
                      return(ggplot() + 
                               annotate("text", x = 0.5, y = 0.5, label = "No card data available", size = 5) +
                               theme_void())
                    }
                    
                    n_cols <- nrow(card_data)
                    cols <- get_consistent_colors(n_cols)
                    
                    category_names <- c(
                      "L1_1" = "Dining", "L1_2" = "Travel", "L1_3" = "Groceries", "L1_4" = "Shopping",
                      "L1_5" = "Entertainment", "L1_6" = "Transport", "L1_7" = "Utilities", "L1_8" = "Insurance",
                      "L1_9" = "Education", "L1_10" = "Healthcare", "L1_11" = "Investment", "L1_12" = "Others"
                    )
                    category_name <- category_names[category_col]
                    
                    ggplot(card_data, aes(x = "", y = pct, fill = Q34_Creditcard_code)) +
                      geom_col(width = 1, color = "white") +
                      coord_polar(theta = "y") +
                      scale_fill_manual(values = cols, 
                                       labels = paste0(card_data$Q34_Creditcard_code, " — ", scales::percent(card_data$pct)), 
                                       name = "Credit Card") +
                      labs(title = paste0("Credit Card Split for ", max_share_bank, "\n(Dominant in ", category_name, " Category)"), 
                           x = NULL, y = NULL) +
                      theme_minimal(base_size = 12) +
                      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                            axis.text = element_blank(), 
                            axis.title = element_blank(),
                            panel.grid = element_blank(), 
                            plot.margin = margin(10, 16, 10, 16),
                            legend.position = "right",
                            legend.title = element_text(face = "bold"))
                  },
                  "privilege_categories_plot" = {
                    df <- filtered_data()
                    validate(need(nrow(df) > 0, "No data available"))
                    
                    # Check if required column exists
                    if (!("vn_7002_T11" %in% names(df))) {
                      validate(need(FALSE, "Required column vn_7002_T11 is missing"))
                    }
                    
                    # Prepare data for privilege categories
                    privilege_data <- df %>%
                      select(Issuer, vn_7002_T11) %>%
                      filter(!is.na(vn_7002_T11)) %>%
                      count(Issuer, vn_7002_T11, name = "count") %>%
                      group_by(Issuer) %>%
                      mutate(
                        total = sum(count),
                        percentage = (count / total) * 100
                      ) %>%
                      ungroup() %>%
                      mutate(
                        category_label = case_when(
                          vn_7002_T11 == 1 ~ "Dining",
                          vn_7002_T11 == 2 ~ "Food Delivery",
                          vn_7002_T11 == 3 ~ "Retail Fashion",
                          vn_7002_T11 == 4 ~ "Online Marketplace",
                          vn_7002_T11 == 5 ~ "Travel",
                          vn_7002_T11 == 6 ~ "Local Transport",
                          vn_7002_T11 == 7 ~ "Groceries",
                          vn_7002_T11 == 8 ~ "Petrol",
                          vn_7002_T11 == 9 ~ "Entertainment",
                          vn_7002_T11 == 10 ~ "Healthcare",
                          vn_7002_T11 == 11 ~ "Data Communication & Online TV Streaming",
                          TRUE ~ paste("Category", vn_7002_T11)
                        )
                      )
                    
                    # Get unique issuers for color palette
                    unique_issuers <- unique(privilege_data$Issuer)
                    n_cols <- length(unique_issuers)
                    cols <- get_consistent_colors(n_cols)
                    
                    # Create the bar chart
                    ggplot(privilege_data, aes(x = reorder(category_label, percentage), y = percentage, fill = Issuer)) +
                      geom_col(position = "dodge", alpha = 0.8) +
                      scale_fill_manual(values = cols) +
                      scale_y_continuous(labels = percent_format(scale = 1)) +
                      labs(
                        title = "Most Satisfying Privilege Categories by Issuer",
                        subtitle = "Distribution of privilege categories customers are most satisfied with",
                        x = "Privilege Category",
                        y = "Percentage of Customers",
                        fill = "Issuer"
                      ) +
                      theme_minimal(base_size = 12) +
                      theme(
                        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                        plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray60"),
                        axis.title = element_text(size = 11),
                        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
                        legend.position = "right",
                        panel.grid.minor = element_blank(),
                        plot.margin = margin(10, 15, 10, 15)
                      ) +
                      coord_flip()
                  },
                  "filtered_card_distribution" = {
                    df <- filtered_data()
                    selected_category <- input$filter_category
                    selected_bank <- input$filter_bank
                    
                    if (is.null(selected_category) || is.null(selected_bank) || 
                        !(selected_category %in% names(df))) return(NULL)
                    
                    filtered_data_subset <- df %>% 
                      filter(!!sym(selected_category) == 1, Issuer == selected_bank)
                    
                    if (nrow(filtered_data_subset) == 0) {
                      return(ggplot() + 
                               annotate("text", x = 0.5, y = 0.5, 
                                        label = paste0("No data available for ", selected_bank, " in selected category"), 
                                        size = 5) +
                               theme_void())
                    }
                    
                    card_data <- filtered_data_subset %>%
                      count(Q34_Creditcard_code, name = "n") %>%
                      mutate(pct = n / sum(n)) %>%
                      arrange(desc(pct))
                    
                    if (nrow(card_data) == 0) {
                      return(ggplot() + 
                               annotate("text", x = 0.5, y = 0.5, label = "No card data available", size = 5) +
                               theme_void())
                    }
                    
                    n_cols <- nrow(card_data)
                    cols <- get_consistent_colors(n_cols)
                    
                    category_names <- c(
                      "L1_1" = "Dining", "L1_2" = "Travel", "L1_3" = "Groceries", "L1_4" = "Shopping",
                      "L1_5" = "Entertainment", "L1_6" = "Transport", "L1_7" = "Utilities", "L1_8" = "Insurance",
                      "L1_9" = "Education", "L1_10" = "Healthcare", "L1_11" = "Investment", "L1_12" = "Others"
                    )
                    category_name <- category_names[selected_category]
                    
                    ggplot(card_data, aes(x = "", y = pct, fill = Q34_Creditcard_code)) +
                      geom_col(width = 1, color = "white") +
                      coord_polar(theta = "y") +
                      scale_fill_manual(values = cols, 
                                       labels = paste0(card_data$Q34_Creditcard_code, " — ", scales::percent(card_data$pct)), 
                                       name = "Credit Card") +
                      labs(title = paste0("Credit Card Distribution for ", selected_bank, "\n(", category_name, " Category)"), 
                           x = NULL, y = NULL) +
                      theme_minimal(base_size = 12) +
                      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                            axis.text = element_blank(), 
                            axis.title = element_blank(),
                            panel.grid = element_blank(), 
                            plot.margin = margin(10, 16, 10, 16),
                            legend.position = "right",
                            legend.title = element_text(face = "bold"))
                  },



                  "ridge_plot" = {
                    df <- filtered_data()
                    validate(need(nrow(df) > 0, "No data available"))
                    if (!("age_group" %in% names(df)) && "age" %in% names(df)) {
                      df <- df %>% mutate(
                        age_group = cut(age,
                                      breaks = c(-Inf, 24, 34, 44, 54, 64, Inf),
                                      labels = c("<25", "25-34", "35-44", "45-54", "55-64", "65+"),
                              right = TRUE, ordered_result = TRUE)
                      )
                    }
                    validate(
                      need(all(c("satis","age_group","Issuer") %in% names(df)), "Required columns missing: satis, age_group (or age), Issuer")
                    )
                    df <- df %>% filter(!is.na(satis), !is.na(age_group), !is.na(Issuer))
                    
                    # Get unique issuers for color palette
                    unique_issuers <- unique(df$Issuer)
                    n_cols <- length(unique_issuers)
                    cols <- get_consistent_colors(n_cols)
                    
                    ggplot(df, aes(x = satis, y = age_group, fill = Issuer)) +
                      geom_density_ridges() +
                      scale_fill_manual(values = cols) +
                      labs(title = "Satisfaction Distribution by Age Group", x = "Satisfaction", y = "Age Group") +
                      custom_theme +
                      theme(plot.margin = margin(12, 16, 12, 16))
                  },
                  "competing_cards_plot" = {
                    df <- filtered_data()
                    # Get unique issuers for color palette
                    unique_issuers <- unique(df$Issuer)
                    n_cols <- length(unique_issuers)
                    cols <- get_consistent_colors(n_cols)
                    
                    ggplot(df, aes(x = vn_7002_T20, fill = Issuer)) +
                      geom_histogram(position = "dodge") +
                      scale_fill_manual(values = cols) +
                      labs(title = "Competing Cards Histogram", x = "Score", y = "Count") +
                      custom_theme +
                      theme(plot.margin = margin(12, 16, 12, 16))
                  }
      )
      
      # Save the plot
      if (!is.null(p)) {
        # Check if it's a base R plot (radar chart) or ggplot
        if (plot_name == "radar_plot") {
          # For base R plots, use png() and dev.off()
          png(file, width = 10, height = 8, units = "in", res = 300, bg = "white")
          # Recreate the radar plot
          df <- filtered_data()
          selected_bank <- input$radar_bank_selector
          
          if (nrow(df) > 0 && !is.null(selected_bank)) {
            df_selected <- df %>% filter(Issuer == selected_bank)
            
            if (nrow(df_selected) > 0) {
              vars <- paste0("vn_7002_T", sprintf("%02d", 1:10))
              radar_means <- df_selected %>%
                summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
              
              max_min <- data.frame(matrix(c(rep(10, length(vars)), rep(0, length(vars))),
                                         nrow = 2, byrow = TRUE))
              colnames(max_min) <- vars
              radar_values <- radar_means %>% select(all_of(vars))
              radar_df <- rbind(max_min, radar_values)
              
              bank_color <- "#2E86AB"
              
              radarchart(radar_df,
                         axistype = 1,
                         pcol = bank_color,
                         plty = 0,
                         plwd = 0.1,
                         cglty = 1,
                         cglcol = "gray",
                         cglwd = 0.8,
                         vlcex = 0.8,
                         caxislabels = seq(0, 10, 2),
                         calcex = 0.7,
                         title = paste0("Brand Perception Profile - ", selected_bank),
                         vlabels = c("T01", "T02", "T03", "T04", "T05", "T06", "T07", "T08", "T09", "T10"))
              
              legend("topright", 
                     legend = c("T01: Feel comfortable and safe when using",
                               "T02: Card benefits are presented clearly",
                               "T03: Brand image complements personality",
                               "T04: Has a good reputation",
                               "T05: Flexibility of policies",
                               "T06: Ease of accessing card balance",
                               "T07: Card benefits (cashbacks, rewards)",
                               "T08: Merchant tie-ups meeting needs",
                               "T09: Ease of reward redemption",
                               "T10: Redemption catalogue meeting needs"),
                     cex = 0.8,
                     bty = "n",
                     x.intersp = 0.5,
                     y.intersp = 0.8)
            }
          }
          dev.off()
        } else {
          # For ggplot objects, use ggsave()
          ggsave(file, plot = p, width = 10, height = 8, dpi = 300, bg = "white")
        }
      }
    }
  )
}

# Run app
shinyApp(ui = ui, server = server)