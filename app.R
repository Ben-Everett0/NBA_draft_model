library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(randomForest)
library(DT)
library(plotly)

# ==========================================
# CORE DATA & MODEL INITIALIZATION
# ==========================================
source("modelv3.3.R", local = TRUE)

# Calculate player role means
role_cohort_means <- mod_data %>%
  filter(season < 2026) %>%
  group_by(player_role) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = 'drop')

# ==========================================
# USER INTERFACE (UI)
# ==========================================
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "NBA Draft Analytics Console"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview & Methodology", tabName = "overview", icon = icon("info-circle")),
      menuItem("Big Board", tabName = "big_board", icon = icon("list-ol")),
      menuItem("Prospect Profile", tabName = "prospect_profile", icon = icon("chart-bar"))
    ),
    hr(),
    h4(" Adjust Optimization Weights to Calculate 'Draft Grade'", style = "padding-left: 15px; color: #b8c7ce;"),
    sliderInput("w_impact", "NBA Impact Model (xRAPM):", min = 0, max = 100, value = 40, step = 5),
    sliderInput("w_contrib", "NBA Contributions Model (WS/48):", min = 0, max = 100, value = 20, step = 5),
    sliderInput("w_expert", "Expert Mock Consensus:", min = 0, max = 100, value = 40, step = 5),

    uiOutput("weight_validation_ui")
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    title = "Ben's NBA Draft Model",
                    h3("A Multi-Target Machine Learning Framework for Prospect Projection"),
                    hr(),
                    
                    fluidRow(
                      column(6,
                             h4(icon("brain"), " Predictive Modeling Core"),
                             p("The back-end engine splits its analysis into two distinct Random Forest models trained on over a decade of historical NCAA-to-NBA transitions:"),
                             tags$ul(
                               tags$li(strong("NBA Impact Model:"), " Predicts a player's on-court NBA impact using historical Regularized Adjusted Plus-Minus (RAPM) frameworks inspired by Jeremias Engelmann's methodologies."),
                               tags$li(strong("NBA Contributions Model:"), " Predicts a player's NBA box-score contributions by targeting NBA Win Shares per 48 Minutes (WS/48).")
                             )
                      ),
                      column(6,
                             h4(icon("database"), " Data Matrix & Lineage"),
                             p("The data matrix combines granular situational statistics, athletic traits, and recruiting background data from multiple high-fidelity pipelines:"),
                             tags$ul(
                               tags$li(strong("Collegiate Context:"), " Per-100 possession scoring, playmaking, turnover profiles, and defensive event metrics (Stocks)."),
                               tags$li(strong("Shot-Location Tracking:"), " Differentiates pure volume from finishing efficiency by isolating rim volume vs. rim completion rates and mid-range profiles."),
                               tags$li(strong("Scouting Context:"), " Incorporates Consensus Expert Mock Draft position as a post-model stabilizer.")
                             )
                      )
                    ),
                    
                    hr(), # Added visual break separating the core architecture from data lineage
                    
                    fluidRow(
                      column(12,
                             h4(icon("link"), " Data Sources, Lineage & Codebase"),
                             p("This platform is built upon open statistical frameworks, market data pipelines, and public repositories:"),
                             tags$ul(
                               tags$li(strong("Methodology Baseline: "), "Prospect data structures and predictive design adapted from Jeremias Engelmann's framework: ", 
                                       tags$a(href = "https://www.roycewebb.com/p/how-to-build-an-nba-draft-model", "How to Build an NBA Draft Model", target = "_blank"), "."),
                               
                               tags$li(strong("Positional & Role Archetypes: "), "Player roles compiled from ", 
                                       tags$a(href = "https://barttorvik.com/playerstat.php?year=2027", "BartTorvik Analytics", target = "_blank"), "."),
                               
                               tags$li(strong("Consensus Big Board: "), "Expert consensus tracking aggregated via the ", 
                                       tags$a(href = "https://nbadraftnetwork.com/consensus-big-board", "NBA Draft Network Consensus Board", target = "_blank"), "."),
                               
                               tags$li(strong("Professional Target Metrology: "), "Historical NBA Win Shares sourced via ", 
                                       tags$a(href = "https://www.sports-reference.com/stathead/basketball/player-season-finder.cgi", "Stathead Basketball", target = "_blank"), "."),
                               
                               tags$li(strong("Open-Source Codebase: "), "The underlying modeling scripts, data matrices, and UI environment files are available on ", 
                                       tags$a(href = "https://github.com/Ben-Everett0?tab=repositories", "GitHub", target = "_blank"), ".")
                             )
                      )
                    )
                )
              )),      
      # --- Tab 2: Big Board ---
      tabItem(tabName = "big_board",
              fluidRow(
                box(title = "Global Filter Tools", width = 12, status = "primary", solidHeader = TRUE,
                    fluidRow(
                      column(4, selectizeInput("filter_role", "Filter by Positional Role:", 
                                               choices = c("All Roles", as.character(unique(test_2026$player_role))), 
                                               selected = "All Roles", multiple = TRUE))))
              ),
              fluidRow(
                box(title = "Consensus Grade Big Board", width = 12, status = "primary", solidHeader = TRUE,
                    DTOutput("big_board_table"))
              )
      ),
      
      # --- Tab 3: Prospect Drill Down ---
      tabItem(tabName = "prospect_profile",
              fluidRow(
                box(title = "Select Prospect", width = 4, status = "primary", solidHeader = TRUE,
                    selectInput("selected_player", "Choose Player:", choices = sort(test_2026$name))),
                valueBoxOutput("player_grade_box", width = 4),
                valueBoxOutput("player_role_box", width = 4)
              ),
              fluidRow(
                box(title = "Statistical Profile Drivers", width = 12, status = "warning", solidHeader = TRUE,
                    p("This chart displays how a prospect's metrics deviate from their positional group's historical average. Positive bars indicate traits driving their grade up; negative bars show areas dragging down value."),
                    plotlyOutput("driver_chart", height = "450px"),
                    
                    hr(),
                    h4("Prospect Metrics (Per 100 for On-Court Statistics)", style = "margin-top: 20px; font-weight: bold;"),
                    tableOutput("prospect_raw_stats_table"))
              )
      )
    )
  )
)

# ==========================================
# 3. SERVER LOGIC
# ==========================================
server <- function(input, output, session) {
  
  # Reactive verification that weights scale cleanly
  weights <- reactive({
    total <- input$w_impact + input$w_contrib + input$w_expert
    list(
      w_i = input$w_impact / total,
      w_c = input$w_contrib / total,
      w_e = input$w_expert / total,
      is_valid = (total == 100)
    )
  })
  
  output$weight_validation_ui <- renderUI({
    if (!weights()$is_valid) {
      div(style = "padding: 15px; color: #ff4d4d; font-weight: bold;",
          icon("exclamation-triangle"), " Warning: Weights sum to ", 
          (input$w_impact + input$w_contrib + input$w_expert), "%. Scaling to 100% proportionally.")
    }
  })
  
  # Reactive Board Matrix Calculation
  calculated_board <- reactive({
    w <- weights()
    
    # Apply Min-Max normalizations
    norm_impact  <- (pred_impact - min(pred_impact)) / (max(pred_impact) - min(pred_impact))
    norm_contrib <- (pred_contrib - min(pred_contrib)) / (max(pred_contrib) - min(pred_contrib))
    norm_expert_mock <- (log(65) - log(test_2026$expert_mock)) / (log(65) - log(1))
    
    # Calculate Custom Composite Grade
    final_grade <- (norm_impact * w$w_i) + (norm_contrib * w$w_c) + (norm_expert_mock * w$w_e)
    
    test_2026 %>%
      mutate(
        Raw_Impact  = round(pred_impact, 2),
        Raw_Contrib = round(pred_contrib, 3),
        Draft_Grade = round(final_grade * 100, 1)
      ) %>%
      arrange(desc(Draft_Grade))
  })
  
  # Render Table Output with Reactive Filtering
  output$big_board_table <- renderDT({
    data_out <- calculated_board()
    
    if (!"All Roles" %in% input$filter_role && length(input$filter_role) > 0) {
      data_out <- data_out %>% filter(player_role %in% input$filter_role)
    }

    datatable(
      data_out %>% select(name, player_role, age, Draft_Grade, Raw_Impact, Raw_Contrib, expert_mock),
      colnames = c("Name", "Assigned Role", "Age", "Draft Grade (0-100)", "Model Impact", "Model WS/48", "Consensus Mock"),
      options = list(pageLength = 15, order = list(list(3, 'desc'))),
      rownames = FALSE
    )
  })
  
  # --- Profile Tab Reactive Cards ---
  selected_player_data <- reactive({
    calculated_board() %>% filter(name == input$selected_player)
  })
  
  output$player_grade_box <- renderValueBox({
    valueBox(selected_player_data()$Draft_Grade, "Draft Grade", icon = icon("percentage"), color = "purple")
  })
  
  output$player_role_box <- renderValueBox({
    valueBox(str_to_title(gsub("_", " ", selected_player_data()$player_role)), "Player Role", color = "blue")
  })
  
  # --- Statistical Driver Chart ---
  output$driver_chart <- renderPlotly({
    p_data <- selected_player_data()
    p_role <- p_data$player_role
    
    # Isolate cohort baseline mean
    cohort <- role_cohort_means %>% filter(player_role == p_role)
    
    # Target specific actionable metrics to chart matching your model features
    tracked_features <- c(
      "height", "rec_rank", "age", "sos", "ft_perc", 
      "X3fga_per_100", "rim_perc", "rim_per_100", "mid_per_100", 
      "ast_per_100", "stocks_per_100", "tov_per_100"
    )
    
    friendly_names <- c(
      "Height", "Recruiting Rank", "Age", "Strength of Schedule", "FT %", 
      "3PA Volume", "Rim Finishing %", "Rim Attempts", "Mid-Range Attempts", 
      "Playmaking (AST)", "Defensive Events (Stocks)", "Turnovers"
    )
    
    # Compute standard z-deviance variations relative to their exact role peers
    deviances <- numeric(length(tracked_features))
    for (i in seq_along(tracked_features)) {
      feat <- tracked_features[i]
      historical_sd <- sd(mod_data[[feat]][mod_data$season < 2026], na.rm = TRUE)
      
      # Handle directional flags (lower age, lower TOV are POSITIVE indicators)
      multiplier <- case_when(
        feat %in% c("tov_per_100", "age") ~ -1,
        TRUE ~ 1
      )
      
      # Safe fallback standard deviation check to avoid division by zero
      if (!is.na(historical_sd) && historical_sd > 0) {
        deviances[i] <- ((p_data[[feat]] - cohort[[feat]]) / historical_sd) * multiplier
      } else {
        deviances[i] <- 0
      }
    }
    
    plot_df <- data.frame(
      Feature = factor(friendly_names, levels = friendly_names),
      Deviance = deviances,
      ImpactDirection = ifelse(deviances >= 0, "Helps Rating", "Hurts Rating")
    )
    
    g <- ggplot(plot_df, aes(x = Feature, y = Deviance, fill = ImpactDirection)) +
      geom_bar(stat = "identity", width = 0.6) +
      coord_flip() +
      scale_fill_manual(values = c("Helps Rating" = "#2ecc71", "Hurts Rating" = "#e74c3c")) +
      theme_minimal() +
      labs(x = "", y = "Standard Deviations from Positional Mean") +
      theme(legend.position = "none")
    
    ggplotly(g) %>% config(displayModeBar = FALSE)
  })
  
  # --- Prospect Raw Stats Table ---
  output$prospect_raw_stats_table <- renderTable({
    p_data <- selected_player_data()
    
    # Define features matching the exact chart vector order
    tracked_features <- c(
      "height", "rec_rank", "age", "sos", "ft_perc", 
      "X3fga_per_100", "rim_perc", "rim_per_100", "mid_per_100", 
      "ast_per_100", "stocks_per_100", "tov_per_100"
    )
    
    friendly_names <- c(
      "Height", "Recruiting Rank", "Age", "SOS", "FT %", 
      "3PA Volume", "Rim %", "Rim Att", "Mid Att", 
      "AST", "Stocks", "TOV"
    )
    
    # Isolate the metrics and format percentages dynamically
    raw_metrics <- p_data %>%
      select(all_of(tracked_features)) %>%
      gather(key = "Feature", value = "Value") %>%
      mutate(
        Feature = friendly_names,
        Value = case_when(
          grepl("%", Feature) ~ paste0(round(Value * 100, 1), "%"),  # Formats efficiency stats cleanly
          Feature == "Height" ~ paste0(floor(Value / 12), "'", round(Value %% 12), '"'), # Formats height as standard feet/inches
          TRUE ~ as.character(round(Value, 1))                       # Standard round for volume counts
        )
      ) %>%
      spread(key = Feature, value = Value)
    
    # Maintain column sequence synchronization
    raw_metrics <- raw_metrics[, friendly_names]
    
    return(raw_metrics)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c', width = "100%")
}


# Run the app
shinyApp(ui = ui, server = server)