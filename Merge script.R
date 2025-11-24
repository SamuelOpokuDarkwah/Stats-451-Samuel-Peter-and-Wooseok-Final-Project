
# ---- Libraries ----
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(viridis)

library(mgcv)
library(factoextra)
library(patchwork)
library(fmsb)
# ---- Load Datasets ----

#Samuels Dataset
df1 <- read_csv("shopping_behavior_updated.csv")   # Samuel's Data

#Samuels Data Preparation
df1 <- df1 %>%
  mutate(
    `Age Group` = cut(Age,
                      breaks = c(0, 25, 35, 45, 55, 65, 100),
                      labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "65+")),
    across(where(is.character), as.factor)
  )

#Peter's Dataset
# Peter's Data prep
df2 <- read_csv("NCHS_-_Leading_Causes_of_Death__United_States.csv")
# Models
top5 <- df2 %>%
  filter(State == "United States", `Cause Name` != "All causes") %>%
  group_by(`Cause Name`) %>%
  summarise(total_deaths = sum(Deaths, na.rm = TRUE)) %>%
  arrange(desc(total_deaths)) %>%
  slice_max(total_deaths, n = 5) %>%
  pull(`Cause Name`)

trend_data <- df2 %>%
  filter(State == "United States",
         `Cause Name` %in% top5) %>%
  mutate(`Cause Name` = factor(`Cause Name`, levels = top5))


#Wooseok's Dataset

# Wooseok data
df3 <- read_csv("student_lifestyle_dataset.csv", show_col_types = FALSE)

# trade-off models
m_gpa <- gam(GPA ~ s(Study_Hours_Per_Day, Sleep_Hours_Per_Day), data = df3)
m_stress <- gam(as.numeric(factor(Stress_Level, ordered = TRUE)) ~ 
                  s(Study_Hours_Per_Day, Sleep_Hours_Per_Day), data = df3)

grid1 <- expand.grid(
  Study_Hours_Per_Day = seq(0, 12, 0.25),
  Sleep_Hours_Per_Day = seq(4, 10, 0.25)
)
grid1$pred_gpa    <- predict(m_gpa,    newdata = grid1)
grid1$pred_stress <- predict(m_stress, newdata = grid1)

# PCA
x_m1 <- df3 %>%
  select(Study_Hours_Per_Day, Sleep_Hours_Per_Day,
         Extracurricular_Hours_Per_Day, Social_Hours_Per_Day,
         Physical_Activity_Hours_Per_Day)

pca_m1 <- prcomp(x_m1, scale. = TRUE)

rn <- rownames(pca_m1$rotation)
rn <- gsub("_Hours_Per_Day", " (hrs/day)", rn)
rn <- gsub("_", " ", rn)
rownames(pca_m1$rotation) <- rn

df_pca <- as.data.frame(pca_m1$x[, 1:2])
colnames(df_pca) <- c("PC1", "PC2")
df_pca$Stress_Level        <- df1$Stress_Level
df_pca$Study_Hours_Per_Day <- df1$Study_Hours_Per_Day
df_pca$Sleep_Hours_Per_Day <- df1$Sleep_Hours_Per_Day
#Wooseok Data done

# ---- UI ----
ui <- navbarPage(
  "Samuel,Peter and Wooseok's Dashboard",
  
  # ------------------- Samuels ui -------------------
  tabPanel("Samuel's Analysis",
           tabsetPanel(
             
             # ---- 1. Data Overview ----
             tabPanel("Data Overview",
                      fluidRow(
                        DTOutput("table1")
                      ),
                      fluidRow(
                        verbatimTextOutput("summary1"),
                        verbatimTextOutput("missing1")
                      )
             ),
             
             # ---- 2. Categorical Variables ----
             tabPanel("Categorical Variables",
                      selectInput("catVar1", "Select a Categorical Variable:",
                                  choices = names(df1 %>% select(where(is.factor)))),
                      plotlyOutput("catPlot1")
             ),
             
             # ---- 3. Numerical Variables ----
             tabPanel("Numerical Variables",
                      selectInput("numVar1", "Select Numerical Variable:",
                                  choices = names(df1 %>% select(where(is.numeric), -`Customer ID`))),
                      plotlyOutput("numPlot1")
             ),
             
             # ---- 4. Demographics ----
             tabPanel("Demographics",
                      plotlyOutput("genderPie1"),
                      plotlyOutput("purchaseGender1"),
                      plotlyOutput("ageGender1"),
                      plotlyOutput("prevGender1")
             ),
             
             # ---- 5. Geography ----
             tabPanel("Geography",
                      plotlyOutput("topLocations1"),
                      plotlyOutput("avgSpendLocation1"),
                      plotlyOutput("usMap1")
             ),
             
             # ---- 6. Seasonal & Category ----
             tabPanel("Seasonal & Category",
                      plotlyOutput("seasonSales1"),
                      plotlyOutput("categoryCount1"),
                      plotlyOutput("categoryAvg1"),
                      plotlyOutput("categoryRating1"),
                      plotlyOutput("seasonCategory1")
             ),
             
             # ---- 7. Behavior Patterns ----
             tabPanel("Behavior Patterns",
                      plotlyOutput("freqPurchase1"),
                      plotlyOutput("paymentMethods1"),
                      plotlyOutput("shippingTypes1"),
                      plotlyOutput("subscriptionStatus1")
             ),
             
             # ---- 8. Correlation ----
             tabPanel("Correlations",
                      plotlyOutput("corrPlot1")
             ),
             
             # ---- 9. Discounts & Promos ----
             tabPanel("Discounts & Promos",
                      plotlyOutput("promoUsage1"),
                      plotlyOutput("discountUsage1"),
                      plotlyOutput("discountBox1")
             ),
             
             # ---- 10. Age Group Analysis ----
             tabPanel("Age Group Analysis",
                      plotlyOutput("agePurchase1"),
                      plotlyOutput("agePrev1"),
                      plotlyOutput("ageRating1"),
                      plotlyOutput("ageFreq1")
             )
           )
  ),
  
  # ------------------- Peter's ui -------------------
  tabPanel("Peter's Analysis",
           tabsetPanel(
             
             # ---- Dataset Overview ----
             tabPanel("Dataset Overview",
                      h3("Dataset Preview"),
                      DTOutput("table")
             ),
             
             # ---- Yearly Summary ----
             tabPanel("Yearly Summary",
                      h3("Leading Causes of Death in the U.S. by Year"),
                      
                      sliderInput("year_slider",
                                  "Select Year:",
                                  min = min(df2$Year, na.rm = TRUE),
                                  max = max(df2$Year, na.rm = TRUE),
                                  value = max(df2$Year, na.rm = TRUE),
                                  step = 1,
                                  sep = ""),
                      
                      plotlyOutput("barplot")
             ),
             
             # ---- Trends Over Time ----
             tabPanel("Death Trends (Top 5 Causes)",
                      h3("Trends in Leading Causes of Death Over Time"),
                      plotlyOutput("trendplot")
             ),
             
             
             # ---- Multi-Cause Trend Comparison ----
             tabPanel("Compare Multiple Causes",
                      h3("Compare Causes Over Time"),
                      checkboxGroupInput("cause_multi", 
                                         "Select Causes:",
                                         choices = sort(unique(df2$`Cause Name`)),
                                         selected = top5),
                      plotlyOutput("trend_compare_plot")
             ),
             
             # ---- State Comparison ----
             tabPanel("State Comparison",
                      h3("Compare Deaths Across States"),
                      selectInput("cause_state", "Select Cause:", choices = top5),
                      plotlyOutput("state_compare_plot")
             ),
             
             # ---- Rate Trends by Year ----
             tabPanel(
               "Rate Trends by Year",
               
               sidebarLayout(
                 sidebarPanel(
                   selectInput("rate_state", "Select State:",
                               choices = NULL),
                   
                   selectInput("rate_cause", "Select Cause:",
                               choices = NULL),
                   
                   width = 3
                 ),
                 
                 mainPanel(
                   plotOutput("rate_trend_plot", height = "450px")
                 )
               )
             )
             
           )
  )

  
  # ------------------- Wooseok's ui -------------------
  tabPanel(
    "Wooseok's Analysis",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Filters (Wooseok)"),
        sliderInput(
          "m1_study_range", "Study Hours",
          min = 0, max = 12, value = c(0, 12), step = 0.5
        ),
        sliderInput(
          "m1_sleep_range", "Sleep Hours",
          min = 4, max = 10, value = c(4, 10), step = 0.5
        ),
        selectInput(
          "m1_stress_show", "Stress Level",
          choices = c("All", "Low", "Moderate", "High"), selected = "All"
        ),
        selectInput(
          "m1_trade_color", "Color in Trade Off plot",
          choices = c("Study hours" = "Study_Hours_Per_Day",
                      "Sleep hours" = "Sleep_Hours_Per_Day"),
          selected = "Study_Hours_Per_Day"
        )
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = "m1_tabs",
          
          # 1 Data overview
          tabPanel(
            "Data Overview",
            verbatimTextOutput("m1_filter_text"),
            DTOutput("m1_table")
          ),
          
          # 2 Trade-off plot
          tabPanel(
            "Trade Off plot",
            plotOutput("m1_trade", height = "450px")
          ),
          
          # 3 PCA summary
          tabPanel(
            "PCA Summary",
            plotOutput("m1_pca_sum", height = "500px")
          ),
          
          # 4 GPA–Stress heatmap
          tabPanel(
            "GPA - Stress Heatmap",
            plotOutput("m1_heatmap", height = "450px")
          ),
          
          # 5 Stress profile radar
          tabPanel(
            "Stress Profile",
            selectInput(
              "m1_student", "Choose Student ID",
              choices = df1$Student_ID
            ),
            plotOutput("m1_radar", height = "450px")
          ),
          
          # 6 Stress predictor
          tabPanel(
            "Stress Predictor",
            selectInput(
              "model_type", "Model Type",
              c("Linear Regression", "Random Forest")
            ),
            sliderInput("pred_study", "Study Hours",
                        min = 0, max = 12, value = 5),
            sliderInput("pred_sleep", "Sleep Hours",
                        min = 4, max = 10, value = 7),
            verbatimTextOutput("m1_prediction")
          ),
          
          # 7 GPA distribution
          tabPanel(
            "GPA Distribution",
            selectInput(
              "m1_gpa_plot_type", "Plot type",
              choices = c("Density", "Histogram", "Boxplot"),
              selected = "Density"
            ),
            plotOutput("m1_gpa_dist", height = "450px")
          )
        )
      )
    )
  )  
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # ------------------- Samuel's SERVER -------------------
  # 1. Overview
  output$table1 <- renderDT({ datatable(df1, options = list(scrollX = TRUE, pageLength = 25)) })
  output$summary1 <- renderPrint({ summary(df1) })
  output$missing1 <- renderPrint({ colSums(is.na(df1)) })
  
  # 2. Categorical
  output$catPlot1 <- renderPlotly({
    var <- input$catVar1
    plot_data <- df1 %>% count(!!sym(var))
    p <- ggplot(plot_data, aes(x = reorder(!!sym(var), n), y = n, fill = !!sym(var))) +
      geom_bar(stat = "identity") + scale_fill_viridis_d(option = "viridis") +
      coord_flip() + labs(x = var, y = "Count") + theme_minimal() + theme(legend.position = "none")
    ggplotly(p)
  })
  
  # 3. Numerical
  output$numPlot1 <- renderPlotly({
    var <- input$numVar1
    p <- ggplot(df1, aes(x = !!sym(var))) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "#69b3a2", alpha = 0.7) +
      geom_density(color = "red") +
      labs(title = paste("Distribution of", var), x = var, y = "Density") +
      theme_minimal()
    ggplotly(p)
  })
  
  # 4. Demographics
  output$genderPie1 <- renderPlotly({
    gender_counts <- df1 %>% count(Gender)
    colors <- viridis(nrow(gender_counts))
    plot_ly(gender_counts, labels = ~Gender, values = ~n, type = "pie", marker=list(colors=colors)) %>%
      layout(title = "Gender Distribution")
  })
  
  output$purchaseGender1 <- renderPlotly({
    p <- ggplot(df1, aes(x = Gender, y = `Purchase Amount (USD)`, fill = Gender)) +
      geom_boxplot() + scale_fill_viridis_d(option = "viridis") +
      labs(title = "Purchase Amount by Gender", y = "Purchase Amount (USD)") + theme_minimal()
    ggplotly(p)
  })
  
  output$ageGender1 <- renderPlotly({
    p <- ggplot(df1, aes(x = Gender, y = Age, fill = Gender)) +
      geom_boxplot() + scale_fill_viridis_d(option = "viridis") + theme_minimal() +
      labs(title = "Age Distribution by Gender")
    ggplotly(p)
  })
  
  output$prevGender1 <- renderPlotly({
    p <- ggplot(df1, aes(x = Gender, y = `Previous Purchases`, fill = Gender)) +
      geom_boxplot() + scale_fill_viridis_d(option = "viridis") + theme_minimal() +
      labs(title = "Previous Purchases by Gender")
    ggplotly(p)
  })
  
  # 5. Geography
  output$topLocations1 <- renderPlotly({
    top_loc <- df1 %>% count(Location, sort = TRUE) %>% slice(1:10)
    p <- ggplot(top_loc, aes(x = n, y = reorder(Location, n), fill = Location)) +
      geom_col() + scale_fill_viridis_d(option = "viridis") + labs(title = "Top 10 Locations")
    ggplotly(p)
  })
  
  output$avgSpendLocation1 <- renderPlotly({
    loc_avg <- df1 %>% group_by(Location) %>% summarize(avg_spend = mean(`Purchase Amount (USD)`)) %>% slice(1:10)
    p <- ggplot(loc_avg, aes(x = avg_spend, y = reorder(Location, avg_spend), fill = Location)) +
      geom_col() + scale_fill_viridis_d(option = "viridis") +
      labs(title = "Avg Purchase Amount by Location", x = "Average (USD)", y = "Location")
    ggplotly(p)
  })
  
  output$usMap1 <- renderPlotly({
    df_map <- df1 %>% mutate(StateAbbrev = state.abb[match(Location, state.name)]) %>% filter(!is.na(StateAbbrev))
    state_summary <- df_map %>% group_by(StateAbbrev) %>% summarize(avg_spend = mean(`Purchase Amount (USD)`))
    plot_ly(state_summary, type = "choropleth", locations = ~StateAbbrev, locationmode = "USA-states",
            z = ~avg_spend, colorscale = "Viridis", colorbar = list(title = "Avg Spend (USD)")) %>%
      layout(title = "Average Purchase Amount by U.S. State", geo = list(scope="usa"))
  })
  
  # 6. Seasonal & Category
  output$seasonSales1 <- renderPlotly({
    sales <- df1 %>% group_by(Season) %>% summarize(total = sum(`Purchase Amount (USD)`))
    colors <- viridis(nrow(sales))
    plot_ly(sales, labels = ~Season, values = ~total, type = "pie", marker=list(colors=colors)) %>%
      layout(title = "Sales by Season")
  })
  
  output$categoryCount1 <- renderPlotly({
    cat_count <- df1 %>% count(Category)
    p <- ggplot(cat_count, aes(x = n, y = reorder(Category, n), fill = Category)) +
      geom_col() + scale_fill_viridis_d(option = "viridis") + labs(title = "Purchases by Category")
    ggplotly(p)
  })
  
  output$categoryAvg1 <- renderPlotly({
    cat_avg <- df1 %>% group_by(Category) %>% summarize(avg_purchase = mean(`Purchase Amount (USD)`))
    p <- ggplot(cat_avg, aes(x = avg_purchase, y = reorder(Category, avg_purchase), fill = Category)) +
      geom_col() + scale_fill_viridis_d(option = "viridis") + labs(title = "Average Purchase by Category")
    ggplotly(p)
  })
  
  output$categoryRating1 <- renderPlotly({
    p <- ggplot(df1, aes(x = Category, y = `Review Rating`, fill=Category)) +
      geom_boxplot() + scale_fill_viridis_d(option = "viridis") + theme_minimal() +
      labs(title = "Review Ratings by Category") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  output$seasonCategory1 <- renderPlotly({
    data <- df1 %>% group_by(Season, Category) %>% summarize(total = sum(`Purchase Amount (USD)`))
    p <- ggplot(data, aes(x = Season, y = total, fill = Category)) +
      geom_bar(stat = "identity", position = "dodge") + scale_fill_viridis_d(option = "viridis") +
      labs(title = "Category Popularity by Season", y = "Total Sales (USD)")
    ggplotly(p)
  })
  
  # 7. Behavior Patterns
  plot_behavior_pie <- function(var, title) {
    counts <- df1 %>% count(!!as.name(var))
    colors <- viridis(nrow(counts))
    plot_ly(counts, labels = ~get(var), values = ~n, type = "pie", marker=list(colors=colors)) %>%
      layout(title = title)
  }
  
  output$freqPurchase1 <- renderPlotly({ plot_behavior_pie("Frequency of Purchases", "Purchase Frequency") })
  output$paymentMethods1 <- renderPlotly({ plot_behavior_pie("Payment Method", "Payment Methods") })
  output$shippingTypes1 <- renderPlotly({ plot_behavior_pie("Shipping Type", "Shipping Types") })
  output$subscriptionStatus1 <- renderPlotly({ plot_behavior_pie("Subscription Status", "Subscription Status") })
  
  # 8. Correlation
  output$corrPlot1 <- renderPlotly({
    num_df <- df1 %>% select(where(is.numeric), -`Customer ID`)
    corr <- round(cor(num_df, use = "complete.obs"), 2)
    plot_ly(z = corr, x = colnames(corr), y = colnames(corr), type = "heatmap",
            colorscale = "RdBu", reversescale=TRUE, zmin=-1, zmax=1) %>%
      layout(title = "Correlation Heatmap")
  })
  
  # 9. Discounts & Promos
  output$promoUsage1 <- renderPlotly({ plot_behavior_pie("Promo Code Used", "Promo Code Usage") })
  output$discountUsage1 <- renderPlotly({ plot_behavior_pie("Discount Applied", "Discount Application") })
  output$discountBox1 <- renderPlotly({
    p <- ggplot(df1, aes(x = `Discount Applied`, y = `Purchase Amount (USD)`, fill = `Discount Applied`)) +
      geom_boxplot() + scale_fill_viridis_d(option = "viridis") + theme_minimal() +
      labs(title = "Purchase Amount: Discount vs No Discount")
    ggplotly(p)
  })
  
  # 10. Age Group Analysis
  output$agePurchase1 <- renderPlotly({
    p <- ggplot(df1, aes(x = `Age Group`, y = `Purchase Amount (USD)`, fill = `Age Group`)) +
      geom_boxplot() + scale_fill_viridis_d(option = "viridis") + theme_minimal() +
      labs(title = "Average Purchase Amount by Age Group",y="Amount(USD)")
    ggplotly(p)
  })
  
  output$agePrev1 <- renderPlotly({
    p <- ggplot(df1, aes(x = `Age Group`, y = `Previous Purchases`, fill = `Age Group`)) +
      geom_boxplot() + scale_fill_viridis_d(option = "viridis") + theme_minimal() +
      labs(title = "Previous Purchases by Age Group")
    ggplotly(p)
  })
  
  output$ageRating1 <- renderPlotly({
    p <- ggplot(df1, aes(x = `Age Group`, y = `Review Rating`, fill = `Age Group`)) +
      geom_boxplot() + scale_fill_viridis_d(option = "viridis") + theme_minimal() +
      labs(title = "Ratings by Age Group")
    ggplotly(p)
  })
  
  output$ageFreq1 <- renderPlotly({
    freq_data <- df1 %>% count(`Age Group`, `Frequency of Purchases`)
    p <- ggplot(freq_data, aes(x = `Age Group`, y = n, fill = `Frequency of Purchases`)) +
      geom_bar(stat = "identity", position = "dodge") + scale_fill_viridis_d(option = "viridis") +
      labs(title = "Purchase Frequency by Age Group", y = "Count") + theme_minimal()
    ggplotly(p)
  }) 
  # ------------------- Peter's SERVER -------------------
    # ---- Dataset Table ----
  output$table <- renderDT({
    datatable(df2)
  })
  
  # ---- Bar plot for selected year ----
  output$barplot <- renderPlotly({
    
    selected_data <- df2 %>%
      filter(State == "United States",
             Year == input$year_slider,
             `Cause Name` != "All causes")
    
    p <- ggplot(selected_data,
                aes(x = reorder(`Cause Name`, Deaths),
                    y = Deaths,
                    fill = `Cause Name`)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(
        title = paste("Leading Causes of Death -", input$year_slider),
        x = "Cause of Death",
        y = "Deaths"
      ) +
      scale_y_continuous(labels = scales::comma)
    
    ggplotly(p)
  })
  
  # ---- Trend Plot (Top 5 Causes) ----
  output$trendplot <- renderPlotly({
    
    p <- ggplot(trend_data,
                aes(x = Year,
                    y = Deaths,
                    color = `Cause Name`)) +
      geom_line(size = 1.2) +
      labs(
        x = "Year",
        y = "Deaths",
        color = "Cause"
      ) +
      scale_y_continuous(labels = scales::comma,
                         limits = c(0, NA),
                         breaks = function(x) unique(c(0, pretty(x))))
    
    ggplotly(p)
  })
  
  # ---- State Comparison Plot ----
  output$state_compare_plot <- renderPlotly({
    p <- df2 %>%
      filter(`Cause Name` == input$cause_state,
             State != "United States") %>%
      group_by(State) %>%
      summarise(total = sum(Deaths), .groups = "drop") %>%
      arrange(desc(total)) %>%
      slice(1:10) %>%
      ggplot(aes(x = total, y = reorder(State, total))) +
      geom_col(fill = "steelblue") +
      labs(
        title = paste("Top 10 States by Deaths for:", input$cause_state),
        x = "Total Deaths",
        y = ""
      )
    
    ggplotly(p)
  })
  
  # ---- Multi-Cause Trend Comparison ----
  output$trend_compare_plot <- renderPlotly({
    data <- df2 %>%
      filter(State == "United States",
             `Cause Name` %in% input$cause_multi)
    
    p <- ggplot(data, aes(x = Year, y = Deaths, color = `Cause Name`)) +
      geom_line(size = 1.1) +
      labs(title = "Trend Comparison of Selected Causes",
           x = "Year",
           y = "Deaths") +
      scale_y_continuous(labels = scales::comma)
    
    ggplotly(p)
  })
  
  # ---- Populate dropdowns for Rate Trends ----
  observe({
    updateSelectInput(
      session,
      "rate_state",
      choices = sort(unique(df2$State)),
      selected = "United States"
    )
    
    updateSelectInput(
      session,
      "rate_cause",
      choices = sort(unique(df2$`Cause Name`)),
      selected = sort(unique(df2$`Cause Name`))[1]
    )
  })
  
  # ---- Rate Trends Plot ----
  output$rate_trend_plot <- renderPlot({
    req(input$rate_state, input$rate_cause)
    
    data_rate <- df2 %>%
      filter(
        State == input$rate_state,
        `Cause Name` == input$rate_cause
      )
    
    ggplot(data_rate, aes(x = Year, y = `Age-adjusted Death Rate`)) +
      geom_line(linewidth = 1.2, color = "steelblue") +
      geom_point(size = 2) +
      labs(
        title = "Age-Adjusted Death Rate Over Time",
        subtitle = paste(input$rate_cause, "in", input$rate_state),
        x = "Year",
        y = "Rate (per 100,000)"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # ------------------- Wooseok's SERVER -------------------
  m1_filtered <- reactive({
    d <- df1 %>%
      dplyr::filter(
        dplyr::between(Study_Hours_Per_Day, input$m1_study_range[1], input$m1_study_range[2]),
        dplyr::between(Sleep_Hours_Per_Day,  input$m1_sleep_range[1], input$m1_sleep_range[2])
      )
    if (input$m1_stress_show != "All") {
      d <- d %>% dplyr::filter(Stress_Level == input$m1_stress_show)
    }
    # debugging
    cat("Rows in filtered table:", nrow(d), "\n")
    d
  })
  
  output$m1_filter_text <- renderText({
    paste0(
      "Current filters:\n",
      "- Study Hours: ", input$m1_study_range[1], " to ", input$m1_study_range[2], "\n",
      "- Sleep Hours: ", input$m1_sleep_range[1], " to ", input$m1_sleep_range[2], "\n",
      "- Stress Level: ", input$m1_stress_show
    )
  })
  
  #Data Overview table
  output$m1_table <- renderDT({
    datatable(m1_filtered(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  #filtered grid for trade-off plot
  grid_filtered <- reactive({
    grid1 %>%
      dplyr::filter(
        dplyr::between(Study_Hours_Per_Day, input$m1_study_range[1], input$m1_study_range[2]),
        dplyr::between(Sleep_Hours_Per_Day,  input$m1_sleep_range[1], input$m1_sleep_range[2])
      )
  })
  
  #Trade-off Plot (GPA vs Stress)
  output$m1_trade <- renderPlot({
    g <- grid_filtered()
    
    color_var <- switch(
      input$m1_trade_color,
      "Study_Hours_Per_Day" = g$Study_Hours_Per_Day,
      "Sleep_Hours_Per_Day" = g$Sleep_Hours_Per_Day
    )
    
    ggplot(g, aes(x = pred_gpa, y = pred_stress)) +
      geom_point(aes(color = color_var), alpha = 0.7, size = 1.8) +
      scale_color_viridis_c() +
      labs(
        title = "Trade-off between Predicted GPA and Stress",
        x = "Predicted GPA",
        y = "Predicted Stress (1–3)",
        color = ifelse(input$m1_trade_color == "Study_Hours_Per_Day",
                       "Study Hours", "Sleep Hours")
      ) +
      theme_minimal(base_size = 13)
  })
  
  #filtered PCA scores
  df_pca_filtered <- reactive({
    d <- df_pca %>%
      dplyr::filter(
        dplyr::between(Study_Hours_Per_Day, input$m1_study_range[1], input$m1_study_range[2]),
        dplyr::between(Sleep_Hours_Per_Day,  input$m1_sleep_range[1], input$m1_sleep_range[2])
      )
    if (input$m1_stress_show != "All") {
      d <- d %>% dplyr::filter(Stress_Level == input$m1_stress_show)
    }
    d
  })
  
  #PCA Summary 
  output$m1_pca_sum <- renderPlot({
    
    pca_point <- ggplot(df_pca_filtered(),
                        aes(x = PC1, y = PC2, color = Stress_Level, fill = Stress_Level)) +
      geom_point(alpha = 0.5, size = 2) +
      stat_ellipse(aes(fill = Stress_Level),
                   geom = "polygon", level = 0.68,
                   alpha = 0.25, color = NA) +
      stat_ellipse(level = 0.68, size = 1.1) +
      scale_color_manual(
        values = c("Low" = "#2E8B57", "Moderate" = "#1E90FF", "High" = "#CD5C5C")
      ) +
      scale_fill_manual(
        values = c("Low" = "#2E8B57", "Moderate" = "#1E90FF", "High" = "#CD5C5C")
      ) +
      labs(
        title = "Lifestyle Pattern (PCA) vs Stress Level",
        x = "PC1", y = "PC2", color = "Stress Level"
      ) +
      theme_minimal(base_size = 12)
    
    pca_var <- fviz_pca_var(
      pca_m1,
      col.var = "contrib",
      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
      repel = TRUE, labelsize = 3,
      title = "Contributions of Activities to Lifestyle Dimensions"
    ) +
      theme_minimal(base_size = 12)
    
    pca_point + pca_var + plot_layout(ncol = 2, widths = c(1.2, 1))
  })
  
  output$m1_heatmap <- renderPlot({
    df1 %>%
      mutate(sleep_bin = cut(Sleep_Hours_Per_Day, breaks=seq(4,10,1)),
             study_bin = cut(Study_Hours_Per_Day, breaks=seq(0,12,1))) %>%
      group_by(study_bin, sleep_bin) %>%
      summarise(avg_gpa = mean(GPA), avg_stress = mean(as.numeric(factor(Stress_Level)))) %>%
      ggplot(aes(x = study_bin, y = sleep_bin, fill = avg_gpa)) +
      geom_tile() +
      scale_fill_viridis_c() +
      labs(title = "GPA Heatmap by Sleep & Study Hours",
           x = "Study Hours (binned)",
           y = "Sleep Hours (binned)",
           fill = "Avg GPA") +
      theme_minimal()
  })
  
  output$m1_radar <- renderPlot({
    student <- df1 %>% filter(Student_ID == input$m1_student)
    avg <- df1 %>% summarise(across(ends_with("_Hours_Per_Day"), mean))
    
    df_radar <- rbind(
      max = c(12,10,8,8,8),
      min = c(0, 4, 0, 0, 0),
      student %>% select(Study_Hours_Per_Day:Physical_Activity_Hours_Per_Day),
      avg
    )
    
    radarchart(df_radar, axistype = 1,
               pcol = c("red", "blue"), plwd = 3,
               title = paste("Lifestyle Profile for Student", input$m1_student))
  })
  
  model_stress <- lm(as.numeric(factor(Stress_Level)) ~ 
                       Study_Hours_Per_Day + Sleep_Hours_Per_Day + 
                       Extracurricular_Hours_Per_Day + Social_Hours_Per_Day +
                       Physical_Activity_Hours_Per_Day, 
                     data = df1)
  
  output$m1_prediction <- renderPrint({
    
    newdata <- data.frame(
      Study_Hours_Per_Day        = input$pred_study,
      Sleep_Hours_Per_Day        = input$pred_sleep,
      Extracurricular_Hours_Per_Day = mean(df1$Extracurricular_Hours_Per_Day),
      Social_Hours_Per_Day          = mean(df1$Social_Hours_Per_Day),
      Physical_Activity_Hours_Per_Day = mean(df1$Physical_Activity_Hours_Per_Day)
    )
    
    if (input$model_type == "Linear Regression") {
      pred_num <- predict(model_stress, newdata)
      stress_cat <- cut(
        pred_num,
        breaks = c(0, 1.5, 2.5, 3.5),
        labels = c("Low", "Moderate", "High"),
        include.lowest = TRUE
      )
      cat("Model: Linear Regression\n",
          "Predicted numeric stress:", round(pred_num, 2), "\n",
          "Predicted category:", as.character(stress_cat), "\n")
      
    } else {
      cat("Random Forest not implemented yet.\n",
          "Currently using only the linear regression model.")
    }
  })
  
  #GPA Distribution Plot
  output$m1_gpa_dist <- renderPlot({
    df <- m1_filtered()
    
    validate(
      need(nrow(df) > 0, "No data in this filter range. Try widening the sliders.")
    )
    
    p_base <- ggplot(df, aes(x = GPA, fill = Stress_Level, color = Stress_Level))
    
    if (input$m1_gpa_plot_type == "Density") {
      p <- p_base +
        geom_density(alpha = 0.3) +
        labs(title = "GPA Density by Stress Level")
      
    } else if (input$m1_gpa_plot_type == "Histogram") {
      p <- p_base +
        geom_histogram(position = "identity", alpha = 0.4, bins = 30) +
        labs(title = "GPA Histogram by Stress Level")
      
    } else if (input$m1_gpa_plot_type == "Boxplot") {
      p <- ggplot(df, aes(x = Stress_Level, y = GPA, fill = Stress_Level)) +
        geom_boxplot(alpha = 0.6) +
        labs(title = "GPA Boxplot by Stress Level")
    }
    
    p +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_minimal(base_size = 13) +
      labs(x = "GPA", y = "Density / Count / GPA")
  })
   
}

# ---- RUN APP ----
shinyApp(ui, server)
