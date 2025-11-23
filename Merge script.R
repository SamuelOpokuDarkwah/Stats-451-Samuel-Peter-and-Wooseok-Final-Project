
# ---- Libraries ----
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(viridis)

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

#Choi's Dataset


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

  
  # ------------------- Wooseok's ui -------------------
  
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
  
  # ------------------- Wooseok's SERVER -------------------
  
}

# ---- RUN APP ----
shinyApp(ui, server)
