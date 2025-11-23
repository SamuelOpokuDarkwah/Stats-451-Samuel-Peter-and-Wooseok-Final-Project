# ============================================================
# Final Project - Member 1 Shiny Draft (Wooseok Choi)
# ============================================================

# I made it into app.R instead of ui.R and server.R

# Third stage(due 11/16)

# Dataset: student_lifestyle_dataset.csv
# Goal: Explore how time allocation (study, sleep, activities, social, physical) relates to GPA and stress level.

# Trade Off plot: GAM based predicted GPA vs stress
# PCA: lifestyle patterns vs stress
# GPA Stress Heatmap: average GPA over sleep/study bins
# GPA Distribution: GPA distribution by stress level
# Stress Profile: radar chart for selected student
# Stress Predictor: simple linear model predicting stress from lifestyle

#still working on how make more interactive cues



# library
library(shiny)
library(tidyverse)
library(DT)
library(viridis)
library(mgcv)
library(factoextra)
library(patchwork)
library(fmsb)

# data
df1 <- read_csv("student_lifestyle_dataset.csv", show_col_types = FALSE)

# trade-off models
m_gpa <- gam(GPA ~ s(Study_Hours_Per_Day, Sleep_Hours_Per_Day), data = df1)
m_stress <- gam(as.numeric(factor(Stress_Level, ordered = TRUE)) ~ 
                  s(Study_Hours_Per_Day, Sleep_Hours_Per_Day), data = df1)

grid1 <- expand.grid(
  Study_Hours_Per_Day = seq(0, 12, 0.25),
  Sleep_Hours_Per_Day = seq(4, 10, 0.25)
)
grid1$pred_gpa    <- predict(m_gpa,    newdata = grid1)
grid1$pred_stress <- predict(m_stress, newdata = grid1)

# PCA
x_m1 <- df1 %>%
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


# UI
ui <- navbarPage(
  "Final Project",
  
  # MEMBER 1(me) UI
  tabPanel(
    "Member 1 Analysis",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Filters (Member 1)"),
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
  ),
  
  # MEMBER 2 UI 
  tabPanel(
    "Member 2 Analysis",
    h3("Member 2: placeholder")
  ),
  
  # MEMBER 3 UI 
  tabPanel(
    "Member 3 Analysis",
    h3("Member 3: placeholder")
  )
)


# SERVER
server <- function(input, output, session) {
  
  # Member 1: filtered data for table
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
  
  # Data Overview table (Member 1)
  output$m1_table <- renderDT({
    datatable(m1_filtered(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Member 1: filtered grid for trade-off plot
  grid_filtered <- reactive({
    grid1 %>%
      dplyr::filter(
        dplyr::between(Study_Hours_Per_Day, input$m1_study_range[1], input$m1_study_range[2]),
        dplyr::between(Sleep_Hours_Per_Day,  input$m1_sleep_range[1], input$m1_sleep_range[2])
      )
  })
  
  # Trade-off Plot (GPA vs Stress)
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
  
  # Member 1: filtered PCA scores
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
  
  # PCA Summary 
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
  
  # GPA Distribution Plot
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
