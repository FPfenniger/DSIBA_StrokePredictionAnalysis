# Exploratory Data Analysis

library(tidyverse)
library(plotly)
library(knitr)
library(caret)
library(vcd)

outlier_stroke_tb <- read.csv("data/datasets/outlier_stroke_tb.csv")
balanced_stroke_tb <- read.csv("data/datasets/balanced_stroke_tb.csv")
preprocessed_stroke_tb <- read.csv("data/datasets/preprocessed_stroke_tb.csv")


## Descriptive Statistics

# Generate summary of selected numerical variables
summary_table <- summary(outlier_stroke_tb[, c("age", "avg_glucose_level", "bmi")])
summary_table <- as.data.frame.matrix(summary_table)
knitr::kable(summary_table, caption = "Summary of Numerical Variables", row.names = FALSE)

# Categorical variables summary
variables_to_summarize <- c("stroke", "gender", "ever_married", "work_type", 
                            "Residence_type", "smoking_status", "hypertension", "heart_disease")

outlier_stroke_tb <- outlier_stroke_tb %>%
  mutate(
    stroke = as.factor(stroke),
    hypertension = as.factor(hypertension),
    heart_disease = as.factor(heart_disease)
  )

total_count <- nrow(outlier_stroke_tb)

summary_table <- bind_rows(
  lapply(variables_to_summarize, function(var) {
    outlier_stroke_tb %>%
      group_by(.data[[var]]) %>%
      summarize(
        Count = n(),
        Proportion = n() / total_count,
        .groups = "drop"
      ) %>%
      rename(Type = .data[[var]]) %>%
      mutate(
        Variable = var,
        Type = as.character(Type),
        Proportion = round(Proportion * 100, 2)
      )
  })
) %>%
  select(Variable, Type, Count, Proportion) %>%  
  arrange(Variable, Type)

knitr::kable(summary_table, caption = "Summary of Categorical Variables with Proportion", row.names = FALSE)


## Univariate Analysis - Numerical variables

library(ggplot2)
library(plotly)

create_histogram <- function(data, variable, stroke_variable, binwidth = 5, show_legend = FALSE) {
  original_data <- data[[variable]]
  stroke_data <- data[[stroke_variable]]
  
  mean_val <- mean(original_data, na.rm = TRUE)
  sd_val <- sd(original_data, na.rm = TRUE)
  
  plot_data <- data.frame(data = original_data, stroke = factor(stroke_data, labels = c("No Stroke", "Stroke")))
  
  p <- ggplot(plot_data, aes(x = data, fill = stroke)) +
    geom_histogram(binwidth = binwidth, color = "white", position = "stack") +
    labs(y = "Frequency", fill = "Stroke Status") +
    scale_fill_manual(values = c("No Stroke" = "#1f77b4", "Stroke" = "#ff7f0e")) +
    theme_minimal() +
    theme(axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5))
  
  p_plotly <- ggplotly(p) %>%
    layout(showlegend = show_legend)
  
  return(p_plotly)
}

hist_age <- create_histogram(outlier_stroke_tb, "age", "stroke", binwidth = 5, show_legend = FALSE) %>%
  layout(title = "Distribution of Age", xaxis = list(title = "Age"), yaxis = list(title = "Frequency"))

hist_glucose <- create_histogram(outlier_stroke_tb, "avg_glucose_level", "stroke", binwidth = 5, show_legend = FALSE) %>%
  layout(title = "Distribution of Average Glucose Level", xaxis = list(title = "Average Glucose Level"), 
         yaxis = list(title = "Frequency"))

hist_bmi <- create_histogram(outlier_stroke_tb, "bmi", "stroke", binwidth = 5, show_legend = FALSE) %>%
  layout(title = "Distribution of BMI", xaxis = list(title = "BMI"), yaxis = list(title = "Frequency"))

combined_histograms <- subplot(hist_age, hist_glucose, hist_bmi, nrows = 1, titleY = TRUE,
                               shareX = TRUE, shareY = TRUE) %>%
  layout(title = "Histograms of Numerical Variables by Stroke", showlegend = TRUE)

combined_histograms

# Violin plots
create_violin_plot <- function(data, variable, stroke_variable) {
  data[[stroke_variable]] <- as.factor(data[[stroke_variable]])
  
  plot <- plot_ly(data, x = ~get(stroke_variable), y = ~get(variable), type = "violin",
                  box = list(visible = TRUE),
                  meanline = list(visible = TRUE),
                  name = variable) %>%
    layout(
      title = paste("Violin Plot of", variable, "by Stroke"),
      xaxis = list(title = "Stroke"),
      yaxis = list(title = variable)
    )
  
  plot
}

plot_age <- create_violin_plot(outlier_stroke_tb, variable = "age", stroke_variable = "stroke")
plot_glucose <- create_violin_plot(outlier_stroke_tb, variable = "avg_glucose_level", stroke_variable = "stroke")
plot_bmi <- create_violin_plot(outlier_stroke_tb, variable = "bmi", stroke_variable = "stroke")

combined_plot <- subplot(plot_age, plot_glucose, plot_bmi, nrows = 1, titleX = TRUE, titleY = TRUE) %>%
  layout(title = "Violin Plots of Numerical Variables by Stroke")

combined_plot

# Correlations with stroke
cor_age <- cor(as.numeric(outlier_stroke_tb$age), as.numeric(outlier_stroke_tb$stroke), method = "pearson")
cor_glucose <- cor(as.numeric(outlier_stroke_tb$avg_glucose_level), as.numeric(outlier_stroke_tb$stroke), method = "pearson")
cor_bmi <- cor(as.numeric(outlier_stroke_tb$bmi), as.numeric(outlier_stroke_tb$stroke), method = "pearson")

correlation_table <- data.frame(
  Variable = c("Age", "Average Glucose Level", "BMI"),
  Correlation_with_Stroke = round(c(cor_age, cor_glucose, cor_bmi), 2)
)

kable(correlation_table, caption = "Correlation Coefficients with Stroke")

## Categorical variables - Stacked bar charts
# Define choices for the variable selection, excluding Gender and Residence Type
x_variable_choices <- c("Ever Married" = "ever_married",
                        "Work Type" = "work_type", 
                        "Smoking Status" = "smoking_status",
                        "Hypertension" = "hypertension", 
                        "Heart Disease" = "heart_disease")

# Create a filtered data frame based on all selected variables
filtered_data <- outlier_stroke_tb
# Remove "Other" for the gender variable
filtered_data <- filtered_data %>% filter(!(gender == "Other"))  # Simply check for 'Other' in gender

# Initialize an empty list to store plots
plots <- list()

# Loop through each categorical variable and create a bar chart
for (selected_variable in x_variable_choices) {
  
  # Get variable name
  variable_name <- names(x_variable_choices)[which(x_variable_choices == selected_variable)]
  
  if (variable_name %in% c("hypertension", "heart_disease")) {
    p <- ggplot(filtered_data, aes_string(x = selected_variable, fill = "as.factor(stroke)")) +
      geom_bar(position = "fill") +
      labs(title = paste("Distribution of Stroke by", variable_name),
           fill = "STROKE", y = "Ratio") +
      scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +  # Custom x-axis for binary variables
      scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e"))  # Set colors for no stroke and stroke
  } else {
    # For other categorical variables
    p <- ggplot(filtered_data, aes_string(x = selected_variable, fill = "as.factor(stroke)")) +
      geom_bar(position = "fill") +
      labs(title = paste("Distribution of Stroke by", variable_name),
           fill = "STROKE", y = "Ratio") +
      scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e"))  # Set colors for no stroke and stroke
  }
  
  # Convert ggplot to an interactive plotly object and store it in the list
  plots[[variable_name]] <- ggplotly(p)
}
# Display all plots
for (plot in plots) {
  print(plot)  # You can use print to display each plot one after the other
}

# Correlation matrix for numeric features
numeric_data <- outlier_stroke_tb[, c("age", "avg_glucose_level", "bmi")]
correlation_matrix <- cor(numeric_data, use = "complete.obs")

plot_ly(
  x = colnames(correlation_matrix),
  y = rownames(correlation_matrix),
  z = correlation_matrix,
  type = "heatmap",
  colorscale = "Viridis"
) %>%
  layout(
    title = "Correlation Matrix Heatmap",
    xaxis = list(title = "", tickangle = 45),
    yaxis = list(title = "")
  )

## Balanced Dataset Analysis

# Original dataset stroke distribution
p <- ggplot(outlier_stroke_tb, aes(x = factor(stroke))) +
  geom_bar() +
  labs(title = "Stroke Distribution in Original Dataset",
       x = "Stroke (0 = No Stroke, 1 = Stroke)",
       y = "Count") +
  theme_minimal()

ggplotly(p)

# Balanced dataset stroke distribution
p <- ggplot(balanced_stroke_tb, aes(x = factor(stroke))) +
  geom_bar() +
  labs(title = "Stroke Distribution in Balanced Dataset",
       x = "Stroke (0 = No Stroke, 1 = Stroke)",
       y = "Count") +
  theme_minimal()

ggplotly(p)

# Summary of numerical variables in balanced dataset
summary_table <- summary(balanced_stroke_tb[, c("age", "avg_glucose_level", "bmi")])
summary_table <- as.data.frame.matrix(summary_table)
knitr::kable(summary_table, caption = "Summary of numerical Variables", row.names = FALSE)

# Histograms for balanced dataset
variable_choices <- c("Age" = "age", "Average Glucose Level" = "avg_glucose_level")

plots <- list()

for (selected_variable in variable_choices) {
  original_data <- balanced_stroke_tb[[selected_variable]]
  stroke_data <- balanced_stroke_tb$stroke
  
  filtered_data <- balanced_stroke_tb %>% filter(!is.na(original_data), !is.infinite(original_data))
  
  mean_val <- mean(filtered_data[[selected_variable]], na.rm = TRUE)
  sd_val <- sd(filtered_data[[selected_variable]], na.rm = TRUE)
  
  data <- filtered_data[[selected_variable]]
  x_label <- selected_variable
  mean_line <- mean_val
  sd_lines <- c(mean_val - sd_val, mean_val + sd_val)
  binwidth <- 5
  
  plot_data <- data.frame(data = data, stroke = factor(filtered_data$stroke, labels = c("No Stroke", "Stroke")))
  
  p <- ggplot(plot_data, aes(x = data, fill = stroke)) +
    geom_histogram(binwidth = binwidth, color = "white", position = "stack") +
    labs(title = paste("Histogram of", ifelse(selected_variable == "age", "Age", "Average Glucose Level"), "by Stroke Status"), 
         x = x_label, y = "Frequency", fill = "Stroke Status") +
    geom_vline(aes(xintercept = mean_line), color = "blue", linetype = "dashed", linewidth = 1) +
    geom_vline(aes(xintercept = sd_lines[1]), color = "red", linetype = "dotted", linewidth = 0.8) +
    geom_vline(aes(xintercept = sd_lines[2]), color = "red", linetype = "dotted", linewidth = 0.8) +
    scale_fill_manual(values = c("No Stroke" = "#1f77b4", "Stroke" = "#ff7f0e"))
  
  plots[[selected_variable]] <- ggplotly(p)
}

for (plot in plots) {
  print(plot)
}

# Violin plots for balanced dataset
plot_age <- create_violin_plot(balanced_stroke_tb, variable = "age", stroke_variable = "stroke")
plot_glucose <- create_violin_plot(balanced_stroke_tb, variable = "avg_glucose_level", stroke_variable = "stroke")
plot_bmi <- create_violin_plot(balanced_stroke_tb, variable = "bmi", stroke_variable = "stroke")

combined_plot <- subplot(plot_age, plot_glucose, plot_bmi, nrows = 1, titleX = TRUE, titleY = TRUE) %>%
  layout(title = "Balanced Violin Plots of Numerical Variables by Stroke")

combined_plot

# Correlations in balanced dataset
cor_age <- cor(as.numeric(balanced_stroke_tb$age), as.numeric(balanced_stroke_tb$stroke), method = "pearson")
cor_glucose <- cor(as.numeric(balanced_stroke_tb$avg_glucose_level), as.numeric(balanced_stroke_tb$stroke), method = "pearson")
cor_bmi <- cor(as.numeric(balanced_stroke_tb$bmi), as.numeric(balanced_stroke_tb$stroke), method = "pearson")

correlation_table <- data.frame(
  Variable = c("Age", "Average Glucose Level", "BMI"),
  Correlation_with_Stroke = round(c(cor_age, cor_glucose, cor_bmi), 2)
)

kable(correlation_table)

# Re-Examination of categorical variables with balanced data
outlier_stroke_tb$stroke <- as.factor(outlier_stroke_tb$stroke)
initial_balanced_stroke_tb <- upSample(
  x = outlier_stroke_tb[, names(outlier_stroke_tb) != "stroke"],
  y = outlier_stroke_tb$stroke,
  yname = "stroke"
)

x_variable_choices <- c("Ever Married" = "ever_married",
                        "Work Type" = "work_type", 
                        "Smoking Status" = "smoking_status",
                        "Hypertension" = "hypertension", 
                        "Heart Disease" = "heart_disease")

plots <- list()
filtered_data <- initial_balanced_stroke_tb

for (selected_variable in names(x_variable_choices)) {
  variable_name <- x_variable_choices[selected_variable]
  
  if (!variable_name %in% names(filtered_data)) {
    next
  }
  
  if (nrow(filtered_data) > 0) {
    if (variable_name %in% c("hypertension", "heart_disease")) {
      p <- ggplot(filtered_data, aes_string(x = variable_name, fill = "as.factor(stroke)")) +
        geom_bar(position = "fill") +
        labs(title = paste("Proportion of Stroke by", selected_variable), fill = "STROKE", y = "Ratio") +
        scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e")) +
        scale_x_discrete(labels = c("0" = "No", "1" = "Yes"))
    } else {
      p <- ggplot(filtered_data, aes_string(x = variable_name, fill = "as.factor(stroke)")) +
        geom_bar(position = "fill") +
        labs(title = paste("Proportion of Stroke by", selected_variable), fill = "STROKE", y = "Ratio") +
        scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e"))
    }
    
    plots[[selected_variable]] <- ggplotly(p)
  }
}

for (plot in plots) {
  print(plot)
}

# Proportions for hypertension and heart_disease
hypertension_proportion <- initial_balanced_stroke_tb %>%
  group_by(hypertension) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(proportion = count / sum(count)) %>%
  mutate(Condition = paste("Hypertension:", hypertension)) %>%
  select(Condition, Count = count, Proportion = proportion)

heart_disease_proportion <- initial_balanced_stroke_tb %>%
  group_by(heart_disease) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(proportion = count / sum(count)) %>%
  mutate(Condition = paste("Heart Disease:", heart_disease)) %>%
  select(Condition, Count = count, Proportion = proportion)

results <- bind_rows(
  hypertension_proportion,
  heart_disease_proportion
)

kable(results, format = "markdown", col.names = c("Condition", "Count", "Proportion"))

# Violin plots for initial_balanced_stroke_tb
plot_age <- create_violin_plot(initial_balanced_stroke_tb, variable = "age", stroke_variable = "stroke")
plot_glucose <- create_violin_plot(initial_balanced_stroke_tb, variable = "avg_glucose_level", stroke_variable = "stroke")
plot_bmi <- create_violin_plot(initial_balanced_stroke_tb, variable = "bmi", stroke_variable = "stroke")

combined_plot <- subplot(plot_age, plot_glucose, plot_bmi, nrows = 1, titleX = TRUE, titleY = TRUE) %>%
  layout(title = "Violin Plots of Numerical Variables by Stroke in Balanced Initial Data")

combined_plot

# Correlations in initial_balanced_stroke_tb
cor_age <- cor(as.numeric(initial_balanced_stroke_tb$age), as.numeric(initial_balanced_stroke_tb$stroke), method = "pearson")
cor_glucose <- cor(as.numeric(initial_balanced_stroke_tb$avg_glucose_level), as.numeric(initial_balanced_stroke_tb$stroke), method = "pearson")
cor_bmi <- cor(as.numeric(initial_balanced_stroke_tb$bmi), as.numeric(initial_balanced_stroke_tb$stroke), method = "pearson")

correlation_table <- data.frame(
  Variable = c("Age", "Average Glucose Level", "BMI"),
  Correlation_with_Stroke = round(c(cor_age, cor_glucose, cor_bmi), 2)
)

kable(correlation_table)


# Stratification: Under 60 and Over 60
under_60_data <- outlier_stroke_tb[outlier_stroke_tb$age < 60, ]
over_60_data <- outlier_stroke_tb[outlier_stroke_tb$age >= 60, ]

kable(head(under_60_data), caption = "Individuals Under 60 Years")
kable(head(over_60_data), caption = "Individuals Aged 60 and Older")

# Under 60 analysis - summaries
summary_under_60_data <- summary(under_60_data[, c("age", "avg_glucose_level", "bmi")])
summary_table_under_60 <- as.data.frame.matrix(summary_under_60_data)
knitr::kable(summary_table_under_60, caption = "Summary of Numerical Variables (Individuals Under 60 Years Old)", row.names = FALSE)

variables_to_summarize <- c("stroke", "gender", "ever_married", 
                            "work_type", "Residence_type", 
                            "smoking_status", "hypertension", 
                            "heart_disease")

under_60_data <- under_60_data %>%
  mutate(
    stroke = as.factor(stroke),
    hypertension = as.factor(hypertension),
    heart_disease = as.factor(heart_disease)
  )

total_count_under_60 <- nrow(under_60_data)

summary_table_under_60 <- bind_rows(
  lapply(variables_to_summarize, function(var) {
    under_60_data %>%
      group_by(!!sym(var)) %>%
      summarize(
        Count = n(),
        Proportion = n() / total_count_under_60,
        .groups = "drop"
      ) %>%
      rename(Type = !!sym(var)) %>%
      mutate(
        Variable = var,
        Type = as.character(Type),
        Proportion = round(Proportion * 100, 2)
      )
  })
) %>%
  select(Variable, Type, Count, Proportion) %>%  
  arrange(Variable, Type)

knitr::kable(summary_table_under_60, caption = "Summary of Categorical Variables with Proportions (Individuals Under 60 Years Old)", row.names = FALSE)

# Histograms under 60
hist_age <- create_histogram(under_60_data, "age", "stroke", binwidth = 5, show_legend = TRUE)
hist_glucose <- create_histogram(under_60_data, "avg_glucose_level", "stroke", binwidth = 5, show_legend = FALSE)
hist_bmi <- create_histogram(under_60_data, "bmi", "stroke", binwidth = 5, show_legend = FALSE)

combined_histograms <- subplot(
  hist_age, hist_glucose, hist_bmi,
  nrows = 1, titleX = TRUE, titleY = TRUE
) %>%
  layout(
    title = "Histograms of Numerical Variables by Stroke Status (Individuals Under 60 Years Old)",
    showlegend = TRUE
  )

#combined_histograms  # Uncomment to display if needed

# Violin plots under 60
plot_age <- create_violin_plot(under_60_data, variable = "age", stroke_variable = "stroke")
plot_glucose <- create_violin_plot(under_60_data, variable = "avg_glucose_level", stroke_variable = "stroke")
plot_bmi <- create_violin_plot(under_60_data, variable = "bmi", stroke_variable = "stroke")

combined_plot <- subplot(plot_age, plot_glucose, plot_bmi,
                         nrows = 1, titleX = TRUE, titleY = TRUE) %>%
  layout(title = "Violin Plots of Numerical Variables by Stroke (Individuals Under 60 Years Old)")

combined_plot

# Correlation under 60
cor_age <- cor(as.numeric(under_60_data$age), as.numeric(under_60_data$stroke), method = "pearson")
cor_glucose <- cor(as.numeric(under_60_data$avg_glucose_level), as.numeric(under_60_data$stroke), method = "pearson")
cor_bmi <- cor(as.numeric(under_60_data$bmi), as.numeric(under_60_data$stroke), method = "pearson")

correlation_table <- data.frame(
  Variable = c("Age", "Average Glucose Level", "BMI"),
  Correlation_with_Stroke = round(c(cor_age, cor_glucose, cor_bmi), 2)
)

kable(correlation_table, caption = "Correlation Coefficients with Stroke (Individuals Under 60 Years Old)")

# Categorical vars under 60
variables_to_summarize <- c("ever_married", "work_type", "smoking_status", "hypertension", "heart_disease")
readable_names <- c("Ever Married", "Work Type", "Smoking Status", "Hypertension", "Heart Disease")

create_bar_chart <- function(data, variable, readable_name) {
  p <- ggplot(data, aes_string(x = variable, fill = "as.factor(stroke)")) +
    geom_bar(position = "fill") +
    labs(fill = "STROKE", y = "Proportion",
         title = paste("Proportion of Stroke by", readable_name, "for Under 60 Years Old")) +
    theme_minimal() +
    scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e"))
  
  ggplotly(p)
}

for (i in seq_along(variables_to_summarize)) {
  var <- variables_to_summarize[i]
  readable_name <- readable_names[i]
  bar_plot <- create_bar_chart(under_60_data, var, readable_name)
  print(bar_plot)
}

# Over 60 analysis
summary_over_60_data <- summary(over_60_data[, c("age", "avg_glucose_level", "bmi")])
summary_table_over_60 <- as.data.frame.matrix(summary_over_60_data)
knitr::kable(summary_table_over_60, caption = "Summary of Numerical Variables (Individuals of 60 and Over Years Old)", row.names = FALSE)

variables_to_summarize <- c("stroke", "gender", "ever_married", "work_type", "Residence_type", 
                            "smoking_status", "hypertension", "heart_disease")

over_60_data <- over_60_data %>%
  mutate(
    stroke = as.factor(stroke),
    hypertension = as.factor(hypertension),
    heart_disease = as.factor(heart_disease)
  )

total_count_over_60 <- nrow(over_60_data)

summary_table_over_60 <- bind_rows(
  lapply(variables_to_summarize, function(var) {
    over_60_data %>%
      group_by(.data[[var]]) %>%
      summarize(
        Count = n(),
        Proportion = n() / total_count_over_60,
        .groups = "drop"
      ) %>%
      rename(Type = .data[[var]]) %>%
      mutate(
        Variable = var,
        Type = as.character(Type),
        Proportion = round(Proportion * 100, 2)
      )
  })
) %>%
  select(Variable, Type, Count, Proportion) %>%  
  arrange(Variable, Type)

knitr::kable(summary_table_over_60, caption = "Summary of Categorical Variables with Proportions (Individuals of 60 and Over Years Old)", row.names = FALSE)

# Histograms over 60
hist_age <- create_histogram(over_60_data, "age", "stroke", binwidth = 5, show_legend = TRUE)
hist_glucose <- create_histogram(over_60_data, "avg_glucose_level", "stroke", binwidth = 5, show_legend = FALSE)
hist_bmi <- create_histogram(over_60_data, "bmi", "stroke", binwidth = 5, show_legend = FALSE)

combined_histograms <- subplot(
  hist_age, hist_glucose, hist_bmi,
  nrows = 1, titleX = TRUE, titleY = TRUE
) %>%
  layout(
    title = "Histograms of Numerical Variables by Stroke Status (Individuals Over 60 Years)",
    showlegend = TRUE
  )

combined_histograms

# Violin plots over 60
plot_age <- create_violin_plot(over_60_data, variable = "age", stroke_variable = "stroke")
plot_glucose <- create_violin_plot(over_60_data, variable = "avg_glucose_level", stroke_variable = "stroke")
plot_bmi <- create_violin_plot(over_60_data, variable = "bmi", stroke_variable = "stroke")

combined_plot <- subplot(
  plot_age %>% layout(title = "Age"),
  plot_glucose %>% layout(title = "Average Glucose Level"),
  plot_bmi %>% layout(title = "BMI"),
  nrows = 1, titleX = TRUE, titleY = TRUE
) %>%
  layout(title = "Violin Plots of Numerical Variables by Stroke (Individuals Aged 60 and Over Years Old)")

combined_plot

# Correlation over 60
cor_age_over_60 <- cor(as.numeric(over_60_data$age), as.numeric(over_60_data$stroke), method = "pearson")
cor_glucose_over_60 <- cor(as.numeric(over_60_data$avg_glucose_level), as.numeric(over_60_data$stroke), method = "pearson")
cor_bmi_over_60 <- cor(as.numeric(over_60_data$bmi), as.numeric(over_60_data$stroke), method = "pearson")

correlation_table_over_60 <- data.frame(
  Variable = c("Age", "Average Glucose Level", "BMI"),
  Correlation_with_Stroke = round(c(cor_age_over_60, cor_glucose_over_60, cor_bmi_over_60), 2)
)

kable(correlation_table_over_60, caption = "Correlation Coefficients with Stroke (Individuals Aged 60 and Over)")

# Categorical over 60
variables_to_summarize <- c("ever_married", "hypertension", "heart_disease")
readable_names <- c("Ever Married", "Hypertension", "Heart Disease")

create_bar_chart <- function(data, variable, readable_name) {
  p <- ggplot(data, aes_string(x = variable, fill = "as.factor(stroke)")) +
    geom_bar(position = "fill") +
    labs(fill = "STROKE", y = "Proportion", 
         title = paste("Proportion of Stroke by", readable_name, "For 60 and Over Years Old")) +
    theme_minimal() +
    scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e"))
  
  ggplotly(p)
}

for (i in seq_along(variables_to_summarize)) {
  var <- variables_to_summarize[i]
  readable_name <- readable_names[i]
  bar_plot <- create_bar_chart(over_60_data, var, readable_name)
  print(bar_plot)
}


# Balancing Stratified Datasets
low_risk_age_tb <- outlier_stroke_tb %>%
  filter(age < 60)
low_risk_age_tb$stroke <- as.factor(low_risk_age_tb$stroke)

x_data <- low_risk_age_tb[, names(low_risk_age_tb) != "stroke"] 
y_data <- low_risk_age_tb$stroke

low_risk_age_tb <- upSample(x = x_data, y = y_data, yname = "stroke")
table(low_risk_age_tb$stroke)
low_risk_age_tb[sapply(low_risk_age_tb, is.double)] <- lapply(low_risk_age_tb[sapply(low_risk_age_tb, is.double)], scale)
write.csv(low_risk_age_tb, "data/datasets/low_risk_age_tb.csv")

high_risk_age_tb <- outlier_stroke_tb %>%
  filter(age >= 60)
high_risk_age_tb$stroke <- as.factor(high_risk_age_tb$stroke)

x_data <- high_risk_age_tb[, names(high_risk_age_tb) != "stroke"] 
y_data <- high_risk_age_tb$stroke

high_risk_age_tb <- upSample(x = x_data, y = y_data, yname = "stroke")
table(high_risk_age_tb$stroke)
high_risk_age_tb[sapply(high_risk_age_tb, is.double)] <- lapply(high_risk_age_tb[sapply(high_risk_age_tb, is.double)], scale)
write.csv(high_risk_age_tb, "data/datasets/high_risk_age_tb.csv")