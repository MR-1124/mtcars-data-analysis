# Load necessary libraries
install.packages(c("ggplot2", "corrplot", "plotly", "scatterplot3d", "shiny", "rpart", "randomForest", "factoextra"))
library(ggplot2)
library(corrplot)
library(plotly)
library(scatterplot3d)
library(shiny)
library(rpart)
library(randomForest)
library(factoextra)

# Load the mtcars dataset
data(mtcars)

# View the first few rows of the dataset
head(mtcars)

# Summary statistics of the dataset
summary(mtcars)

# Check for missing values
sum(is.na(mtcars))

# Handle outliers (example: removing outliers based on z-scores)
z_scores <- scale(mtcars)
outliers <- which(apply(z_scores, 1, function(x) any(abs(x) > 3)))
mtcars_cleaned <- mtcars[-outliers, ]

# Convert categorical variables to factors
mtcars_cleaned$cyl <- as.factor(mtcars_cleaned$cyl)
mtcars_cleaned$gear <- as.factor(mtcars_cleaned$gear)

# Create a new feature: horsepower per weight
mtcars_cleaned$hp_per_wt <- mtcars_cleaned$hp / mtcars_cleaned$wt

# Basic Plots
# Histogram of Miles Per Gallon (mpg)
hist(mtcars_cleaned$mpg, main="Histogram of MPG", xlab="Miles Per Gallon", col="blue")

# Scatter plot of MPG vs Weight
plot(mtcars_cleaned$wt, mtcars_cleaned$mpg, main="Scatterplot of MPG vs Weight", xlab="Weight (1000 lbs)", ylab="Miles Per Gallon", pch=19)

# Box plot of MPG by Cylinder
boxplot(mtcars_cleaned$mpg ~ mtcars_cleaned$cyl, main="Boxplot of MPG by Cylinder", xlab="Number of Cylinders", ylab="Miles Per Gallon")

# Scatter plot of MPG vs Weight using ggplot2
ggplot(mtcars_cleaned, aes(x=wt, y=mpg)) +
  geom_point() +
  ggtitle("Scatterplot of MPG vs Weight") +
  xlab("Weight (1000 lbs)") +
  ylab("Miles Per Gallon")

# Box plot of MPG by Cylinder using ggplot2
ggplot(mtcars_cleaned, aes(x=factor(cyl), y=mpg)) +
  geom_boxplot() +
  ggtitle("Boxplot of MPG by Cylinder") +
  xlab("Number of Cylinders") +
  ylab("Miles Per Gallon")

# Density plot of MPG
ggplot(mtcars_cleaned, aes(x=mpg)) +
  geom_density(fill="blue", alpha=0.5) +
  ggtitle("Density Plot of MPG") +
  xlab("Miles Per Gallon")

# Pair plot (scatterplot matrix)
pairs(mtcars_cleaned, main="Scatterplot Matrix")

# Correlation heatmap
cor_matrix <- cor(mtcars_cleaned[sapply(mtcars_cleaned, is.numeric)])
corrplot(cor_matrix, method="color", addCoef.col = "black", number.cex = 0.7)

# Data Transformation: Standardization
mtcars_scaled <- as.data.frame(scale(mtcars_cleaned))
head(mtcars_scaled)

# Statistical Analysis: Correlation
cor(mtcars_cleaned$mpg, mtcars_cleaned$wt)

# T-test: MPG by number of cylinders (4 vs 6)
t.test(mtcars_cleaned$mpg[mtcars_cleaned$cyl == 4], mtcars_cleaned$mpg[mtcars_cleaned$cyl == 6])

# ANOVA: MPG by number of cylinders
anova_result <- aov(mpg ~ as.factor(cyl), data=mtcars_cleaned)
summary(anova_result)

# 3D scatter plot
scatterplot3d(mtcars_cleaned$wt, mtcars_cleaned$hp, mtcars_cleaned$mpg,
              main="3D Scatterplot of Weight, HP, and MPG",
              xlab="Weight (1000 lbs)", ylab="Horsepower", zlab="MPG")

# Shiny Dashboard
ui <- fluidPage(
  titlePanel("Mtcars Data Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "X-axis variable", choices = names(mtcars_cleaned)),
      selectInput("yvar", "Y-axis variable", choices = names(mtcars_cleaned), selected = "mpg")
    ),
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

server <- function(input, output) {
  output$scatterPlot <- renderPlot({
    ggplot(mtcars_cleaned, aes_string(x = input$xvar, y = input$yvar)) +
      geom_point() +
      ggtitle(paste("Scatterplot of", input$yvar, "vs", input$xvar)) +
      xlab(input$xvar) +
      ylab(input$yvar)
  })
}

shinyApp(ui = ui, server = server)

# Machine Learning Models
# Linear regression model to predict MPG
model <- lm(mpg ~ wt + hp + qsec + am, data=mtcars_cleaned)
summary(model)

# Decision Tree model
tree_model <- rpart(mpg ~ ., data=mtcars_cleaned)
print(tree_model)

# Random Forest model
rf_model <- randomForest(mpg ~ ., data=mtcars_cleaned)
print(rf_model)

# Multivariate analysis: PCA
mtcars_pca <- prcomp(mtcars_cleaned[, sapply(mtcars_cleaned, is.numeric)], scale=TRUE)
summary(mtcars_pca)

# Plot PCA
fviz_pca_biplot(mtcars_pca, label="var", habillage=mtcars_cleaned$cyl)

# Conclusion and Insights
cat("Summary of Findings:
1. The histogram and density plot show that MPG is roughly normally distributed.
2. The scatter plot indicates a negative correlation between weight and MPG.
3. The box plot shows that cars with 4 cylinders generally have higher MPG than those with 6 or 8 cylinders.
4. The correlation heatmap highlights strong correlations between various attributes.
5. Statistical tests confirm significant differences in MPG based on the number of cylinders.
")

# Save ggplot
ggsave("scatterplot_mpg_vs_weight.png")

# Save base R plot
png("histogram_mpg.png")
hist(mtcars_cleaned$mpg, main="Histogram of MPG", xlab="Miles Per Gallon", col="blue")
dev.off()

