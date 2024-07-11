# Load necessary libraries
library(shiny)
library(ggplot2)
library(corrplot)  # Load corrplot package

# Define UI for application
ui <- fluidPage(
  titlePanel("Diabetes Data Visualization"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      # Add input widgets here if needed
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Correlation Plot", plotOutput("correlation_plot")),
        tabPanel("Histogram", plotOutput("histogram")),
        tabPanel("Boxplot", plotOutput("boxplot")),
        tabPanel("Density Plot", plotOutput("density_plot"))
        # Add more tabs as needed
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Read data
  diabetes <- read.csv("diabetes.csv")
  
  # Correlation Plot
  output$correlation_plot <- renderPlot({
    # Calculate correlation matrix
    corr_matrix <- cor(diabetes[, c("Pregnancies", "Glucose", "BloodPressure", 
                                    "SkinThickness", "Insulin", "BMI", 
                                    "DiabetesPedigreeFunction", "Age")])
    
    # Create correlation plot using corrplot
    corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.7)
  })
  
  # Histogram
  output$histogram <- renderPlot({
    ggplot(diabetes, aes(x = Glucose)) +
      geom_histogram(binwidth = 5, fill = "blue", color = "black") +
      labs(title = "Histogram of Glucose", x = "Glucose", y = "Frequency")
  })
  
  # Boxplot
  output$boxplot <- renderPlot({
    ggplot(diabetes, aes(x = as.factor(Outcome), y = BMI)) +
      geom_boxplot(fill = "lightgreen", color = "black") +
      labs(title = "Boxplot of BMI by Outcome", x = "Outcome", y = "BMI")
  })
  
  # Density Plot
  output$density_plot <- renderPlot({
    ggplot(diabetes, aes(x = Age, fill = as.factor(Outcome))) +
      geom_density(alpha = 0.5) +
      labs(title = "Density Plot of Age by Outcome", x = "Age", y = "Density") +
      scale_fill_manual(values = c("orange", "blue"), name = "Outcome",
                        labels = c("No Diabetes", "Diabetes"))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

