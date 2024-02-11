#Import Libiraries
library(readr)
library(ggplot2)  
library(shiny)
library(shinydashboard)
#11111111111111111111111111111111111111111111111111   Data Preprocessing   11111111111111111111111111111111111111111111111111#
udemy <- read_csv("../udemy.csv")
str(udemy)
summary(udemy)

udemy<- udemy[-1]

selected_columns <- c("course_title", "is_paid", "price", "num_subscribers", "num_reviews", 
                      "num_lectures", "level", "content_duration", "published_timestamp", "subject")
udemy <- udemy[, selected_columns]

udemy$published_timestamp <- as.POSIXct(udemy$published_timestamp, format="%Y-%m-%d %H:%M:%S")

# Replace missing values with the mean of the column
udemy$price[is.na(udemy$price)] <- mean(udemy$price, na.rm = TRUE)
udemy$num_subscribers[is.na(udemy$num_subscribers)] <- mean(udemy$num_subscribers, na.rm = TRUE)
udemy$num_reviews[is.na(udemy$num_reviews)] <- mean(udemy$num_reviews, na.rm = TRUE)
udemy$num_lectures[is.na(udemy$num_lectures)] <- mean(udemy$num_lectures, na.rm = TRUE)
udemy$content_duration[is.na(udemy$content_duration)] <- mean(udemy$content_duration, na.rm = TRUE)

#remove missing v and duplicated data
udemy <- na.omit(udemy)
udemy <- udemy[!duplicated(udemy),]

#check a if still non-cleaned
missing_values_count <- colSums(is.na(udemy))
print(missing_values_count)
is.null(udemy)
duplicated(udemy)

#stastics
mean(udemy$price)
median(udemy$num_subscribers)
mode(udemy$num_lectures)
min(udemy$num_reviews)
max(udemy$num_reviews)

str(udemy)
summary(udemy)
#2222222222222222222222222222222222222222222222222222   Shiny & ggplot  22222222222222222222222222222222222222222222222222222#
# Define the UI
ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "Udemy Courses Explorer"),
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Scatter plot", tabName = "scatter"),
        menuItem("Box plot", tabName = "box"),
        menuItem("Histogram Plot", tabName = "hist"),
        menuItem("Bar plot", tabName = "bar"),
        menuItem("Time Series plot", tabName = "time"),
        menuItem("Line plot", tabName = "line"),
        menuItem("Modifying axes plot", tabName = "axes"),
        menuItem("Jitter plot", tabName = "jitter"),
        menuItem("Density plot", tabName = "density"),
        menuItem("Pie Chart", tabName = "pie"),
        # Add a selector for choosing the level
        selectInput("level_selector", label = "Select Level:",
                    choices = c("All Levels", "Beginner Level", "Intermediate Level", "Expert Level"),
                    selected = "All Levels"),
        actionButton("percentage_button", "Percentage"),
        # Add a selector for choosing the paid status
        selectInput("paid_selector", label = "Select Paid Status:",
                    choices = c("All", "False", "True"),
                    selected = "All"),
        # Add a selector for choosing the subject
        selectInput("subject_selector", label = "Select Subject:",
                    choices = c("All Subjects", "Business Finance", "Graphic Design", 
                                "Musical Instruments", "Web Development"),
                    selected = "All Subjects")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "scatter", plotOutput("scatterPlot")),
        tabItem(tabName = "box", plotOutput("boxPlot")),
        tabItem(tabName = "hist", plotOutput("histPlot")),
        tabItem(tabName = "bar", plotOutput("barPlot")),
        tabItem(tabName = "time", plotOutput("timePlot")),
        tabItem(tabName = "line", plotOutput("linePlot")),
        tabItem(tabName = "axes", plotOutput("axesPlot")),
        tabItem(tabName = "jitter", plotOutput("jitterPlot")),
        tabItem(tabName = "density", plotOutput("densityPlot")),
        tabItem(tabName = "pie", plotOutput("pieChart"))
      )
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Reactive data filtering based on selected level
  filtered_data <- reactive({
    selected_level <- input$level_selector
    
    if (selected_level == "All Levels") {
      return(udemy)
    } else {
      return(subset(udemy, level == selected_level))
    }
  })
  
  # Scatter plot
  output$scatterPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = num_subscribers, y = num_reviews, color = level)) +
      geom_point(na.rm = FALSE) +
      labs(title = "Scatter Plot of Number of Subscribers vs Number of Reviews",
           x = "Number of Subscribers",
           y = "Number of Reviews")
  })
  
  # Boxplot
  output$boxPlot <- renderPlot({
    selected_subject <- input$subject_selector
    filtered_data <- filtered_data()
    
    if (selected_subject == "All Subjects") {
      ggplot(filtered_data, aes(subject, price)) +
        geom_boxplot() +
        labs(title = "Boxplot of Course Price by Subject",
             x = "Subject",
             y = "Course Price")
    } else {
      ggplot(subset(filtered_data, subject == selected_subject), aes(subject, price)) +
        geom_boxplot() +
        labs(title = paste("Boxplot of Course Price for", selected_subject),
             x = "Subject",
             y = "Course Price")
    }
  })
  
  # Histogram
  output$histPlot <- renderPlot({
    selected_paid_status <- input$paid_selector
    filtered_data <- filtered_data()
    
    if (selected_paid_status == "All") {
      ggplot(filtered_data, aes(num_lectures, fill = is_paid)) +
        geom_histogram(binwidth = 10, position = "dodge") +
        labs(title = "Histogram of Number of Lectures by Paid Status",
             x = "Number of Lectures",
             fill = "Paid Status")
    } else {
      ggplot(subset(filtered_data, is_paid == as.logical(selected_paid_status)), 
             aes(num_lectures, fill = is_paid)) +
        geom_histogram(binwidth = 10, position = "dodge") +
        labs(title = paste("Histogram of Number of Lectures for Paid Status:", 
                           selected_paid_status),
             x = "Number of Lectures",
             fill = "Paid Status")
    }
  })
  
  
  # Bar Chart
  output$barPlot <- renderPlot({
    selected_subject <- input$subject_selector
    filtered_data <- filtered_data()
    
    if (selected_subject == "All Subjects") {
      ggplot(filtered_data, aes(x = subject)) +
        geom_bar() +
        labs(title = "Bar Chart of Course Counts by Subject",
             x = "Subject",
             y = "Number of Courses") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      ggplot(subset(filtered_data, subject == selected_subject), aes(x = subject)) +
        geom_bar() +
        labs(title = paste("Bar Chart of Course Counts for", selected_subject),
             x = "Subject",
             y = "Number of Courses") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # Time Series
  output$timePlot <- renderPlot({
    ggplot(udemy, aes(x = as.Date(published_timestamp), y = num_subscribers)) +
      geom_line() +
      labs(title = "Time Series of Number of Subscribers Over Time",
           x = "Published Timestamp",
           y = "Number of Subscribers")
  })
  
  # Line plot with smoother
  output$linePlot <- renderPlot({
    ggplot(udemy, aes(x = num_lectures, y = num_reviews)) +
      geom_point() +
      geom_smooth() +
      labs(title = "Line Plot of Lectures vs Reviews with Smoother",
           x = "Number of Lectures",
           y = "Number of Reviews")
  })
  
  # Modifying axes plot
  output$axesPlot <- renderPlot({
    ggplot(udemy, aes(x = content_duration, y = num_reviews)) +
      geom_point(alpha = 0.5) +
      xlab("Content Duration") +
      ylab("Number of Reviews") +
      labs(title = "Scatter Plot with Modified Axes")
  })
  
  # Jitter plot
  output$jitterPlot <- renderPlot({
    selected_level <- input$level_selector
    filtered_data <- filtered_data()
    
    ggplot(filtered_data, aes(x = level, y = num_subscribers)) +
      geom_jitter() +
      labs(title = "Jitter Plot of Subscribers by Level",
           x = "Level",
           y = "Number of Subscribers")
  })
  
  # Density plot with dynamic filter
  output$densityPlot <- renderPlot({
    selected_level <- input$level_selector
    
    if (selected_level == "All Levels") {
      ggplot(udemy, aes(x = num_lectures, fill = level)) +
        geom_density(alpha = 0.5) +
        labs(title = "Density Plot of Lectures by Level",
             x = "Number of Lectures",
             fill = "Level")
    } else {
      ggplot(subset(udemy, level == selected_level), aes(x = num_lectures, fill = level)) +
        geom_density(alpha = 0.5) +
        labs(title = paste("Density Plot of Lectures for", selected_level, "Level"),
             x = "Number of Lectures",
             fill = "Level")
    }
  })
  
  # Pie chart with ggplot2
  output$pieChart <- renderPlot({
    course_counts <- table(udemy$level)
    labels <- paste(names(course_counts), "\n", course_counts, " courses")
    
    # Check if the "Percentage" button is clicked
    if (input$percentage_button > 0) {
      # Convert counts to percentages
      course_percentages <- prop.table(course_counts) * 100
      labels <- paste(names(course_percentages), "\n", round(course_percentages, 2), "%")
      
      pie(course_counts, labels = labels, col = rainbow(length(course_counts)),
          main = "Distribution of Courses by Level")
    } 
    else {
      pie(course_counts, labels = labels, col = rainbow(length(course_counts)),
          main = "Distribution of Courses by Level")
    }
  })
}

shinyApp(ui = ui, server = server)