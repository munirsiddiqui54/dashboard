# Load necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(cluster)  # For clustering analysis
library(caret) 

df<- read.csv("Flipkart.csv")

mean_rating <- mean(df$Rating, na.rm = TRUE)
df$Rating[is.na(df$Rating)] <- mean_rating
# After reading the dataset
df$Memory <- as.factor(df$Memory)
df$Storage <- as.factor(df$Storage)


ui <- dashboardPage(
  
  dashboardHeader(title = "Phone Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Overview", tabName = "dataset", icon = icon("database")),
      menuItem("Price Analysis", tabName = "price_analysis", icon = icon("bar-chart")),
      menuItem("Trending Phones", tabName = "trending", icon = icon("star")),
      menuItem("Prediction", tabName = "prediction", icon = icon("line-chart"))
    )
  ),
  
  
  # Dashboard Body
  dashboardBody(
    
    tabItems(
      
      # Dashboard tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                
                column(width = 8,
                       fluidRow(
                         box(plotOutput("priceComparisonPlot", height = "450px"), 
                             title = "Scatter Plot of Original Price vs Selling Price", 
                             solidHeader = TRUE, status = "success", width = 5+1),
                         # Selling Price Density Plot Box (4 units wide)
                         box(plotOutput("sellingPriceDensityPlot", height = "450px"), 
                             title = "Density Plot of Selling Prices", 
                             solidHeader = TRUE, status = "primary", width = 5+1),
                       ),
                       fluidRow(
                       
                       box(plotOutput("brandPopularityPlot", height = "450px"),
                           title = "Brand Popularity Chart", 
                           solidHeader = TRUE, status = "warning", width = 12)
                       )
                       ),
                
               
                
                # Column with two stacked boxes (7 units wide in total)
                column(width = 4,  # This column spans 7 out of 12 units
                       fluidRow(
                         # Box 1 inside the column (half of the 7 units)
                         box(plotOutput("brandSharePlot", height = "250px"), 
                             title = "Percentage Share of Different Brands", 
                             solidHeader = TRUE, status = "info", width = 12),
                         # Box 2 inside the column (remaining half)
                         box(title = "Price Statistics", status = "info", solidHeader = TRUE,
                             div(style = "height: 150px; overflow-y: scroll;",  
                                 tableOutput("priceStats")
                             ), width = 12
                         ),
                         box(plotOutput("clusterPlot", height = "400px"), 
                             title = "K-Means Clustering", solidHeader = TRUE, status = "info", width = 12)
                       )
                )
              )
              
      ),
      tabItem(tabName = "dataset",
              h2("Data set Overview"),
            
              fluidRow(
                box(title = "Dataset Summary", width = 12, status = "info", solidHeader = TRUE,
                    h3(textOutput("dataDimensions")),   
                    verbatimTextOutput("datasetStructure"),  # Output for str() function
                    tableOutput("missingValues"),   # Output for missing values
                    verbatimTextOutput("summaryStats"),      # Output for summary() function
                )
              ),
              
              fluidRow(
                box(title = "Sample Data", width = 12, status = "info", solidHeader = TRUE,
                    tableOutput("sampleRows")               # Output for sample rows
                )
               
              ),
              
              fluidRow(
                box(title = "Correlation Matrix", width = 12, status = "info", solidHeader = TRUE,
                    plotOutput("correlationPlot")           # Output for correlation plot
                )
              )
      ),
      
      
      # Trending Phones tab content
      tabItem(tabName = "trending",
              h2("Trending Phones"),
              fluidRow(
                box(title = "Trending Phones", width = 12, status = "success", solidHeader = TRUE,
                    plotOutput("trendingPlot", height = "400px")  # Placeholder for trending phones plot
                )
              )
      ),
      
      # Price Analysis tab content
      tabItem(tabName = "price_analysis",
              h2("Price Analysis"),
             fluidRow(
               box(
                 title = "Cheapest and Expensive Evaluation based on Original Price",
                 width = 6,
                 status="info",
                 fluidRow(
                   box(plotOutput("cheapestPlot", height = "300px"), title = "Top 10 Cheapest Phones (Original Price)",solidHeader = TRUE, status = "success", width = 6),
                   box(plotOutput("expensivePlot", height = "300px"), title = "Top 10 Expensive Phones (Original Price)", solidHeader = TRUE,status = "danger", width = 6)
                 )
               ),
               box(
                 title = "Cheapest and Expensive Evaluation based on Selling Price",
                 width = 6,
                 status="info",
                 fluidRow(
                   box(plotOutput("cheapestSellingPlot", height = "300px"), title = "Top 10 Cheapest Phones (Selling Price)",solidHeader = TRUE, status = "success", width = 6),
                   box(plotOutput("expensiveSellingPlot", height = "300px"), title = "Top 10 Expensive Phones (Selling Price)", solidHeader = TRUE,status = "danger", width = 6)
                 )
               )
             )
             ,
             fluidRow(
               box(plotOutput("topDiscountPlot", height = "400px"), title = "Top Phones with Highest Discounts", solidHeader = TRUE, status = "success", width = 12)
             )
             
      ),
      
      # Prediction tab content
      tabItem(tabName = "prediction",
              h2("Price Prediction Model"),
            
              fluidRow(
                box(title = "Description", width = 12, status = "info", solidHeader = TRUE,
                    "This price prediction model utilizes a linear regression approach to estimate the selling price of mobile phones based on key attributes such as brand, memory capacity, storage capacity, and original price. Leveraging a dataset sourced from Flipkart, the model aims to provide accurate price predictions that can assist consumers in making informed purchasing decisions and help retailers in pricing strategies."
                ),
                box(title = "Model", width = 12, status = "info", solidHeader = TRUE,
                    selectInput("inputBrand", "Select Brand:", choices = unique(df$Brand)),
                    selectInput("inputMemory", "Select Memory:", choices = unique(df$Memory)),
                    selectInput("inputStorage", "Select Storage:", choices = unique(df$Storage)),
                    numericInput("inputOriginalPrice", "Original Price:", value = 0, min = 0),
                    actionButton("predictButton", "Predict Selling Price"),
                    h2(textOutput("predictedPrice"))  # Output for the predicted price
                )
              ),
              fluidRow(
                box(plotOutput("model", height = "400px"), title = "Scatter Plot of Original Price vs Selling Price", solidHeader = TRUE, status = "info", width = 12)
              )
      )
    )
  )
)
server <- function(input, output) {
  
  #DASHBOARD
  
  output$brandSharePlot <- renderPlot({
    brand_counts <- as.data.frame(table(df$Brand))  # Count the number of phones per brand
    colnames(brand_counts) <- c("Brand", "Count")    # Rename columns for clarity
    
    # Create the pie chart
    ggplot(brand_counts, aes(x = "", y = Count, fill = Brand)) +
      geom_bar(stat = "identity", width = 1) +  # Create a bar chart with width = 1
      coord_polar(theta = "y") +  # Transform the bar chart into a pie chart
      labs(title = "Brand Share in Dataset") +
      theme_void() +  # Remove background and axes
      theme(legend.position = "right")  # Position the legend on the right
  })
  
  output$priceComparisonPlot <- renderPlot({
    ggplot(df, aes(x = Original.Price, y = Selling.Price)) +
      geom_point(color = "green", alpha = 0.5) +  # Scatter points
      labs(title = "Scatter Plot of Original Price vs Selling Price",
           x = "Original Price",
           y = "Selling Price") +
      theme_minimal()  # Minimal theme for better aesthetics
  })
  
  output$brandPopularityPlot <- renderPlot({
    brand_counts <- as.data.frame(table(df$Brand))  # Count the number of phones per brand
    colnames(brand_counts) <- c("Brand", "Count")    # Rename columns for clarity
    
    ggplot(brand_counts, aes(x = reorder(Brand, -Count), y = Count)) +
      geom_bar(stat = "identity", fill = "orange") +  # Bar color
      labs(title = "Brand Popularity", 
           x = "Brand", 
           y = "Number of Phones") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, size = 10)) +  # Rotate x-axis labels vertically and adjust size
      scale_x_discrete(expand = expansion(mult = c(0.02, 0.02))) +  # Add some space around the labels
      theme_minimal()  # Minimal theme for better aesthetics
  })
  output$priceStats <- renderTable({
    selling_prices <- df$Selling.Price  # Adjust this to your actual selling price column name
    original_prices <- df$Original.Price  # Adjust this to your actual original price column name
    
    # Calculate statistics for Selling Price
    mean_selling <- mean(selling_prices, na.rm = TRUE)
    median_selling <- median(selling_prices, na.rm = TRUE)
    min_selling <- min(selling_prices, na.rm = TRUE)
    max_selling <- max(selling_prices, na.rm = TRUE)
    
    # Calculate statistics for Original Price
    mean_original <- mean(original_prices, na.rm = TRUE)
    median_original <- median(original_prices, na.rm = TRUE)
    min_original <- min(original_prices, na.rm = TRUE)
    max_original <- max(original_prices, na.rm = TRUE)
    
    # Create a data frame with the results
    stats <- data.frame(
      Statistic = c("Mean", "Median",  "Minimum", "Maximum"),
      Selling = c(mean_selling, median_selling,  min_selling, max_selling),
      Original = c(mean_original, median_original, min_original, max_original)
    )
    
    return(stats)
  })
  output$sellingPriceDensityPlot <- renderPlot({
    ggplot(df, aes(x = Selling.Price)) +
      geom_density(fill = "blue", alpha = 0.5) +  # Adjust fill and transparency
      labs(title = "Density Plot of Selling Prices",
           x = "Selling Price",
           y = "Density") +
      theme_minimal()  # Use minimal theme for better aesthetics
  })
  
  # K-Means Clustering Plot
  output$clusterPlot <- renderPlot({
    # Select numeric columns for clustering
    numeric_df <- df %>% select(where(is.numeric))
    
    # Perform K-Means clustering (let's assume k = 3 for demonstration purposes)
    kmeans_result <- kmeans(numeric_df, centers = 3, nstart = 20)
    
    # Append cluster labels to the dataset
    df$Cluster <- as.factor(kmeans_result$cluster)
    
    # Plot clusters (using the first two principal components for visualization)
    pca <- prcomp(numeric_df, scale. = TRUE)  # Principal component analysis
    pca_df <- as.data.frame(pca$x)  # Get principal components
    pca_df$Cluster <- df$Cluster  # Add clusters to the data
    
    # Scatter plot of clusters
    ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
      geom_point(alpha = 0.6, size = 3) +
      labs(title = "K-Means Clustering (PCA)", x = "Principal Component 1", y = "Principal Component 2") +
      theme_minimal() +
      scale_color_brewer(palette = "Set1")
  })

  #DATASET Overview
  output$datasetStructure <- renderPrint({
    str(df)
  })
  
  # Output for summary()
  output$summaryStats <- renderPrint({
    summary(df)
  })
  
  output$sampleRows <- renderTable({
    head(df)  # Display first few rows
  })
  output$dataDimensions <- renderText({
    paste(dim(df)[1], "rows and ", dim(df)[2], "columns.")
  })
  
  output$missingValues <- renderTable({
    # Get the missing values
    missing_data <- colSums(is.na(df))
    # Get the data types
    data_types <- sapply(df, class)
    
    # Create a data frame that combines both pieces of information
    merged_data <- data.frame(
      Column = names(missing_data),
      Type = data_types,
      MissingValues = missing_data,
      stringsAsFactors = FALSE  # Avoid factors for better readability
    )
    
    return(merged_data)
  })

  
  library(corrplot)
  
  output$correlationPlot <- renderPlot({
    corr <- cor(df[sapply(df, is.numeric)], use = "complete.obs")
    corrplot(corr, method = "color", 
             title = "Correlation Matrix", 
             tl.col = "black",  # Text label color
             tl.srt = 45,       # Text label rotation
             addgrid.col = "grey")
  })
  
  
  output$keyStats <- renderTable({
    summary_stats <- df %>% summarise(
      Average_Original_Price = mean(Original.Price, na.rm = TRUE),
      Average_Selling_Price = mean(Selling.Price, na.rm = TRUE),
      Average_Rating = mean(Rating, na.rm = TRUE)
    )
    return(summary_stats)
  })
  
  library(DT)
  
  output$dataTable <- renderDT({
    datatable(df)
  })
  
  
  
  
  # price analysis
  # Calculate discount percentage again
  df_unique <- df %>%
    distinct(Model, .keep_all = TRUE)
  
  # Calculate discount percentage
  df_unique <- df_unique %>%
    mutate(Discount_Percentage = ((Original.Price - Selling.Price) / Original.Price) * 100)
  
  # Select top discounted phones
  # Select top 8 unique phones with maximum discounts
  top_discounted_phones <- df_unique %>%
    arrange(desc(Discount_Percentage)) %>%
    select(Model, Original.Price, Selling.Price, Discount_Percentage) %>%
    head(15)
  
  print(top_discounted_phones)
  
  output$topDiscountPlot <- renderPlot({
    ggplot(top_discounted_phones, aes(x =Discount_Percentage , y =reorder(Model, -Discount_Percentage),fill=Discount_Percentage)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 15 Phones with Maximum Discounts", 
           y = "Phone Model", 
           x = "Discount Percentage") +
      theme_minimal() +
      coord_flip()
  })
  
  
  
  #Based on Original Price
  unique_cheap_phones <- df[order(df$Original.Price), ]
  cheapest_phones <- unique_cheap_phones[!duplicated(unique_cheap_phones$Model), ][1:5, ]
  
  unique_expensive_phones <- df[order(-df$Original.Price), ]
  expensive_phones <- unique_expensive_phones[!duplicated(unique_expensive_phones$Model), ][1:5, ]
  
  output$cheapestPlot <- renderPlot({
    ggplot(data = cheapest_phones, aes(x = reorder(Model, Original.Price), y = Original.Price)) +
      geom_bar(stat = "identity", fill = "green") +   # Fill color for the bars
      labs(title = "Top 10 Cheapest Phones", 
           x = "Phone Model", 
           y = "Original Price") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
      coord_flip()  # Flip coordinates for better visibility
  })
  
  output$expensivePlot <- renderPlot({
    ggplot(data = expensive_phones, aes(x = reorder(Model, Original.Price), y = Original.Price)) +
      geom_bar(stat = "identity", fill = "blue") +   # Fill color for the bars
      labs(title = "Top 10 Expensive Phones", 
           x = "Phone Model", 
           y = "Original Price"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      coord_flip()  
  })
  
  # Based on Selling Price
  unique_cheap_selling_phones <- df[order(df$Selling.Price), ]
  cheapest_selling_phones <- unique_cheap_selling_phones[!duplicated(unique_cheap_selling_phones$Model), ][1:5, ]
  
  unique_expensive_selling_phones <- df[order(-df$Selling.Price), ]
  expensive_selling_phones <- unique_expensive_selling_phones[!duplicated(unique_expensive_selling_phones$Model), ][1:5, ]
  
  # Render plot for Cheapest Phones based on Selling Price
  output$cheapestSellingPlot <- renderPlot({
    ggplot(data = cheapest_selling_phones, aes(x = reorder(Model, Selling.Price), y = Selling.Price)) +
      geom_bar(stat = "identity", fill = "orange") +   # Fill color for the bars
      labs(title = "Top 10 Cheapest Phones (Selling Price)", 
           x = "Phone Model", 
           y = "Selling Price") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
      coord_flip() +
      scale_y_continuous(breaks = seq(0, max(cheapest_selling_phones$Selling.Price), by = 500))# Flip coordinates for better visibility
  })
  
  # Render plot for Expensive Phones based on Selling Price
  output$expensiveSellingPlot <- renderPlot({
    ggplot(data = expensive_selling_phones, aes(x = reorder(Model, Selling.Price), y = Selling.Price)) +
      geom_bar(stat = "identity", fill = "red") +   # Fill color for the bars
      labs(title = "Top 10 Expensive Phones (Selling Price)", 
           x = "Phone Model", 
           y = "Selling Price") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      coord_flip()  
  })
  

  
  output$trendingPlot <- renderPlot({
    top_brands <- as.data.frame(table(df$Brand))
    colnames(top_brands) <- c("Brand", "Frequency")
    
    ggplot(top_brands, aes(x = Brand, y = Frequency, fill = Brand)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Trending Brands", x = "Brand", y = "Number of Phones") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Example plotting function for price analysis
  output$pricePlot <- renderPlot({
    ggplot(df, aes(x = Original.Price)) +
      geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Original Price Distribution", x = "Price", y = "Frequency")
  })
  
  #Price prediction
  # Load necessary libraries for machine learning
  library(caret)  # For creating models
  
  # Train a machine learning model when the app starts
  # Create the linear model for price prediction
  model <- lm(Selling.Price ~ Brand + Memory + Storage + Original.Price, data = df)
  
  
  # Make predictions when the button is clicked
  observeEvent(input$predictButton, {
    # Prepare input data for prediction
    new_data <- data.frame(
      Brand = input$inputBrand,
      Memory = input$inputMemory,
      Storage = input$inputStorage,
      Original.Price = input$inputOriginalPrice
    )
    
    # Predict the selling price
    predicted_price <- predict(model, new_data)
    
    # Update the predicted price output
    output$predictedPrice <- renderText({
      paste("₹", round(predicted_price, 2))
    })
  })
  output$model <- renderPlot({
    ggplot(df, aes(x = Original.Price, y = Selling.Price)) +
      geom_point(color = "blue", alpha = 0.5) +  # Scatter points
      geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add regression line
      labs(title = "Scatter Plot of Original Price vs Selling Price",
           x = "Original Price",
           y = "Selling Price") +
      theme_minimal()  # Minimal theme for better aesthetics
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
