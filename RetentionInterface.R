library(shiny)
library(data.table)
library(rpart)

uiRetention<-fluidPage(
                  pageWithSidebar(
                  headerPanel("Customer Retention"),
                  sidebarPanel(
                  fileInput('file1', 'Choose CSV File',
                            multiple = TRUE,
                            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                  downloadButton("downloadData", "Download")
                ),
                            mainPanel(
                              tableOutput("contents")
                            )
), tags$style(type="text/css",".shiny-output-error {visibility:hidden;}",
              ".shiny-output-error:before{visibility:hidden;}"
))

serverRetention <- function(input, output, session) {
    reactive_data <- reactive({
    print(input$file1$datapath)
    data <- fread(input$file1$datapath, header = T, sep = ",",quote = '"')
    
    data$Exited <- as.numeric(data$Exited)
    data$CustomerId <- as.integer(data$CustomerId)
    
    #Categorical data 
    data$Exited <- factor(data$Exited)
    data$HasCrCard <- factor(data$HasCrCard)
    data$IsActiveMember <- factor(data$IsActiveMember)
    data$Gender <- factor(data$Gender)
    data$Tenure <- factor(data$Tenure)
    data$Geography <- factor(data$Geography)
    data$NumOfProducts <- factor(data$NumOfProducts)
    data$Promomethod <- factor(data$Promomethod)
    data$Language_provided <- factor(data$Language_provided)
    data$Language_preferred <- factor(data$Language_preferred)
    
    #Numerical data 
    data$Age <- as.numeric(data$Age)
    data$CreditScore <- as.numeric(data$CreditScore)
    data$Balance <- as.numeric(data$Balance)
    data$EstimatedSalary <- as.numeric(data$EstimatedSalary)
    
    return(data)
})
    output$contents <- renderTable({
      
      data <- reactive_data()
      data1 <- data.frame(predict(PrunedCARTmodel1,newdata=data,type="class"))
      data2 <- cbind(as.integer(data$CustomerId),as.numeric(data$CreditScore)
                     ,factor(data$Geography),factor(data$Gender),as.numeric(data$Age)
                     ,factor(data$Tenure),as.numeric(data$Balance),factor(data$NumOfProducts)
                     ,factor(data$HasCrCard),factor(data$IsActiveMember)
                     ,as.numeric(data$EstimatedSalary),factor(data$Promomethod)
                     ,factor(data$Language_provided),factor(data$Language_preferred),data1)
      #leaving <- subset(data2, data1==1)
      colnames(data2) <- c("Customer ID","CreditScore","Geography","Gender"
                             ,"Age","Tenure","Balance","NumOfProducts","HasCrCard","IsActiveMember"
                             ,"EstimatedSalary","Promomethod","Language_provided","Language_preferred"
                             ,"Predicted Retention")
      
      data2
}) 
    
    
    output$downloadData <- downloadHandler(
      filename = "leaving.csv",
      content = function(file) {
        
        data <- reactive_data()
        data1 <- data.frame(predict(PrunedCARTmodel1,newdata=data,type="class"))
        data2 <- cbind(as.integer(data$CustomerId),as.numeric(data$CreditScore)
                       ,factor(data$Geography),factor(data$Gender),as.numeric(data$Age)
                       ,factor(data$Tenure),as.numeric(data$Balance),factor(data$NumOfProducts)
                       ,factor(data$HasCrCard),factor(data$IsActiveMember)
                       ,as.numeric(data$EstimatedSalary),factor(data$Promomethod)
                       ,factor(data$Language_provided),factor(data$Language_preferred),data1)
        
        colnames(data2) <- c("CustomerId","CreditScore","Geography","Gender"
                             ,"Age","Tenure","Balance","NumOfProducts","HasCrCard","IsActiveMember"
                             ,"EstimatedSalary","Promomethod","Language_provided","Language_preferred"
                             ,"PredictedRetention")
        
        leaving <- subset(data2, data1==1)
        
        
        write.csv(leaving, file, row.names = TRUE)
      }
    )
    
}

shinyApp(uiRetention,serverRetention)
