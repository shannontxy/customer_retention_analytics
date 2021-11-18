library(shiny)
library(data.table)
library(rpart)

uiPromo<-fluidPage(
  pageWithSidebar(
    headerPanel("Promotional Method"),
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

serverPromo <- function(input, output, session) {
  reactive_dataP <- reactive({
    print(input$file1$datapath)
    dataP <- fread(input$file1$datapath, header = T, sep = ",",quote = '"')
    
    dataP$Exited <- as.numeric(dataP$Exited)
    dataP$CustomerId <- as.integer(dataP$CustomerId)
    
    #Categorical data 
    dataP$Exited <- factor(dataP$Exited)
    dataP$HasCrCard <- factor(dataP$HasCrCard)
    dataP$IsActiveMember <- factor(dataP$IsActiveMember)
    dataP$Gender <- factor(dataP$Gender)
    dataP$Tenure <- factor(dataP$Tenure)
    dataP$Geography <- factor(dataP$Geography)
    dataP$Promomethod <- factor(dataP$Promomethod)
    dataP$Language_provided <- factor(dataP$Language_provided)
    dataP$Language_preferred <- factor(dataP$Language_preferred)
    
    #Numerical data 
    dataP$Age <- as.numeric(dataP$Age)
    dataP$CreditScore <- as.numeric(dataP$CreditScore)
    dataP$NumOfProducts <- as.numeric(dataP$NumOfProducts)
    dataP$Balance <- as.numeric(dataP$Balance)
    dataP$EstimatedSalary <- as.numeric(dataP$EstimatedSalary)
    
    return(dataP)
  })
  output$contents <- renderTable({
    
    dataP <- reactive_dataP()
    data1P <- data.frame(predict(PrunedCARTmodel2,newdata=dataP,type="class"))
    data1P2 <- data.frame(predict(PrunedCARTmodel3,newdata=dataP,type="class"))
    data2P <- cbind(as.integer(dataP$CustomerId),as.numeric(dataP$CreditScore)
                   ,factor(dataP$Geography),factor(dataP$Gender),as.numeric(dataP$Age)
                   ,factor(dataP$Tenure),as.numeric(dataP$Balance),as.numeric(dataP$NumOfProducts)
                   ,factor(dataP$HasCrCard),factor(dataP$IsActiveMember)
                   ,as.numeric(dataP$EstimatedSalary),factor(dataP$Promomethod)
                   ,factor(dataP$Language_provided),factor(dataP$Language_preferred),data1P, data1P2)
    colnames(data2P) <- c("CustomerId","CreditScore","Geography","Gender"
                         ,"Age","Tenure","Balance","NumOfProducts","HasCrCard","IsActiveMember"
                         ,"EstimatedSalary","Current-Promotional-Method","Language_provided"
                         ,"Language_preferred","Suggested-Promotional-Method", "Personalisation")
    
    data2P
  }) 
  
  
  output$downloadData <- downloadHandler(
    filename = "promotionalmethod.csv",
    content = function(file) {
      
      dataP <- reactive_dataP()
      data1P <- data.frame(predict(PrunedCARTmodel2,newdata=dataP,type="class"))
      data1P2 <- data.frame(predict(PrunedCARTmodel3,newdata=dataP,type="class"))
      data2P <- cbind(as.integer(dataP$CustomerId),as.numeric(dataP$CreditScore)
                      ,factor(dataP$Geography),factor(dataP$Gender),as.numeric(dataP$Age)
                      ,factor(dataP$Tenure),as.numeric(dataP$Balance),as.numeric(dataP$NumOfProducts)
                      ,factor(dataP$HasCrCard),factor(dataP$IsActiveMember)
                      ,as.numeric(dataP$EstimatedSalary),factor(dataP$Promomethod)
                      ,factor(dataP$Language_provided),factor(dataP$Language_preferred),data1P,data1P2)
      colnames(data2P) <- c("CustomerId","CreditScore","Geography","Gender"
                            ,"Age","Tenure","Balance","NumOfProducts","HasCrCard","IsActiveMember"
                            ,"EstimatedSalary","Current-Promotional-Method","Language_provided"
                            ,"Language_preferred","Suggested-Promotional-Method","Personalisation")
      
      write.csv(data2P, file, row.names = TRUE)
})}

shinyApp(uiPromo,serverPromo)


