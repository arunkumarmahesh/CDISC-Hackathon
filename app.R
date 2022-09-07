library(shiny)
library(RJSONIO)
library(rjson)
library(DT)

########################
shinyApp(
  ui = fluidPage(
    fileInput("Json", "Choose Json File",
              multiple = FALSE,
              accept = c(".json")),
    DTOutput('tbl')
  ),
  server = function(input, output) {
    
    data <- reactive({
      
      req(input$Json)
      
      item <-  fromJSON(file = input$Json$datapath)
      
      z <- names(item$clinicalData$itemGroupData[1])
      
      item <- eval(parse(text = paste("item$clinicalData$itemGroupData$",z,"$items",sep = '')))
      
      x <- c()
      y <- c()
      
      for ( i in 1:length(item)){
        
        y[i] <- item[[i]][3]
        
      }
      
      label <- as.data.frame(y)
      
      item1 <-  fromJSON(file = input$Json$datapath)
      
      z <- names(item1$clinicalData$itemGroupData[1])
      
      itemData1 <- eval(parse(text = paste("item1$clinicalData$itemGroupData$",z,"$itemData",sep = '')))
      
      data_item <- data.frame(do.call("rbind",itemData1))
      
      colnames(data_item) <- colnames(label)
      
      
      data_final <- rbind(label, data_item)
      
      ########################
      
      colnames(data_final) <- label[1,]
      
      data_final <- data_final[-1,]
      
    })
    
    
    
   # output$tbl = renderDT({
      
      #data()

    #})
    
    output$tbl <- DT::renderDataTable({
      datatable(data(),extensions = c('Scroller','Buttons'), options = list(
        deferRender = TRUE,
        scrollY = 2000,
        scrollX = 5,
        scroller = TRUE,dom = 'Bfrtip',
        exportOptions = list(header = ''),
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),filter = 'top'
      )
    })
      
  }
)

############################
