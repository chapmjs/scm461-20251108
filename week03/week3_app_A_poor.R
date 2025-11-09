library(shiny)
library(dplyr)
library(readr)

parts<-read_csv("parts_master.csv")
inventory<-read_csv("current_inventory.csv")
capacity<-read_csv("capacity_data.csv")

ui<-fluidPage(
  selectInput("p","Part:",choices=parts$part_number),
  tableOutput("t")
)

server<-function(input,output,session){
  output$t<-renderTable({
    capacity%>%filter(part_number==input$p)
  })
}

shinyApp(ui,server)
