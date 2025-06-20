install.packages("shiny")
install.packages("VennDiagram")
install.packages("gridExtra")  # For displaying the Venn diagram

library(shiny)
library(VennDiagram)
library(gridExtra)  # Required for handling Venn diagram plotting

# Define UI
ui <- fluidPage(
  
  titlePanel("Scopus vs. OpenAlex Venn Diagram"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("scopus_only", "Scopus Only (X):", value = 100, min = 0),
      numericInput("openalex_only", "OpenAlex Only (Y):", value = 80, min = 0),
      numericInput("overlap", "Overlap (Z):", value = 50, min = 0)
    ),
    
    mainPanel(
      plotOutput("vennPlot")
    )
  )
)

# Define Server
server <- function(input, output) {
  output$vennPlot <- renderPlot({
    
    # Set aspect ratio to square to prevent ellipse distortion
    par(pty = "s")  # Enforces a square plotting region
    
    # Generate Venn diagram
    draw.pairwise.venn(
      area1 = input$scopus_only + input$overlap,  # Total Scopus publications
      area2 = input$openalex_only + input$overlap, # Total OpenAlex publications
      cross.area = input$overlap,  # Overlapping publications
      category = c("Scopus", "OpenAlex"),
      fill = c("red", "blue"),
      alpha = 0.5,
      cat.col = c("red", "blue"),
      cat.cex = 1.5,
      cex = 1.5,
      lwd = 2  # Makes the border more visible
    )
  })
}


# Run App
shinyApp(ui, server)

