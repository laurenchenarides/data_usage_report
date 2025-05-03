library(shiny)
library(VennDiagram)

ui <- fluidPage(
  plotOutput("venn_plot")
)

server <- function(input, output, session) {
  
  output$venn_plot <- renderPlot({
    draw.pairwise.venn(
      area1 = 505 + 761,   # OpenAlex total (Overlap + Only OpenAlex)
      area2 = 505 + 4207,  # Scopus total (Overlap + Only Scopus)
      cross.area = 505,    # Publications in both
      category = c("OpenAlex", "Scopus"),
      fill = c("lightblue", "pink"),
      alpha = 0.5,
      cat.pos = c(-20, 20),
      cat.dist = 0.05,
      scaled = FALSE
    )
  })
}

shinyApp(ui, server)


library(shiny)
library(ggVennDiagram)

ui <- fluidPage(
  plotOutput("venn_plot")
)

server <- function(input, output, session) {
  
  output$venn_plot <- renderPlot({
    # Simulate sets
    openalex_only <- paste0("OA", 1:761)
    scopus_only <- paste0("S", 1:4207)
    overlap <- paste0("Both", 1:505)
    
    venn_list <- list(
      OpenAlex = c(openalex_only, overlap),
      Scopus = c(scopus_only, overlap)
    )
    
    ggVennDiagram(venn_list, label_alpha = 0) +
      ggplot2::scale_fill_gradient(low = "white", high = "lightblue")
  })
}

shinyApp(ui, server)


library(shiny)
install.packages("eulerr")
library(eulerr)

ui <- fluidPage(
  plotOutput("venn_plot")
)

server <- function(input, output, session) {
  
  output$venn_plot <- renderPlot({
    fit <- euler(c(
      "OpenAlex" = 761 + 505,   # Only OpenAlex + Overlap
      "Scopus" = 4207 + 505,    # Only Scopus + Overlap
      "OpenAlex&Scopus" = 505   # Overlap
    ))
    
    plot(fit,
         fills = list(fill = c("lightblue", "white"), alpha = 0.6),
         labels = list(font = 4),
         quantities = TRUE,
         edges = TRUE
    )
  })
}

shinyApp(ui, server)


library(shiny)
library(eulerr)

ui <- fluidPage(
  plotOutput("venn_plot")
)

server <- function(input, output, session) {
  
  output$venn_plot <- renderPlot({
    
    fit <- euler(c(
      "Scopus" = 4207 + 405 + 263 + 100,       # Only Scopus + adjusted overlaps
      "FullText" = 761 + 405 + 5 + 100,
      "SeedCorpus" = 940 + 263 + 5 + 100,
      "Scopus&FullText" = 405 + 100,            # adjusted Scopus & FullText total
      "Scopus&SeedCorpus" = 263 + 100,           # adjusted Scopus & SeedCorpus total
      "FullText&SeedCorpus" = 5 + 100,           # adjusted FullText & SeedCorpus total
      "Scopus&FullText&SeedCorpus" = 100         # assumed triple overlap
    ))
    
    plot(fit,
         fills = list(fill = c("lightblue", "pink", "lightgreen"), alpha = 0.6),
         labels = list(font = 4),
         quantities = TRUE,
         edges = TRUE
    )
  })
}

shinyApp(ui, server)


## Venn Diagram Markdown Content


# ```{r}
# #| echo: false
# #| include: false
# 
# library(shiny)
# library(eulerr)
# ```
# 
# ```{r, fig.cap="Overlap of Scopus (paywall) and OpenAlex Full Text and Seed Corpus (open access) publications."}
# #| code-fold: true
# #| code-summary: "Show R code used to generate the Venn Diagram"
# 
# ui <- fluidPage(
#   plotOutput("venn_plot")
# )
# 
# server <- function(input, output, session) {
#   
#   output$venn_plot <- renderPlot({
#     
#     fit <- euler(c(
#       "Scopus" = 4207,       # Only Scopus + estimated triple overlap
#       "FullText" = 761,        # OpenAlex Full Text + estimated triple overlap
#       "SeedCorpus" = 940,      # OpenAlex Seed Corpus + estimated triple overlap
#       "Scopus&FullText" = 405,            # Scopus & FullText total
#       "Scopus&SeedCorpus" = 263,           # Scopus & SeedCorpus total
#       "FullText&SeedCorpus" = 5,           # FullText & SeedCorpus total
#       "Scopus&FullText&SeedCorpus" = 100         # assumed triple overlap
#     ))
#     
#     plot(fit,
#          fills = list(fill = c("orange", "lightblue", "lightblue"), alpha = 0.6),
#          edges = TRUE,
#          labels = list(font = 4),
#          quantities = TRUE,
#          lty = c("solid", "dashed", "dashed"),   # Line type: Scopus solid, OpenAlex dotted
#          col = c("black", "black", "black")       # Border color: all black
#     )
#   })
# }
# 
# shinyApp(ui, server)
# ```
# 
# ::: {.callout-note icon=false collapse=true title="How to Read Venn Diagram (click to expand)"}
# | Set Combination                     | Meaning                                              | Count |
#   |--------------------------------------|------------------------------------------------------|-------|
#   | Scopus                               | Only Scopus                               | 4207  |
#   | Full Text                            | Only OpenAlex Full Text                   | 761  |
#   | Seed Corpus                          | Only OpenAlex Seed Corpus                  | 940  |
#   | Scopus ∩ Full Text                   | Publications in Scopus and Full Text (including triple overlap) | 505   |
#   | Scopus ∩ Seed Corpus                 | Publications in Scopus and Seed Corpus (including triple overlap) | 363   |
#   | Full Text ∩ Seed Corpus              | Publications in Full Text and Seed Corpus (including triple overlap) | 105   |
#   | Scopus ∩ Full Text ∩ Seed Corpus     | Publications in all three sources                    | 100   |
#   
# :::
