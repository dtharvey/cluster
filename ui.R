ui = navbarPage("AC 3.0: Cluster Analysis",
                theme = shinytheme("journal"),
                header = tags$head(
                  tags$link(rel = "stylesheet",
                            type = "text/css",
                            href = "style.css") 
                ),
                
# introduction
  tabPanel("Introduction",
      fluidRow(
        withMathJax(),
        useShinyjs(),
        column(width = 6, 
          wellPanel(
            class = "scrollable-well",
              div(
                class = "html-fragment",
                  includeHTML("text/introduction.html")
                ))),
         column(width = 6,
             align = "center",
             tags$div(
               id = "image_container",
               tags$img(
                 id = "img1",
                 src = "intro_new_figure1.png",
                 style = "cursor:pointer;",
                 height = "600px",
                 onclick = "Shiny.setInputValue('img_click',Math.random())"
               ),
               tags$img(
                 id = "img2",
                 src = "intro_new_figure2.png",
                 style = "display: none; cursor: pointer;",
                 height = "600px",
                 onclick = "Shiny.setInputValue('img_click',Math.random())"
               ),
               tags$img(
                 id = "img3",
                 src = "intro_new_figure3.png",
                 style = "display: none; cursor: pointer;",
                 height = "600px",
                 onclick = "Shiny.setInputValue('img_click',Math.random())"
               ),
               tags$img(
                 id = "img4",
                 src = "intro_new_figure4.png",
                 style = "display: none; cursor: pointer;",
                 height = "600px",
                 onclick = "Shiny.setInputValue('img_click',Math.random())"
               ),
               # tags$img(
               #   id = "img5",
               #   src = "intro5.png",
               #   style = "display: none; cursor: pointer;",
               #   height = "600px",
               #   onclick = "Shiny.setInputValue('img_click',Math.random())"
               # ),
               # tags$img(
               #   id = "img6",
               #   src = "intro6.png",
               #   style = "display: none; cursor: pointer;",
               #   height = "600px",
               #   onclick = "Shiny.setInputValue('img_click',Math.random())"
               # ),
               # tags$img(
               #   id = "img7",
               #   src = "intro7.png",
               #   style = "display: none; cursor: pointer;",
               #   height = "600px",
               #   onclick = "Shiny.setInputValue('img_click',Math.random())"
               # ),
             )
              ))),
                
# first activity
    tabPanel("Similarity",
              fluidRow(
                column(width = 6,
                      wellPanel(
                        class = "scrollable-well",
                        div(
                          class = "html-fragment",
                          includeHTML("text/activity1.html")
                        ))),
                column(
                  width = 6,
                  align = "center",
                  h4("original data"),
                  tableOutput("origdata"),
                  h4("similarity matrix"),
                  radioButtons(inputId = "dist_matrix",
                               label = "distance method",
                               choices = c("Euclidean" = "euclidean", 
                                           "Manhattan" = "manhattan"),
                               inline = TRUE),
                  img(src = "distances.png", height = "125px"),
                  tableOutput("dist_matrix")
                ))),

# second activity
tabPanel("Agglomeration",
         fluidRow(
           column(width = 6,
                  wellPanel(
                    class = "scrollable-well",
                    div(
                      class = "html-fragment",
                      includeHTML("text/activity2.html")
                    ))),
           column(
             width = 6,
             align = "center",
             img(src = "similarity.png", height = "175px"),
             br(),
             br(),
             splitLayout(
               cellWidths = c("50%", "50%"),
             radioButtons(inputId = "agglomeration",
                          label = "agglomeration method",
                          choices = c("single","complete",
                                      "average", "centroid"),
                          inline = TRUE),
             radioButtons(inputId = "dist_method",
                          label = "distance method",
                          choices = c("Euclidean" = "euclidean", 
                                      "Manhattan" = "manhattan"),
                          inline = TRUE),
           ),
           verbatimTextOutput("cluster_steps")
           ))),   

# third activity

tabPanel("Linkage",
         fluidPage(
           column(width = 6,
                  wellPanel(
                    class = "scrollable-well",
                    div(
                    class = "html-fragment",
                    includeHTML("text/activity3.html")
                  ))),
           column(width = 6,
                  align = "center",
                  splitLayout(
                    cellWidths = c("60%", "40%"),
                    radioButtons(inputId = "link_app",
                                 label = "agglomeration method",
                                 choices = c("single","complete",
                                             "average", "centroid"),
                                 inline = TRUE),
                    radioButtons(inputId = "link_dist",
                                 label = "distance method",
                                 choices = c("Euclidean" = "euclidean", 
                                             "Manhattan" = "manhattan"),
                                 inline = TRUE),
                  ),
                  splitLayout(
                    sliderInput(inputId = "num_clust",
                               label = "number of clusters",
                               min = 2, max = 5, value = 2 
                               )),
                  plotOutput("linkage", height = "500px")
                  ))),
             
# wrapping up
                
tabPanel("Wrapping Up",
          fluidPage(
            column(width = 6,
                  wellPanel(
                    class = "scrollable-well",
                    div(
                      class = "html-fragment",
                      includeHTML("text/wrapup.html")
                    ))),
           column(width = 6,
                  align = "center",
                    img(src = "metals.png", height = "300px"),
                    img(src = "spectra.png", height = "300px")
                           )))            
                
) # closing for user interface
