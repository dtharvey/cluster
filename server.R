

shinyServer(function(input, output, session){
  
  current_img = reactiveVal(1)
  
  observeEvent(input$img_click,{
    idx = (current_img() %% 4) + 1
    shinyjs::hide(paste0("img",current_img()))
    shinyjs::show(paste0("img",idx))
    current_img(idx)
  })
  
  # Reactive that recomputes based on the selected method
  dist_matrix_reactive <- reactive({
    dist_result <- dist(original_data, method = input$dist_matrix)
    hc_result   <- hclust(dist_result, method = "single")
    sim_matrix(hc_result, dist_result, step = 0)
  })
  
  output$origdata = renderTable(
    {
      t(original_data)
    },
    striped = TRUE,
    rownames = TRUE,
    colnames = TRUE
  )
  
output$dist_matrix = renderTable(
  {
    input$dist_matrix
    round(as.matrix(dist_matrix_reactive()), digits = 3)
  },
  striped = TRUE,
  rownames = TRUE,
  colnames = TRUE
)

hc_result = reactive({
  d = dist(original_data, method = input$dist_method)
  hclust(d, method = input$agglomeration)
})

output$cluster_steps = renderPrint({
  parse_hclust_steps(hc_result())

})
  
output$linkage = renderPlot({
  link_d = dist(original_data, method = input$link_dist)
  link_hc = hclust(link_d, method = input$link_app)
  plot(link_hc, hang = -1, main = "Cluster Dendrogram", 
       sub = "", xlab = "", lwd = 3, cex = 1.25, cex.axis = 1.25,
       cex.lab = 1.25, cex.main = 1.5)
  old.par = par(lwd = 3, lty = 2)
  rect.hclust(link_hc, k = input$num_clust, border = okabe_ito[4])
  par(old.par)
  grid(col = okabe_ito[1])
})

})
