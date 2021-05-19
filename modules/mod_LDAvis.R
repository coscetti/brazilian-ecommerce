library("LDAvis")
load("rblogs.RData")
# Module UI ---------------------------------------------------------------
LDAvis_t_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(visOutput(ns("lda_vis")))
  
}





# Module logic ------------------------------------------------------------

LDAvis_t <- function(input, output, session) {
  ns <- session$ns
  
  HTML_TEMPLATE_COLUMN <- '<div class="col-sm-4"><div class="widget"><h5 class="widget-title font-alt">%s</h5><ul>%s</ul></div></div>\n'
  
  output$lda_vis <- renderVis({
    with(rblogTexts, 
         createJSON(phi = phi, 
                    theta = theta, 
                    doc.length = doc.length, 
                    vocab = vocab, 
                    term.frequency = term.frequency))
  })
}