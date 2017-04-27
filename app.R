library(shiny)
library(rmarkdown)

shslines <- c("Increased student return rates by 50%",
              "Reached out to troubled populations",
              "Stepped in to ensure student well-being")

dblmtlines <- c("Employee of the month - March 2002",
                "Promoted to grill after one day",
                "Closed with 100% drawer accuracy")

watcherslines <- c("Died twice",
                   "Earned inaugural Class Protector award",
                   "Saved the world. A lot.")

skills <- c("Microsoft Office",
            "Research",
            "Mentoring",
            "Bullying prevention")


ui <- fluidPage(
   
   titlePanel("Shiny Resume Builder"),
   
   sidebarLayout(
      sidebarPanel(
        checkboxInput("incl_address", "Include contact info", value = TRUE),
        checkboxGroupInput("employers", "Choose employers to include:", 
                           choices = c("Doublemeat Palace", "Sunnydale High School", "Watchers Council"),
                           selected = c("Doublemeat Palace", "Sunnydale High School", "Watchers Council")),
        uiOutput("choose_emp"),
        uiOutput("choose_emp2"),
        uiOutput("choose_emp3"),
        selectizeInput("skills", "Choose skills:", choices = skills,
                       multiple = TRUE, options = list(plugins = list('drag_drop'))),
        checkboxInput("incl_orgs", "Include organizations", value = TRUE),
        radioButtons("format", "Output format:", 
                     choices = c("HTML" = "html_document",
                                 "PDF" = "pdf_document",
                                 "Word" = "word_document")),
        actionButton("goknit", "I am the plan")
        
      ),
      
      mainPanel(
         uiOutput("buttonappear"),
         tableOutput("preview"),
         tableOutput("skills")
      )
   )
)


server <- function(input, output) {
  
  reportdone <- eventReactive(input$goknit, {
        render("bsummers_resume.Rmd",
             output_format = isolate(input$format),
             params = list(
               shs_strings = isolate(input$shs),
               dblmt_strings = isolate(input$dblmt),
               watcher_strings = isolate(input$wc),
               incl_address = isolate(input$incl_address),
               incl_orgs = isolate(input$incl_orgs),
               skills = isolate(input$skills)))
  })
  
  output$buttonappear <- renderUI({
    reportdone()
    downloadButton("knitdoc", "It's ready!")
  })
  
  output$knitdoc <- downloadHandler(
    filename = function(){
      ext <- switch(isolate(input$format),
                    "html_document" = ".html",
                    "pdf_document" = ".pdf",
                    "word_document" = ".docx")
      paste0("bsummers_resume", ext)
    },
    content = function(file){
      file.copy(reportdone(), file, overwrite = TRUE)
    }
  )
   
   output$preview <- renderTable({
     if (length(input$employers) > 0){
       rlist <- NULL
       if (!(is.null(input$shs))){
         rlist <- c(rlist, c("SUNNYDALE HIGH", input$shs))
       }
       if (!(is.null(input$dblmt))){
         rlist <- c(rlist, c("DOUBLEMEAT PALACE", input$dblmt))
       }
       if (!(is.null(input$wc))){
         rlist <- c(rlist, c("WATCHERS COUNCIL", input$wc))
       }
       data.frame("Employers" = rlist)
     }
   })
   
   output$skills <- renderTable({
     data.frame("Skills" = input$skills)
   })
   
   output$choose_emp <- renderUI({
     if ("Sunnydale High School" %in% input$employers){
       selectizeInput("shs", "Choose accomplishments for Sunnydale High School:", choices = shslines,
                      multiple = TRUE, options = list(plugins = list('drag_drop')))
     }
   })
   
   output$choose_emp2 <- renderUI({
     if ("Doublemeat Palace" %in% input$employers){
       selectizeInput("dblmt", "Choose accomplishments for Doublemeat Palace:", choices = dblmtlines,
                      multiple = TRUE, options = list(plugins = list('drag_drop')))
     }
   })
   
   output$choose_emp3 <- renderUI({
     if ("Watchers Council" %in% input$employers){
       selectizeInput("wc", "Choose accomplishments for Watchers Council:", choices = watcherslines,
                      multiple = TRUE, options = list(plugins = list('drag_drop')))
     }
   })
   
   output$test <- renderText(
     input$format
   )
}

# Run the application 
shinyApp(ui = ui, server = server)

