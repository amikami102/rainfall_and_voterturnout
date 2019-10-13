
# Define UI for application that draws a map with CCES data
ui <- fluidPage(theme = "style.css",
       
        titlePanel("CCES Data Visualization"),
        mainPanel(
                absolutePanel(id = "controls", 
                              class = "panel panel-default", 
                              fixed = TRUE,
                              draggable = TRUE, top = 60, 
                              left = "20px", right = 20, bottom = "auto",
                              width = 330, height = "auto",
                              
                              h3("Create ggplot maps with CCES data."),
                              
                              # Radio buttons to select year 
                              radioButtons("year", label = "Year",
                                           choices = list("2006" = 2006, 
                                                          "2008" = 2008, 
                                                          "2010" = 2010,
                                                          "2012" = 2012), 
                                           selected = 2006),
                             
                              # Variable list may change depending on the year
                              uiOutput('select_UI')
                              
                        ),
                plotOutput('plot', width = "150%", height = "800px")
                #verbatimTextOutput("text")
                )
        
)
