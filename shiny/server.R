# Define server logic required to draw a map with CCES data
shinyServer(function(input, output) {
        
        source("theme_map.R")
        
        output$select_UI <- renderUI({
                switch(input$year,
                       "2006" = selectInput("var",
                                   "Choose variable to display",
                                    vars, 
                                 selected = vars[1]),
                        "2008" = selectInput("var",
                                "Choose variable to display",
                                vars,
                                selected = vars[1]),
                        "2010" = selectInput("var",
                                "Choose variable to display",
                                 vars,
                                 selected = vars[1]),
                        "2012" = selectInput("var",
                                  "Choose variable to display",
                                  vars,
                                  selected = vars[1])
                )
        })
        
        dataInput <- reactive({
                paste0("map_cces", input$year) %>% sym %>%
                eval
        })
        
       
        
        finalInput <- reactive({
                if(input$var == "Sample count"){
                        finalInput <- dataInput() %>% group_by(FIPScode) %>%
                                mutate(count = n()) %>%
                                select(FIPScode, count, long, lat, group)
                }
                if(input$var == "Do you intend to vote this November?"){
                        finalInput <- dataInput() %>% group_by(FIPScode) %>%
                                mutate(Yes = sum(grepl("Yes", Intend_to_Vote))/n()*100)
                }
                if(input$var == "Did you vote this November?"){
                        finalInput <- dataInput() %>% group_by(FIPScode) %>%
                                mutate(Yes = sum(Voted == "Yes" | grepl("definitely", Voted))/n()*100)
                }
                if(input$var %in% race){
                        finalInput <- dataInput() %>% group_by(FIPScode) %>%
                                mutate(White = sum(Race == "White")/n()*100,
                                       Black = sum(Race == "Black")/n()*100,
                                       Hispanic = sum(Race == "Hispanic")/n()*100,
                                       Asian = sum(Race == "Asian")/n()*100)
                }
                if(input$var %in% educ){
                        finalInput <- dataInput() %>% group_by(FIPScode) %>%
                                mutate(NoHS = sum(Education == educ[1])/n()*100,
                                       HSgrad = sum(Education == educ[2])/n()*100,
                                        College = sum(Education == educ[3])/n()*100)
                }
                if(input$var %in% income){
                        finalInput <- dataInput() %>% group_by(FIPScode) %>%
                                mutate(income1 = sum(Income %in% income[1:3])/n()*100,
                                       income2 = sum(Income %in% income[4:6])/n()*100,
                                       income3 = sum(Income %in% income[7:8])/n()*100,
                                       income4 = sum(Income %in% income[9:10])/n()*100,
                                       income5 = sum(Income %in% income[11:12])/n()*100)
                }
                else{
                        finalInput <- dataInput()
                }
        })
        
        variable <- reactive({
                switch(input$var,
                       "Rainfall (inches)" = "Rainfall12", 
                       "Rainfall (z-score index)" = "Rainfall12.index",
                       "Rainfall (Standard Precipitation Index)" = "Rainfall12.spi", 
                       "Sample count" = "count",
                       "Age" = "Age",
                       "% White" = "White", "% Black" = "Black",
                       "% Hispanic" = "Hispanic", "% Asian" = "Asian",
                       "% No HS" = "NoHS", "% HS graduate" = "HSgrad",
                       "% Some college and above" = "College",
                       "% less than $30k" = "income1", 
                       "% less than 60k" = "income2", 
                       "% less than $80k" = "income3", 
                       "% less than 120k" = "income4", 
                       "% $120k and above" = "income5",
                       "Did you vote this November?" = "Yes",
                       "Do you intend to vote this November?" = "Yes") 
        })
        
        output$plot <- renderPlot({
                p <- ggplot(finalInput(),
                       aes(x = long, y = lat, group = group)) +
                        geom_polygon(aes_string(fill = variable()), size = 1.75) +
                        labs(fill = input$var) + 
                        theme_map + 
                        scale_fill_viridis_c()
                print(p)
        })
        
        output$text <- renderText({
                variable()
        })
})
