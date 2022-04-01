shinyServer(function(input, output, session) {
    
    theData <- reactive({
        daily_frame
    })
    
    theCountries <- reactive({
        
        unique(daily_frame$country)
        
    })
    
    theClasses <- reactive({
        
        unique(daily_frame[daily_frame$country %in% theCountries(), ]$class)
        
    })
    
    theClassesSelected <- reactive({
        
        if(input$useallclasses==TRUE){
            unique(daily_frame[daily_frame$country %in% theCountries(), ]$class)
        } else if(input$useallclasses==FALSE){
            "Tanks"
        }
        
        
    })
    
    theSystems <- reactive({
        
        unique(daily_frame[daily_frame$class %in% theClasses(),]$system)
        
    })
    
    theSystemsSelected <- reactive({
        
        all_of_them <- unique(daily_frame[daily_frame$class %in% theClasses(),]$system)
        
        if(input$useallclasses==TRUE){
            all_of_them
        } else if(input$useallclasses==FALSE){
            sort(table(all_of_them), decreasing = TRUE)[1]
        }
        
    })
    

    
    theOutcomes <- reactive({
        
        unique(daily_frame$status)
        
    })
    
    
    output$countriesui <- renderUI({
        
        selectInput("countries", "Countries", choices=theCountries(), selected=theCountries(), multiple=TRUE)
        
    })
    
    output$outcomesui <- renderUI({
        
        selectInput("outcomes", "Status", choices=theOutcomes(), selected=theOutcomes(), multiple=TRUE)
        
    })
    
    output$classesui <- renderUI({
        
        selectInput("classes", "Systems", choices=theClasses(), selected=theClassesSelected(), multiple=TRUE)
        
    })
    
    output$systemsui <- renderUI({
        
        selectInput("systems", "Systems", choices=theSystems(), selected=theSystemsSelected(), multiple=TRUE)
        
    })
    
    
    dataMod <- reactive({
        
        the_data <- theData()
        the_countries <- input$countries
        the_outcomes <- input$outcomes
        the_classes <- input$classes
        the_systems <- input$sytems
        
        the_data_mod <- the_data[the_data$country %in% the_countries & the_data$class %in% the_classes & the_data$system %in% the_systems,]
        
        the_data_tidy <- total_by_system_wide(the_data_mod)
        
        if(length(the_outcomes) < 4){
            
            the_data_tidy <- the_data_tidy %>% dplyr::rowwise() %>%
            dplyr::mutate(total = sum(dplyr::c_across(the_outcomes)))
            
        }
        
        the_data_list <- split(the_data_tidy, f=the_data_tidy$class)
        
    })
    
    bulkPlot <- reactive({
        
        
        
        ggplot(the_data_mod, aes(Date, total)
        
        
    })
    

    
    
    
    
    })
