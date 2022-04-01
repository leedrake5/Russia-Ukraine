shinyServer(function(input, output, session) {
    
    theData <- reactive({
        daily_frame <- as.data.frame(daily_frame)
        daily_frame$Date <- as.Date(daily_frame$Date, format="%Y-%m-%d", origin="1970-01-01")
        daily_frame
    })
    
    theCountries <- reactive({
        
        unique(daily_frame$country)
        
    })
    
    theClasses <- reactive({
        
        unique(daily_frame$class)
        
    })
    
    theClassesSelected <- reactive({
        
        if(input$useallclasses==TRUE){
            unique(daily_frame$class)
        } else if(input$useallclasses==FALSE){
            "Tanks"
        }
        
        
    })
    
    theSystems <- reactive({
        
        unique(daily_frame[daily_frame$class %in% theClassesSelected(),]$system)
        
    })
    
    theSystemsSelected <- reactive({
        
        all_of_them <- unique(daily_frame[daily_frame$class %in% input$classes,]$system)
        
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
        
        selectInput("classes", "Classes", choices=theClasses(), selected=theClassesSelected(), multiple=TRUE)
        
    })
    
    output$systemsui <- renderUI({
        
        selectInput("systems", "Systems", choices=theSystems(), selected=theSystemsSelected(), multiple=TRUE)
        
    })
    
    dataRawRussia <- reactive({
        
        the_data <- theData()
        the_data <- the_data[the_data$country %in% "Russia",]
        the_outcomes <- input$outcomes
        the_classes <- input$classes
        the_systems <- input$systems
        
        the_data_mod <- the_data[the_data$system %in% the_systems,]
        
        the_data_mod
        
    })
    
    output$russiaunits <- renderDataTable({
        
        table <- dataRawRussia()
        table$url <- paste0("<a href='", table$url, "'target='_blank'>", table$url, "</a>")
        table <- DT::datatable(table, escape=FALSE)
        table
    }, escape=FALSE)
    
    dataModRussia <- reactive({
        
        the_outcomes <- input$outcomes
        the_classes <- input$classes
        the_systems <- input$sytems
        
        the_data_mod <- dataRawRussia()
        
        
        
        
        the_data_tidy <- date_crunch(the_data_mod)
        
        if(length(the_outcomes) < 4){
            
            the_data_tidy <- the_data_tidy %>% dplyr::rowwise() %>%
            dplyr::mutate(total = sum(dplyr::c_across(the_outcomes)))
            
        }
        
        #the_data_list <- split(the_data_tidy, f=the_data_tidy$class)
        
        #data_class_list <- list()
        #for(i in names(the_data_list)){
            
        #    data_class_list[[i]] <- the_data_list[[i]] %>%
        #      mutate(total_count = cumsum(total)) %>%
        #      mutate(cum_rolling10 = rollapplyr(total, width = 10, FUN = sum, partial = TRUE)) %>%
        #      drop_na(total_count)
        #}
        
        #data_class_frame <- rbindlist(data_class_list, use.names=TRUE, fill=TRUE)
        
        data_class_frame <-  the_data_tidy %>%
        mutate(total_count = cumsum(total)) %>%
        mutate(cum_rolling10 = rollapplyr(total, width = 10, FUN = sum, partial = TRUE)) %>%
        drop_na(total_count)
        
        data_class_frame
    })
    
    
    dataRawUkraine <- reactive({
        
        the_data <- theData()
        the_data <- the_data[the_data$country %in% "Ukraine",]
        the_outcomes <- input$outcomes
        the_classes <- input$classes
        the_systems <- input$systems

        the_data_mod <- the_data[the_data$system %in% the_systems,]
        
        the_data_mod
        
    })
    
    output$ukraineunits <- renderDataTable({
        
        table <- dataRawUkraine()
        table$url <- paste0("<a href='", table$url, "'target='_blank'>", table$url, "</a>")
        table <- DT::datatable(table, escape=FALSE)
        table
    }, escape=FALSE)
    
    
    dataModUkraine <- reactive({
        
        the_outcomes <- input$outcomes
        the_classes <- input$classes
        the_systems <- input$sytems
        
        the_data_mod <- dataRawUkraine()
        
        the_data_tidy <- date_crunch(the_data_mod)
        
        if(length(the_outcomes) < 4){
            
            the_data_tidy <- the_data_tidy %>% dplyr::rowwise() %>%
            dplyr::mutate(total = sum(dplyr::c_across(the_outcomes)))
            
        }
        
        #the_data_list <- split(the_data_tidy, f=the_data_tidy$class)
        
        #data_class_list <- list()
        #for(i in names(the_data_list)){
            
        #    data_class_list[[i]] <- the_data_list[[i]] %>%
        #      mutate(total_count = cumsum(total)) %>%
        #      mutate(cum_rolling10 = rollapplyr(total, width = 10, FUN = sum, partial = TRUE)) %>%
        #      drop_na(total_count)
        #}
        
        #data_class_frame <- rbindlist(data_class_list, use.names=TRUE, fill=TRUE)
        
        data_class_frame <-  the_data_tidy %>%
        mutate(total_count = cumsum(total)) %>%
        mutate(cum_rolling10 = rollapplyr(total, width = 10, FUN = sum, partial = TRUE)) %>%
        drop_na(total_count)
        
        data_class_frame
    })
    
    
    dataMod <- reactive({
        
        russia <- dataModRussia()
        ukraine <- dataModUkraine()
        
        data_merged <- as.data.frame(rbindlist(list(russia, ukraine), use.names=TRUE, fill=TRUE))
        
        data_merged
        
    })
    
    bulkPlot <- reactive({
        
        data_class_frame <- dataMod()
        
        
        ggplot(data_class_frame, aes(Date, total_count, colour=country, shape=country)) +
        stat_smooth(method="gam") +
        geom_point() +
        scale_x_date(date_labels = "%m/%d") +
        scale_y_continuous("Equipment Lost") +
        theme_light()
        
    })
    
    output$bulkplot <- renderPlot({
        
        bulkPlot()
        
    })

    
    
    
    
    })
