shinyServer(function(input, output, session) {
    
    
    
    output$russia_strength_modifier_ui <- renderUI({
        if(input$russia_strength_modifier_type=="Absolute"){
            sliderInput("russia_strength_modifier", "Russian Absolute Strength Modifier", min=2, max=150, value=30, step=1)
        } else if(input$russia_strength_modifier_type=="Relative"){
            sliderInput("russia_strength_modifier", "Russian Relative Strength Modifier", min=0.01, max=1, value=0.33, step=0.01)
        }
    })
    
    output$ukraine_strength_modifier_ui <- renderUI({
        if(input$ukraine_strength_modifier_type=="Absolute"){
            sliderInput("ukraine_strength_modifier", "Ukrainian Absolute Strength Modifier", min=2, max=50, value=20, step=1)
        } else if(input$ukraine_strength_modifier_type=="Relative"){
            sliderInput("ukraine_strength_modifier", "Ukrainian Relative Strength Modifier", min=0.01, max=1, value=0.33, step=0.01)
        }
    })
    
    dupoyReSampler <- reactive({
        
        my.cores <- parallel::detectCores()
        
        pbapply::pblapply(1:input$iterations, function(x)
            dupuySample(seed=x, strength_ru=input$russia_strength, strength_ukr=input$ukraine_strength, ru_strength_modifier=input$russia_strength_modifier, ru_strength_lock=!input$russian_reinforcements, ukr_strength_modifier=input$ukraine_strength_modifier, ukr_strength_lock=!input$ukraine_reinforcements, terrain_ru=input$russia_terrain, terrain_ukr=input$ukraine_terrain, ru_terrain_modifier=input$russia_terrain_modifier, ukr_terrain_modifier=input$ukraine_terrain_modifier, season_ru=input$russia_season, season_ukr=input$ukraine_season, ru_season_modifier=input$russia_season_modifier, ukr_season_modifier=input$ukraine_season_modifier, posture_ru=input$russia_posture, posture_ukr=input$ukraine_posture, ru_posture_modifier=input$russia_posture_modifier, ukr_posture_modifier=input$ukraine_posture_modifier, air_ru=input$russia_air, air_ukr=input$ukraine_air, ru_air_modifier=input$russia_air_modifier, ukr_air_modifier=input$ukraine_air_modifier, morale_ru=input$russia_morale, morale_ukr=input$ukraine_morale, ru_morale_modifier=input$russia_morale_modifier, ukr_morale_modifier=input$ukraine_morale_modifier), cl=1)
    })
    
    simulation_results <- reactiveValues()
    
    observeEvent(input$run, {
        
        simulation_results$simulation <<- dupoyReSampler()
        
    })
    
    simulationOutcomes <- reactive({
        
        sapply(simulation_results$simulation, function(x) x[[1]])
        
    })
    
    dupuyDensityPlot <- reactive({
        
        just_outcomes <- simulationOutcomes()
        
        ggplot() +
        geom_vline(xintercept=1, lty=2) +
        geom_density(aes(x=just_outcomes), fill="grey80", alpha=0.5) +
        scale_x_continuous("Dupuy's Ratio", limits=c(0, 5)) +
        theme_light()
        
    })
    
    output$outcome_densities <- renderPlot({
        
        dupuyDensityPlot()
        
    })
    
    output$downloadDensity <- downloadHandler(
    filename = function() { "Dupoy_Density_Plot.jpg" },
    content = function(file) {
        ggsave(file,dupuyDensityPlot(),  device="jpg",  dpi=300)
    }
    )
    
    
    outcomeTable <- reactive({
        
        data.frame(Outcome=round(sapply(simulation_results$simulation, function(x) x[["Outcome"]]), 1),
        Russian_Outcome=round(sapply(simulation_results$simulation, function(x) x[["Russian_Outcome"]]), 1),
        Ukranian_Outcome=round(sapply(simulation_results$simulation, function(x) x[["Ukranian_Outcome"]]), 1),
        Russian_Strength=round(sapply(simulation_results$simulation, function(x) x[["Russian_Strength"]]), 1),
        Russian_Terrain=round(sapply(simulation_results$simulation, function(x) x[["Russian_Terrain"]]), 1),
        Russian_Posture=round(sapply(simulation_results$simulation, function(x) x[["Russian_Posture"]]), 1),
        Russian_Air=round(sapply(simulation_results$simulation, function(x) x[["Russian_Air"]]), 1),
        Russian_Morale=round(sapply(simulation_results$simulation, function(x) x[["Russian_Morale"]]), 1),
        Ukrainian_Strength=round(sapply(simulation_results$simulation, function(x) x[["Ukrainian_Strength"]]), 1),
        Ukrainian_Terrain=round(sapply(simulation_results$simulation, function(x) x[["Ukrainian_Terrain"]]), 1),
        Ukrainian_Posture=round(sapply(simulation_results$simulation, function(x) x[["Ukrainian_Posture"]]), 1),
        Ukrainian_Air=round(sapply(simulation_results$simulation, function(x) x[["Ukrainian_Air"]]), 1),
        Ukrainian_Morale=round(sapply(simulation_results$simulation, function(x) x[["Ukrainian_Morale"]]), 1))
        
    })
    
    output$outcome_table <- renderDataTable({
        
        outcomeTable()
        
    })
    
    output$downloadOutcomes <- downloadHandler(
    filename = function() { "Dupoy_Outcomes.csv" },
    content = function(file) {
        write.csv(outcomeTable(), file)
    }
    )
    
    
    
    
    theData <- reactive({
        daily_frame <- as.data.frame(daily_frame)
        daily_frame$Date <- as.Date(daily_frame$Date, format="%Y-%m-%d", origin="1970-01-01")
        daily_frame
    })
    
    theFullData <- reactive({
        full_data <- as.data.frame(full_data)
        full_data$Date <- as.Date(full_data$Date, format="%Y-%m-%d", origin="1970-01-01")
        full_data
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
        
        selected <- input$classes
        
        all_of_them <- unique(daily_frame[daily_frame$class %in% selected ,]$system)
        
        if(input$useallsystems==TRUE){
            all_of_them
        } else if(input$useallsystems==FALSE){
            names(sort(table(all_of_them), decreasing = TRUE)[1])
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
    
    dataFullRussia <- reactive({
        
        the_data <- dataRawRussia()
        the_data <- the_data[the_data$country %in% "Russia",]
        the_outcomes <- input$outcomes
        the_classes <- input$classes
        the_systems <- input$systems
        
        the_data_mod <- the_data[the_data$system %in% the_systems,]
        
        the_data_mod
        
    })
    
    dataModRussia <- reactive({
        
        the_outcomes <- input$outcomes
        the_classes <- input$classes
        the_systems <- input$sytems
        
        the_data_mod <- dataFullRussia()
        the_data_mod <- the_data_mod[order(the_data_mod$Date), ]
        
        the_data_tidy <- date_crunch(the_data_mod)
        the_data_tidy <- the_data_tidy[order(the_data_tidy$Date), ]

        if(length(the_outcomes) < 4){
            
            the_data_tidy <- the_data_tidy %>% dplyr::rowwise() %>%
            dplyr::mutate(total = sum(dplyr::c_across(the_outcomes)))
            
        }
        the_data_tidy <- the_data_tidy[order(the_data_tidy$Date), ]

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
        data_class_frame <- data_class_frame[order(data_class_frame$Date), ]

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
    
    dataFullUkraine <- reactive({
        
        the_data <- theFullData()
        the_data <- the_data[the_data$country %in% "Ukraine",]
        the_outcomes <- input$outcomes
        the_classes <- input$classes
        the_systems <- input$systems

        the_data_mod <- the_data[the_data$system %in% the_systems,]
        
        the_data_mod
        
    })
    
    dataModUkraine <- reactive({
        
        the_outcomes <- input$outcomes
        the_classes <- input$classes
        the_systems <- input$sytems
        
        the_data_mod <- dataRawUkraine()
        the_data_mod <- the_data_mod[order(the_data_mod$Date), ]

        the_data_tidy <- date_crunch(the_data_mod)
        the_data_tidy <- the_data_tidy[order(the_data_tidy$Date), ]

        if(length(the_outcomes) < 4){
            
            the_data_tidy <- the_data_tidy %>% dplyr::rowwise() %>%
            dplyr::mutate(total = sum(dplyr::c_across(the_outcomes)))
            
        }
        the_data_tidy <- the_data_tidy[order(the_data_tidy$Date), ]

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
        data_class_frame <- data_class_frame[order(data_class_frame$Date), ]

        data_class_frame
    })
    
    
    dataMod <- reactive({
        
        russia <- dataModRussia()
        ukraine <- dataModUkraine()
        
        data_merged <- as.data.frame(rbindlist(list(russia, ukraine), use.names=TRUE, fill=TRUE))
        
        data_merged <- data_merged[order(data_merged$Date), ]
        
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
