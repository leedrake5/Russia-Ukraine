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
        
        
        result <- list()
        withProgress(
              message='Please wait',
              detail='Runing...',
              value=0, {

                for (i in 1:input$iterations) {
                    
                    result[[i]] <- dupuySample(seed=i, strength_ru=input$russia_strength, strength_ukr=input$ukraine_strength, ru_strength_modifier=input$russia_strength_modifier, ru_strength_lock=!input$russian_reinforcements, ukr_strength_modifier=input$ukraine_strength_modifier, ukr_strength_lock=!input$ukraine_reinforcements, terrain_ru=input$russia_terrain, terrain_ukr=input$ukraine_terrain, ru_terrain_modifier=input$russia_terrain_modifier, ukr_terrain_modifier=input$ukraine_terrain_modifier, season_ru=input$russia_season, season_ukr=input$ukraine_season, ru_season_modifier=input$russia_season_modifier, ukr_season_modifier=input$ukraine_season_modifier, posture_ru=input$russia_posture, posture_ukr=input$ukraine_posture, ru_posture_modifier=input$russia_posture_modifier, ukr_posture_modifier=input$ukraine_posture_modifier, air_ru=input$russia_air, air_ukr=input$ukraine_air, ru_air_modifier=input$russia_air_modifier, ukr_air_modifier=input$ukraine_air_modifier, morale_ru=input$russia_morale, morale_ukr=input$ukraine_morale, ru_morale_modifier=input$russia_morale_modifier, ukr_morale_modifier=input$ukraine_morale_modifier)
                    incProgress(1/input$iterations, detail = paste("Resample ", i))
                }
              })
              
              result
    })
    
    simulation_results <- reactiveValues()
    
    observeEvent(input$run, {
        
        simulation_results$simulation <<- dupoyReSampler()
        
    })
    
    simulationOutcomes <- reactive({
        
        sapply(simulation_results$simulation, function(x) x[["Outcome"]])
        
    })
    
    simulationRussianStrength <- reactive({
        
        sapply(simulation_results$simulation, function(x) x[["Russian_Outcome"]])
        
    })
    
    simulationUkrainianStrength <- reactive({
        
        sapply(simulation_results$simulation, function(x) x[["Ukranian_Outcome"]])
        
    })
    
    russianPositiveOutcomes <- reactive({
        
        length(simulationOutcomes()[simulationOutcomes()>1])
        
    })
    
    ukrainianPositiveOutcomes <- reactive({
        
        length(simulationOutcomes()[simulationOutcomes()<=1])
        
    })
    
    output$simulationresultsui <- renderUI({
        
        renderText(paste0("Russian Breakthroughs: ", format(russianPositiveOutcomes(),big.mark=",",scientific=FALSE), ", Ukranian Holds: ", format(ukrainianPositiveOutcomes(),big.mark=",",scientific=FALSE)))
        
    })
    
    output$simulationresults2ui <- renderUI({
        
        renderText(paste0("Russian Breakthroughs: ", format(russianPositiveOutcomes(),big.mark=",",scientific=FALSE), ", Ukranian Holds: ", format(ukrainianPositiveOutcomes(),big.mark=",",scientific=FALSE)))
        
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
    
    
    dupuyStrengthPlot <- reactive({
        
        just_strength <- data.frame(Strength=c(simulationRussianStrength(), simulationUkrainianStrength()), Country=c(rep("Russia", input$iterations), rep("Ukraine", input$iterations)))
        
        
        
        ggplot() +
        geom_density(data=just_strength, mapping=aes(x=Strength, colour=Country, fill=Country), alpha=0.5) +
        scale_x_continuous("Dupuy Strength") +
        theme_light()
        
    })
    
    output$outcome_strength <- renderPlot({
        
        dupuyStrengthPlot()
        
    })
    
    output$downloadStrength <- downloadHandler(
    filename = function() { "Dupoy_Density_Strength_Plot.jpg" },
    content = function(file) {
        ggsave(file,dupuyStrengthPlot(),  device="jpg",  dpi=300)
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
        daily_frame$Date <- as.Date(daily_frame$Date, format="%m/%d/%Y", origin="1970-01-01")
        daily_frame
    })
    
    theFullData <- reactive({
        full_data <- as.data.frame(full_data)
        full_data$Date <- as.Date(full_data$Date, format="%m/%d/%Y", origin="1970-01-01")
        full_data
    })
    
    theCountries <- reactive({
        
        unique(daily_frame$origin)
        
    })
    
    theClasses <- reactive({
        
        unique(daily_frame$class)
        
    })
    
    theClassesSelected <- reactive({
        
        #if(input$useallclasses==TRUE){
            unique(daily_frame$class)
        #} else if(input$useallclasses==FALSE){
            #"Tanks"
        #}
        
        
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
        
        unique(daily_frame$status)[order(unique(daily_frame$status))]
        
    })
    
    
    output$countriesui <- renderUI({
        
        selectInput("countries", "Countries", choices=theCountries(), selected=theCountries(), multiple=TRUE)
        
    })
    
    output$outcomesui <- renderUI({
        
        selectInput("outcomes", "Status", choices=theOutcomes(), selected=theOutcomes(), multiple=TRUE)
        
    })
    

    
    Classes <- reactive({
        
        classes <- unique(theData()$class)
        classes <- classes[!classes %in% c(NA, "NA", "Unknown", "Radars")]
        
        classes[order(classes)]
        
        
    })
    
    classesToUse <- reactive({
        
        if(input$dropall){
            NULL
        } else {
            Classes()
        }
        
    })
    
    output$classesui <- renderUI({
        
        selectInput("classes", "Equipment Types", choices=Classes(), selected=classesToUse(), multiple=TRUE)
        
    })
    
    tanksSelection <- reactive({
        
        if("Tanks" %in% input$classes){
            unique(theData()[theData()$class %in% "Tanks", "system"])
        } else {
            NULL
        }
        
    })
    
    output$tanksui <- renderUI({
        
        selectInput("tanks", "Tanks", choices=unique(theData()[theData()$class %in% "Tanks", "system"]), selected=tanksSelection(), multiple=TRUE)
        
    })
    
    afvSelection <- reactive({
        
        if("Armoured Fighting Vehicles" %in% input$classes){
            unique(theData()[theData()$class %in% "Armoured Fighting Vehicles", "system"])
        } else {
            NULL
        }
        
    })
    
    output$afvui <- renderUI({
        
        selectInput("afv", "Armoured Fighting Vehicles", choices=unique(theData()[theData()$class %in% "Armoured Fighting Vehicles", "system"]), selected=afvSelection(), multiple=TRUE)
        
    })
    
    apcSelection <- reactive({
        
        if("Armoured Personnel Carriers" %in% input$classes){
            unique(theData()[theData()$class %in% "Armoured Personnel Carriers", "system"])
        } else {
            NULL
        }
        
    })
    
    output$apcui <- renderUI({
        
        selectInput("apc", "Armoured Personnel Carriers", choices=unique(theData()[theData()$class %in% "Armoured Personnel Carriers", "system"]), selected=apcSelection(), multiple=TRUE)
        
    })
    
    ifvSelection <- reactive({
        
        if("Infantry Fighting Vehicles" %in% input$classes){
            unique(theData()[theData()$class %in% "Infantry Fighting Vehicles", "system"])
        } else {
            NULL
        }
        
    })
    
    output$ifvui <- renderUI({
        
        selectInput("ifv", "Infantry Fighting Vehicles", choices=unique(theData()[theData()$class %in% "Infantry Fighting Vehicles", "system"]), selected=ifvSelection(), multiple=TRUE)
        
    })
    
    imvSelection <- reactive({
        
        if("Infantry Mobility Vehicles" %in% input$classes){
            unique(theData()[theData()$class %in% "Infantry Mobility Vehicles", "system"])
        } else {
            NULL
        }
        
    })
    
    output$imvui <- renderUI({
        
        selectInput("ifm", "Infantry Mobility Vehicles", choices=unique(theData()[theData()$class %in% "Infantry Mobility Vehicles", "system"]), selected=imvSelection(), multiple=TRUE)
        
    })
    
    spatmsSelection <- reactive({
        
        if("Self-Propelled Anti-Tank Missile Systems" %in% input$classes){
            unique(theData()[theData()$class %in% "Self-Propelled Anti-Tank Missile Systems", "system"])
        } else {
            NULL
        }
        
    })
    
    
    output$spatmsui <- renderUI({
        
        selectInput("spatms", "Self-Propelled Anti-Tank Missile Systems", choices=unique(theData()[theData()$class %in% "Self-Propelled Anti-Tank Missile Systems", "system"]), selected=spatmsSelection(), multiple=TRUE)
        
    })
    
    aircraftSelection <- reactive({
        
        if("Aircraft" %in% input$classes){
            unique(theData()[theData()$class %in% "Aircraft", "system"])
        } else {
            NULL
        }
        
    })
    
    output$aircraftui <- renderUI({
        
        selectInput("aircraft", "Aircraft", choices=unique(theData()[theData()$class %in% "Aircraft", "system"]), selected=aircraftSelection(), multiple=TRUE)
        
    })
    
    helicoptersSelection <- reactive({
        
        if("Helicopters" %in% input$classes){
            unique(theData()[theData()$class %in% "Helicopters", "system"])
        } else {
            NULL
        }
        
    })
    
    output$helicoptersui <- renderUI({
        
        selectInput("helicopters", "Helicopters", choices=unique(theData()[theData()$class %in% "Helicopters", "system"]), selected=helicoptersSelection(), multiple=TRUE)
        
    })
    
    aagSelection <- reactive({
        
        if("Anti-Aircraft Guns" %in% input$classes){
            unique(theData()[theData()$class %in% "Anti-Aircraft Guns", "system"])
        } else {
            NULL
        }
        
    })
    
    output$aagui <- renderUI({
        
        selectInput("aag", "Anti-Aircraft Guns", choices=unique(theData()[theData()$class %in% "Anti-Aircraft Guns", "system"]), selected=aagSelection(), multiple=TRUE)
        
    })
    
    spaagSelection <- reactive({
        
        if("Self-Propelled Anti-Aircraft Guns" %in% input$classes){
            unique(theData()[theData()$class %in% "Self-Propelled Anti-Aircraft Guns", "system"])
        } else {
            NULL
        }
        
    })
    
    output$spaagui <- renderUI({
        
        selectInput("spaag", "Self-Propelled Anti-Aircraft Guns", choices=unique(theData()[theData()$class %in% "Self-Propelled Anti-Aircraft Guns", "system"]), selected=spaagSelection(), multiple=TRUE)
        
    })
    
    stamsSelection <- reactive({
        
        if("Surface-To-Air Missile Systems" %in% input$classes){
            unique(theData()[theData()$class %in% "Surface-To-Air Missile Systems", "system"])
        } else {
            NULL
        }
        
    })
    
    output$stamsui <- renderUI({
        
        selectInput("stams", "Surface-To-Air Missile Systems", choices=unique(theData()[theData()$class %in% "Surface-To-Air Missile Systems", "system"]), selected=stamsSelection(), multiple=TRUE)
        
    })
    

    uavSelection <- reactive({
        
        if("Unmanned Aerial Vehicles" %in% input$classes){
            unique(theData()[theData()$class %in% "Unmanned Aerial Vehicles", "system"])
        } else {
            NULL
        }
        
    })
    
    output$uavui <- renderUI({
        
        selectInput("uav", "Unmanned Aerial Vehicles", choices=unique(theData()[theData()$class %in% "Unmanned Aerial Vehicles", "system"]), selected=uavSelection(), multiple=TRUE)
        
    })
    
    ucavSelection <- reactive({
        
        if("Unmanned Combat Aerial Vehicles" %in% input$classes){
            unique(theData()[theData()$class %in% "Unmanned Combat Aerial Vehicles", "system"])
        } else {
            NULL
        }
        
    })
    
    output$ucavui <- renderUI({
        
        selectInput("ucav", "Unmanned Combat Aerial Vehicles", choices=unique(theData()[theData()$class %in% "Unmanned Combat Aerial Vehicles", "system"]), selected=ucavSelection(), multiple=TRUE)
        
    })
    
    ruavSelection <- reactive({
        
        if("Reconnaissance Unmanned Aerial Vehicles" %in% input$classes){
            unique(theData()[theData()$class %in% "Reconnaissance Unmanned Aerial Vehicles", "system"])
        } else {
            NULL
        }
        
    })
    
    output$ruavui <- renderUI({
        
        selectInput("ruav", "Reconnaissance Unmanned Aerial Vehicles", choices=unique(theData()[theData()$class %in% "Reconnaissance Unmanned Aerial Vehicles", "system"]), selected=ruavSelection(), multiple=TRUE)
        
    })
    
    jammersSelection <- reactive({
        
        if("Jammers And Deception Systems" %in% input$classes){
            unique(theData()[theData()$class %in% "Jammers And Deception Systems", "system"])
        } else {
            NULL
        }
        
    })
    
    output$jammersui <- renderUI({
        
        selectInput("jammers", "Jammers And Deception Systems", choices=unique(theData()[theData()$class %in% "Jammers And Deception Systems", "system"]), selected=jammersSelection(), multiple=TRUE)
        
    })
    
    radarsSelection <- reactive({
        
        if("Radars And Communications Equipment" %in% input$classes){
            unique(theData()[theData()$class %in% c("Radars", "Radars And Communications Equipment"), "system"])
        } else {
            NULL
        }
        
    })
    
    output$radarsui <- renderUI({
        
        selectInput("radars", "Radars And Communications Equipment", choices=unique(theData()[theData()$class %in% c("Radars", "Radars And Communications Equipment"), "system"]), selected=radarsSelection(), multiple=TRUE)
        
    })
    
    taSelection <- reactive({
        
        if("Towed Artillery" %in% input$classes){
            unique(theData()[theData()$class %in% "Towed Artillery", "system"])
        } else {
            NULL
        }
        
    })
    
    
    output$taui <- renderUI({
        
        selectInput("ta", "Towed Artillery", choices=unique(theData()[theData()$class %in% "Towed Artillery", "system"]), selected=taSelection(), multiple=TRUE)
        
    })
    
    spaSelection <- reactive({
        
        if("Self-Propelled Artillery" %in% input$classes){
            unique(theData()[theData()$class %in% "Self-Propelled Artillery", "system"])
        } else {
            NULL
        }
        
    })
    
    output$spaui <- renderUI({
        
        selectInput("spa", "Self-Propelled Artillery", choices=unique(theData()[theData()$class %in% "Self-Propelled Artillery", "system"]), selected=spaSelection(), multiple=TRUE)
        
    })
    
    asveSelection <- reactive({
        
        if("Artillery Support Vehicles And Equipment" %in% input$classes){
            unique(theData()[theData()$class %in% "Artillery Support Vehicles And Equipment", "system"])
        } else {
            NULL
        }
        
    })
    
    output$asveui <- renderUI({
        
        selectInput("asve", "Artillery Support Vehicles And Equipment", choices=unique(theData()[theData()$class %in% "Artillery Support Vehicles And Equipment", "system"]), selected=asveSelection(), multiple=TRUE)
        
    })
    
    mrlSelection <- reactive({
        
        if("Multiple Rocket Launchers" %in% input$classes){
            unique(theData()[theData()$class %in% "Multiple Rocket Launchers", "system"])
        } else {
            NULL
        }
        
    })
    
    output$mrlui <- renderUI({
        
        selectInput("mrl", "Multiple Rocket Launchers", choices=unique(theData()[theData()$class %in% "Multiple Rocket Launchers", "system"]), selected=mrlSelection(), multiple=TRUE)
        
    })
    
    cpcsSelection <- reactive({
        
        if("Command Posts And Communications Stations" %in% input$classes){
            unique(theData()[theData()$class %in% "Command Posts And Communications Stations", "system"])
        } else {
            NULL
        }
        
    })
    
    output$cpcsui <- renderUI({
        
        selectInput("cpcs", "Command Posts And Communications Stations", choices=unique(theData()[theData()$class %in% "Command Posts And Communications Stations", "system"]), selected=cpcsSelection(), multiple=TRUE)
        
    })
    
    eveSelection <- reactive({
        
        if("Engineering Vehicles And Equipment" %in% input$classes){
            unique(theData()[theData()$class %in% "Engineering Vehicles And Equipment", "system"])
        } else {
            NULL
        }
        
    })
    
    
    output$eveui <- renderUI({
        
        selectInput("eve", "Engineering Vehicles And Equipment", choices=unique(theData()[theData()$class %in% "Engineering Vehicles And Equipment", "system"]), selected=eveSelection(), multiple=TRUE)
        
    })
    
    navySelection <- reactive({
        
        if("Naval Ships" %in% input$classes){
            unique(theData()[theData()$class %in% "Naval Ships", "system"])
        } else {
            NULL
        }
        
    })
    
    output$navyui <- renderUI({
        
        selectInput("navy", "Naval Ships", choices=unique(theData()[theData()$class %in% "Naval Ships", "system"]), selected=navySelection(), multiple=TRUE)
        
    })
    
    trucksSelection <- reactive({
        
        if("Trucks, Vehicles and Jeeps" %in% input$classes){
            unique(theData()[theData()$class %in% "Trucks, Vehicles and Jeeps", "system"])
        } else {
            NULL
        }
        
    })
    
    output$trucksui <- renderUI({
        
        selectInput("trucks", "Trucks, Vehicles and Jeeps", choices=unique(theData()[theData()$class %in% "Trucks, Vehicles and Jeeps", "system"]), selected=trucksSelection(), multiple=TRUE)
        
    })
    
    
    systemsToUse <- reactive({
        
        c(input$tanks, input$afv, input$apc, input$ifv, input$ifm, input$spatms, input$aircraft, input$helicopters, input$aag, input$spaag, input$stams, input$uav, input$ucav, input$ruav, input$jammers, input$radars, input$ta, input$spa, input$asve, input$mrl, input$cpcs, input$eve, input$navy, input$trucks)
        
    })
    
    outcomesToUse <- reactive({
        
        input$outcomes
        
    })
    
    countriesToUse <- reactive({
        
        input$countries
        
    })
    

    
    #output$classesui <- renderUI({
        
    #    selectInput("classes", "Classes", choices=theClasses(), selected=theClassesSelected(), multiple=TRUE)
        
    #})
    
    output$systemsui <- renderUI({
        
        selectInput("systems", "Systems", choices=theSystems(), selected=theSystemsSelected(), multiple=TRUE)
        
    })
    
    
    dataRawRussia <- reactive({
        
        the_data <- theData()
        the_data <- the_data[the_data$country %in% "Russia",]
        the_outcomes <- outcomesToUse()
        the_systems <- systemsToUse()
        the_countries <- countriesToUse()

        the_data_mod <- the_data[the_data$system %in% the_systems,]
        the_data_mod <- the_data_mod[the_data_mod$status %in% the_outcomes,]
        the_data_mod <- the_data_mod[the_data_mod$origin %in% the_countries,]


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
        the_outcomes <- outcomesToUse()
        the_systems <- systemsToUse()
        the_countries <- countriesToUse()
        
        the_data_mod <- the_data[the_data$system %in% the_systems,]
        the_data_mod <- the_data_mod[the_data_mod$status %in% the_outcomes,]
        the_data_mod <- the_data_mod[the_data_mod$origin %in% the_countries,]


        the_data_mod
        
    })
    
    dataModRussia <- reactive({
        
        the_outcomes <- outcomesToUse()
        the_systems <- systemsToUse()
        the_countries <- countriesToUse()

        the_data_mod <- dataFullRussia()
        the_data_mod <- the_data_mod[order(the_data_mod$Date), ]
        
        the_data_tidy <- date_crunch(the_data_mod)
        the_data_tidy <- the_data_tidy[order(the_data_tidy$Date), ]
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
        
        #if(length(the_outcomes)==4){
            data_class_frame <-  the_data_tidy %>%
            mutate(total_count = cumsum(total)) %>%
            mutate(cum_rolling10 = rollapplyr(total, width = 10, FUN = sum, partial = TRUE)) %>%
            drop_na(total_count)
        #} else if(length(the_outcomes) < 4){
          #  data_class_frame <-  the_data_tidy %>%
          #  dplyr::mutate(total = sum(dplyr::c_across(the_outcomes))) %>%
          #  mutate(total_count = cumsum(total)) %>%
          #  mutate(cum_rolling10 = rollapplyr(total, width = 10, FUN = sum, partial = TRUE)) %>%
         #   drop_na(total_count)
        #}
        
        data_class_frame <- data_class_frame[order(data_class_frame$Date), ]

        data_class_frame
    })
    
    
    dataRawUkraine <- reactive({
        
        the_data <- theData()
        the_data <- the_data[the_data$country %in% "Ukraine",]
        the_outcomes <- outcomesToUse()
        the_systems <- systemsToUse()
        the_countries <- countriesToUse()

        the_data_mod <- the_data[the_data$system %in% the_systems,]
        the_data_mod <- the_data_mod[the_data_mod$status %in% the_outcomes,]
        the_data_mod <- the_data_mod[the_data_mod$origin %in% the_countries,]


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
        the_outcomes <- outcomesToUse()
        the_systems <- systemsToUse()
        the_countries <- countriesToUse()

        the_data_mod <- the_data[the_data$system %in% the_systems,]
        the_data_mod <- the_data_mod[the_data_mod$status %in% the_outcomes,]
        the_data_mod <- the_data_mod[the_data_mod$origin %in% the_countries,]


        the_data_mod
        
    })
    
    dataModUkraine <- reactive({
        
        the_outcomes <- outcomesToUse()
        the_systems <- systemsToUse()
        the_countries <- countriesToUse()

        the_data_mod <- dataRawUkraine()
        the_data_mod <- the_data_mod[order(the_data_mod$Date), ]

        the_data_tidy <- date_crunch(the_data_mod)
        the_data_tidy <- the_data_tidy[order(the_data_tidy$Date), ]


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
        
        #if(length(the_outcomes)==4){
            data_class_frame <-  the_data_tidy %>%
            mutate(total_count = cumsum(total)) %>%
            mutate(cum_rolling10 = rollapplyr(total, width = 10, FUN = sum, partial = TRUE)) %>%
            drop_na(total_count)
        #} else if(length(the_outcomes) < 4){
          #  data_class_frame <-  the_data_tidy %>%
          #  dplyr::mutate(total = sum(dplyr::c_across(the_outcomes))) %>%
          #  mutate(total_count = cumsum(total)) %>%
          #  mutate(cum_rolling10 = rollapplyr(total, width = 10, FUN = sum, partial = TRUE)) %>%
         #   drop_na(total_count)
        #}
        
        data_class_frame <- data_class_frame[order(data_class_frame$Date), ]

        data_class_frame
    })
    
    
    dataMod <- reactive({
        
        russia <- dataModRussia()
        ukraine <- dataModUkraine()
        
        data_merged <- as.data.frame(rbindlist(list(russia, ukraine), use.names=TRUE, fill=TRUE))
        
        data_merged <- data_merged[order(data_merged$Date), ]
        
        data_merged <- data_merged %>%
          group_by(country) %>%
          arrange(Date) %>%
          mutate(Daily = total_count - lag(total_count, default = first(total_count)))
        
        data_merged
        
    })
    
    output$debug <- renderDataTable({
        
        dataMod()
    })
    
    bulkPlot <- reactive({
        
        data_class_frame <- dataMod()
        
        
        ggplot(data_class_frame, aes(Date, total_count, colour=country, shape=country)) +
        geom_col(data=data_class_frame, mapping=aes(Date, Daily, colour=country,  fill=country), alpha=0.8, position = position_dodge(0.7)) +
        stat_smooth(method="gam") +
        geom_point() +
        scale_x_date("", date_labels = "%m/%d") +
        scale_y_continuous("Equipment Lost") +
        scale_colour_manual(values = country_colors)  +
        theme_light()
        
    })
    
    output$bulkplot <- renderPlot({
        
        bulkPlot()
        
    })
    
    ratioPlot <- reactive({
        
       russia_frame <- dataMod()[dataMod()$country %in% "Russia",c("Date", "total_count")]
       colnames(russia_frame) <- c("Date", "Russia")
       ukraine_frame <- dataMod()[dataMod()$country %in% "Ukraine",c("Date", "total_count")]
       colnames(ukraine_frame) <- c("Date", "Ukraine")
       total_ratio_frame <- merge(russia_frame, ukraine_frame, by="Date")
       total_ratio_frame$Ratio <- total_ratio_frame$Russia/total_ratio_frame$Ukraine
        

        total_ratio_frame$Date <- as.Date(total_ratio_frame$Date, format="%m/%d/%Y")
        total_ratio_frame <- total_ratio_frame %>%
          arrange(Date) %>%
          mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

        
        
        ggplot(total_ratio_frame, aes(Date, Ratio)) +
        geom_col(data=total_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
        geom_point(show.legend=FALSE, size=0.1) +
        geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, aes(color="Ratio")) +
        scale_x_date(date_labels = "%m/%d") +
        scale_y_continuous("Ratio Ru:Ukr") +
        scale_color_manual(values = NA) +
        theme_light() +
        theme( legend.title = element_blank())
        
        
    })
    
    output$ratioplot <- renderPlot({
        
        ratioPlot()
        
    })
    
    comboPlot <- reactive({
        
        cowplot::plot_grid(bulkPlot(), ratioPlot(), rel_heights = c(2,1), ncol=1, align="hv")
        
    })

    output$comboplot <- renderPlot({
        
        tryCatch(comboPlot(), error=function(e) NULL)
        
    })
    
    
    
    })
