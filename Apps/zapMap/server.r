library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)
library(data.table)
library(DT)

function(input, output, session) {
    
    country_colors <-   c("Russia" = "#E4181C", "Ukraine" = "#0057B8")

    # Leaflet bindings are a bit slow; for now we'll just sample to compensate
    set.seed(100)
    
    
    zipdataInput <- reactive({
    data[data$Date >= input$dateselect[1] & data$Date <= input$dateselect[2] , ]
    
    })
    
    
    # By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
    # will be drawn last and thus be easier to see
    zipdataToo <- reactive(
    {zipdata <- zipdataInput()
    #zipdata[order(zipdata$centile),]
    })
    


  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
      leaflet() %>%
            addTiles() %>%
      setView(lng = 35.865555, lat = 47.459433, zoom = 10)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
      zipdata <- zipdataToo()
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(zipdata,
      lat >= latRng[1] & lat <= latRng[2] &
        lng >= lngRng[1] & lng <= lngRng[2])
  })

  # Precalculate the breaks we'll need for the two histograms
  #centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks


  output$typeselectui <- renderUI({
      
      selectInput("typeselect", "Type", choices=unique(data$Type), selected=unique(data$Type)[!unique(data$Type) %in% "Other"], multiple=TRUE)
      
  })
  
  output$dateselectui <- renderUI({
      
      sliderInput("dateselect",
                                "Dates:",
                                min = as.Date("2023-06-01","%Y-%m-%d"),
                                max = as.Date(firms$acq_date[nrow(firms)],"%Y-%m-%d"),
                                value=c(as.Date("2023-06-01","%Y-%m-%d"), as.Date(firms$acq_date[nrow(firms)],"%Y-%m-%d")),
                                timeFormat="%m-%d")
  })
  
  dataSelected <- reactive({
      data <- zipsInBounds()
      
      data[data$Type %in% input$typeselect,]
      
  })
  
  ukraineMod <- reactive({
      
      data <- dataSelected()

      
      data[data$Country=="Ukraine", ]

  })
  
  ukraineTotal <- reactive({
      ukraine_total_temp <- data.frame(Date=ukraineMod()[,c("Date")])
      ukraine_total_melt <- ukraine_total_temp %>%
          group_by(Date) %>%
          summarise(Daily=n())

      ukraine_total_melt$Total <- cumsum(ukraine_total_melt$Daily)
      ukraine_total_melt <- ukraine_total_melt[1:nrow(ukraine_total_melt)-1,]
      ukraine_total_melt$Country <- "Ukraine"
      
      ukraine_total_melt
  })
  

  russiaMod <- reactive({
      
      data <- dataSelected()

      
      data[data$Country=="Russia", ]

  })
  
  russiaTotal <- reactive({
      russia_total_temp <- data.frame(Date=russiaMod()[,c("Date")])
      russia_total_melt <- russia_total_temp %>%
          group_by(Date) %>%
          summarise(Daily=n())

      russia_total_melt$Total <- cumsum(russia_total_melt$Daily)
      russia_total_melt <- russia_total_melt[1:nrow(russia_total_melt)-1,]
      russia_total_melt$Country <- "Russia"
      russia_total_melt
  })
  
  totalMelt <- reactive({
      
      as.data.frame(data.table::rbindlist(list(russiaTotal(), ukraineTotal())))
      
  })
  
  ratioFrame <- reactive({
      
      ukraine_small <- ukraineTotal()[,c("Date", "Total")]
      colnames(ukraine_small) <- c("Date", "Ukraine_Total")
      russia_small <- russiaTotal()[,c("Date", "Total")]
      colnames(russia_small) <- c("Date", "Russia_Total")

      total_ratio_merge<- merge(ukraine_small, russia_small, by="Date")
      total_ratio_frame <- data.frame(Date=total_ratio_merge$Date, Ratio=total_ratio_merge$Russia_Total/total_ratio_merge$Ukraine_Total)

      total_ratio_frame$Date <- as.Date(total_ratio_frame$Date)
      total_ratio_frame <- total_ratio_frame %>%
        arrange(Date) %>%
        mutate(Daily = Ratio - lag(Ratio, default = first(Ratio)))

      total_ratio_frame
  })
  
  
  currentGen <- reactive({
      
      total_melt <- totalMelt()
      
      dist.plot <- if(input$uselabs){
          ggplot(total_melt, aes(Date, Total, colour=Country)) +
          geom_col(data=total_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
          geom_point(show.legend=FALSE, size=0.1) +
          geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
          scale_x_date(date_labels = "%m/%d") +
          scale_y_continuous("Equipment Losses") +
          ggtitle(paste0("Equipment losses through ", Sys.Date())) +
          theme_light() +
          scale_colour_manual(values = country_colors)  +
          scale_fill_manual(values = country_colors) +
          theme(legend.position="bottom")
      } else {
          ggplot(total_melt, aes(Date, Total, colour=Country)) +
          geom_col(data=total_melt, mapping=aes(Date, Daily, colour=Country,  fill=Country), alpha=0.8, position = position_dodge(0.7)) +
          geom_point(show.legend=FALSE, size=0.1) +
          geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
          scale_x_date(date_labels = "%m/%d") +
          scale_y_continuous("Equipment Losses") +
          ggtitle(paste0("Equipment losses through ", Sys.Date())) +
          theme_light() +
          scale_colour_manual(values = country_colors)  +
          scale_fill_manual(values = country_colors) +
          theme(legend.position="none")
      }
      

      
      dist.plot
      
  })
  
  output$current_gen <- renderPlot({
      
      currentGen()
      
  })

  output$ratio_plot <- renderPlot({
    # If no zipcodes are in view, don't plot
    total_ratio_frame <- ratioFrame()
   
   ggplot(total_ratio_frame, aes(Date, Ratio)) +
   geom_col(data=total_ratio_frame, mapping=aes(Date, Daily), alpha=0.8, position = position_dodge(0.7)) +
   geom_point(show.legend=FALSE, size=0.1) +
   geom_line(stat="smooth", method="gam", size=1, linetype="solid", alpha=0.5, show.legend=FALSE) +
   scale_x_date(date_labels = "%m/%d") +
   scale_y_continuous("Equipment Loss Ratio Ru:Ukr", breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
   ggtitle(paste0("Equipment loss ratio through ", Sys.Date())) +
   theme_light()
  })
  
  zipParsed <- reactive({
      zipdataToo()[zipdataToo()$Type %in% input$typeselect, ]
      
  })
  

  

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
     zipdata <- zipParsed()
     firms_plot_data <- firms[firms$acq_date == as.Date(input$dateselect[2]),]

    colorBy <- input$color
    sizeBy <- input$size
    
    firmspal <- colorFactor(
      palette = 'orange',
      domain = "FIRMS"
    )

    if (input$color == "Type") {

        pal <- colorFactor(
          palette = 'Dark2',
          domain = input$typeselect
        )
        
        leafletProxy("map", data = zipdata) %>%
            clearControls() %>%
            clearShapes() %>%
            clearMarkers() %>%
            addCircleMarkers(~lng, ~lat, radius=input$size, layerId=~Type,
            stroke=FALSE, fillOpacity=0.6, fillColor=~pal(Type)) %>%
            addCircleMarkers(~firms_plot_data$longitude, ~firms_plot_data$latitude, radius=5,
                stroke=FALSE, fillOpacity=0.7, fillColor="orange") %>%
            addLegend("bottomleft", pal=pal, values=input$typeselect, title=input$color,
            layerId="Equipment Legend") %>%
            addLegend("bottomleft", pal=firmspal, values="FIRMS", title="Current FIRMS",
              layerId="FIRMS")
      } else if(input$color=="Country"){
          pal <- colorFactor(
          palette = c('dark red', 'navy blue'),
          domain = c("Russia", "Ukraine")
          )
      
      leafletProxy("map", data = zipdata) %>%
        clearControls() %>%
        clearShapes() %>%
        clearMarkers() %>%
        addCircleMarkers(~lng, ~lat, radius=input$size, layerId=~Type,
          stroke=FALSE, fillOpacity=0.4, fillColor=~pal(Country)) %>%
        addCircleMarkers(~firms_plot_data$longitude, ~firms_plot_data$latitude, radius=5,
            stroke=FALSE, fillOpacity=0.7, fillColor="orange") %>%
        addLegend("bottomleft", pal=pal, values=c("Russia", "Ukraine"), title=input$color,
          layerId="colorLegend") %>%
        addLegend("bottomleft", pal=firmspal, values="FIRMS", title="Current FIRMS",
                layerId="FIRMS")
    }



  })

  # Show a popup at the given location
  showZipcodePopup <- function(zipcode, lat, lng) {
    selectedZip <- zipParsed()[zipParsed()$ID == zipcode,]
    content <- as.character(tagList(
      tags$h4("Type:", as.character(selectedZip$Type[1])),
      tags$br(),
      sprintf("Model: ", as.character(selectedZip$Model[1])), tags$br(),
      sprintf("Status: ", as.character(selectedZip$Status[1])), tags$br(),
      sprintf("Date: ", as.character(selectedZip$Date[1])), tags$br()

      )
    )
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    #event <- input$map_marker_click
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
  
  ####When zipcode is selected, show popup with city info
  
  #observe({
  #    leafletProxy("map") %>% clearPopups()
  #    event <- as.numeric(paste(input$yourzipcode))
  #    zipframe <- subset(zipcodes, zipcodes$zip_code==event)
      
      
      
  #    if (is.null(event))
  #    return()
      
  #    isolate({
  #        showZipcodePopup(event, zipframe$latitude, zipframe$longitude)
  #    })
  #})



  ## Data Explorer ###########################################
  
  reactiveZip <- reactive({
      
      smalls <- zipParsed()
      
      smalls
      
  })



  output$ziptable <- DT::renderDataTable({
      
      
    df <- reactiveZip()
    
    df$Oryx.URL <- paste0("<a href='",df$Oryx.URL,"'>",df$Oryx.URL,"</a>")
    df$Source <- paste0("<a href='",df$Source,"'>",df$Source,"</a>")


    DT::datatable(df, escape=FALSE)
  })
  
  
  output$downloaddata <- downloadHandler(
  filename = function() { paste("oryxData", '.csv', sep=',') },
  content = function(file
  ) {
      write.csv(reactiveZip(), file)
  }
  )
  
  
  
}
