shinyUI(navbarPage("Russia-Ukraine",
id="nav",
theme = shinytheme("yeti"),

tabsetPanel(




tabPanel("Bulk Timeseries",
sidebarLayout(
sidebarPanel(width=3,

#checkboxInput("useallclasses", "Use All Classes", value=TRUE),
#checkboxInput("useallsystems", "Use All Systems", value=TRUE),
checkboxInput("dropall", "Start From Scratch", value=FALSE),
uiOutput("classesui"),

tags$hr(),

uiOutput("outcomesui"),
uiOutput("countriesui"),
uiOutput("tanksui"),
uiOutput("afvui"),
uiOutput("apcui"),
uiOutput("ifvui"),
uiOutput("imvui"),
uiOutput("spatmsui"),
uiOutput("aircraftui"),
uiOutput("helicoptersui"),
uiOutput("aagui"),
uiOutput("spaagui"),
uiOutput("stamsui"),
uiOutput("uavui"),
uiOutput("ucavui"),
uiOutput("ruavui"),
uiOutput("jammersui"),
uiOutput("radarsui"),
uiOutput("taui"),
uiOutput("spaui"),
uiOutput("asveui"),
uiOutput("mrlui"),
uiOutput("cpcsui"),
uiOutput("eveui"),
uiOutput("trucksui"),
uiOutput("navyui"),




#uiOutput("systemsui")


),

mainPanel(
tabsetPanel(
#tabPanel("Debug", dataTableOutput("debug")),
tabPanel("Plot",
    plotOutput("comboplot", height=800)),
tabPanel("Russia", dataTableOutput("russiaunits")),
tabPanel("Ukraine", dataTableOutput("ukraineunits"))
)
)

)
),

tabPanel("Dupuy Resampler",

sidebarLayout(
sidebarPanel(width=3,

actionButton("run", "Run"),

sliderInput("iterations", "# Resampling Iterations", min=10, max=1000000, value=10000, step=10),

tags$hr(),
h2("Russia"),
sliderInput("russia_strength", "Russian Strength", min=25, max=250, value=180, step=1),
selectInput("russia_strength_modifier_type", "Strength Modifier Type", choices=c("Absolute", "Relative"), selected="Relative"),
uiOutput("russia_strength_modifier_ui"),
checkboxInput("russian_reinforcements", "Russian Reinforcements Allowed?", value=TRUE),
sliderInput("russia_terrain", "Russian Terrain", min=0.1, max=2, value=1.4, step=0.1),
sliderInput("russia_terrain_modifier", "Russian Terrain Variance", min=0.1, max=1, value=0.1, step=0.1),
sliderInput("russia_season", "Russian Season", min=0.1, max=2, value=1.1, step=0.1),
sliderInput("russia_season_modifier", "Russian Terrain Variance", min=0.1, max=1, value=0.1, step=0.1),
sliderInput("russia_posture", "Russian Posture", min=0.1, max=2, value=1.5, step=0.1),
sliderInput("russia_posture_modifier", "Russian Posture Variance", min=0.1, max=1, value=0.1, step=0.1),
sliderInput("russia_air", "Russian Air Defense", min=0.1, max=1, value=0.8, step=0.1),
sliderInput("russia_air_modifier", "Russian Air Defense Variance", min=0.1, max=1, value=0.2, step=0.1),
sliderInput("russia_morale", "Russian Morale", min=0.1, max=1, value=0.8, step=0.1),
sliderInput("russia_morale_modifier", "Russian Morale Variance", min=0.1, max=1, value=0.2, step=0.1),

tags$hr(),

h2("Ukraine"),
sliderInput("ukraine_strength", "Ukranian Strength", min=25, max=250, value=77, step=1),
selectInput("ukraine_strength_modifier_type", "Strength Modifier Type", choices=c("Absolute", "Relative"), selected="Relative"),
uiOutput("ukraine_strength_modifier_ui"),
checkboxInput("ukraine_reinforcements", "Ukraniain Reinforcements Allowed?", value=TRUE),
sliderInput("ukraine_terrain", "Ukranian Terrain", min=0.1, max=2, value=1.5, step=0.1),
sliderInput("ukraine_terrain_modifier", "Ukrainian Terrain Variance", min=0.1, max=1, value=0.1, step=0.1),
sliderInput("ukraine_season", "Ukrainian Season", min=0.1, max=2, value=1.1, step=0.1),
sliderInput("ukraine_season_modifier", "Ukrainian Terrain Variance", min=0.1, max=1, value=0.1, step=0.1),
sliderInput("ukraine_posture", "Ukrainian Posture", min=0.1, max=2, value=1.5, step=0.1),
sliderInput("ukraine_posture_modifier", "Ukrainian Posture Variance", min=0.1, max=1, value=0.1, step=0.1),
sliderInput("ukraine_air", "Ukrainian Air Defense", min=0.1, max=1, value=1, step=0.1),
sliderInput("ukraine_air_modifier", "Ukrainian Air Defense Variance", min=0.1, max=1, value=0.2, step=0.1),
sliderInput("ukraine_morale", "Ukrainian Morale", min=0.1, max=1, value=1, step=0.1),
sliderInput("ukraine_morale_modifier", "Ukrianian Morale Variance", min=0.1, max=1, value=0.2, step=0.1),

),

mainPanel(
tabsetPanel(
tabPanel("Ratio Plot", plotOutput("outcome_densities"),
uiOutput("simulationresultsui"),
tags$hr(),
downloadButton("downloadDensity", "Download")
),
tabPanel("Strength Plot", plotOutput("outcome_strength"),
uiOutput("simulationresults2ui"),
tags$hr(),
downloadButton("downloadStrength", "Download")
),
tabPanel("Outcomes", dataTableOutput("outcome_table"),
tags$hr(),
downloadButton("downloadOutcomes", "Download"))

)
)

))
)))
