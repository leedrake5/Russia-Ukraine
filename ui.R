shinyUI(navbarPage("Russia-Ukraine",
id="nav",
theme = shinytheme("yeti"),

tabPanel("Bulk Timeseries",
sidebarLayout(
sidebarPanel(width=3,

checkboxInput("useallclasses", "Use All Classes", value=TRUE),
checkboxInput("useallsystems", "Use All Systems", value=TRUE),

tags$hr(),

uiOutput("outcomesui"),
uiOutput("classesui"),
uiOutput("systemsui")


),

mainPanel(
tabsetPanel(
tabPanel("Plot", plotOutput("bulkplot")),
tabPanel("Russia", dataTableOutput("russiaunits")),
tabPanel("Ukraine", dataTableOutput("ukraineunits"))
)
)

)
)
))
