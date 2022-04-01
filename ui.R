shinyUI(navbarPage("sigParameters",
id="nav",
theme = shinytheme("yeti"),

tabPanel("Bulk Timeseries",
sidebarLayout(
sidebarPanel(width=3,

checkboxUI("useallclasses", "Use All Classes", value=TRUE)
checkboxUI("useallsystems", "Use All Systems", value=TRUE)

)

)

)
))
