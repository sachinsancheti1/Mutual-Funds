library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Large Cap Funds in India"),
  sidebarPanel({
    tagList(
      uiOutput("displayside")
    )
  }),
  mainPanel(uiOutput("displaymain"))
))
