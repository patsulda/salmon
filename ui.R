library(shiny)
library(shinydashboard)
library(dplyr)
library(plyr)
library(DT)
library(ggplot2)
library(xlsx)

tab.summary <- function(name)
{
  tab.contents <- 
     "Coming soon..."
  return(tab.contents)
}

tab.charts <- function(name)
{
  tab.contents <- 
  {
    div(
    fluidRow(
      column(width = 6,
             plotOutput(paste0(name, ".vac.sal.plot"))),
      
      column(width = 6,
             plotOutput(paste0(name, ".res.sal.plot")))
      ),
    fluidRow(br(),
             HTML('<center>'),
             plotOutput(paste0(name, ".intersection.plot"), width = "95%"),
             HTML('</center>')
             )
    )

  }
  return(tab.contents)
}

tab.raw.res <- function(name)
{
  tab.contents <- dataTableOutput(paste0(name, ".res.dt"))

  return(tab.contents)
}

tab.raw.vac <- function(name)
{
  tab.contents <- dataTableOutput(paste0(name, ".vac.dt"))
  
  return(tab.contents)
}

details <- function(name)
{
  page <- fluidRow(
    tabBox(
      title = tools::toTitleCase(name), side = "right", width = 12, selected = "Charts",
      id = paste0("tabset.", name),
      tabPanel("Summary", tab.summary(name = name)),
      tabPanel("Charts", tab.charts(name)),
      tabPanel("Raw data (RES)", tab.raw.res(name = name)),
      tabPanel("Raw data (VAC)", tab.raw.vac(name = name))
    ))
  
  return(page)
}

dashboardPage(
  skin = "blue", 
  dashboardHeader(title = "Salmon"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overall Summary", tabName = "overall", icon = icon("table")),
      menuItem("Details", icon = icon("black-tie"), tabName = "chart",
               startExpanded = TRUE,
               menuSubItem("Analyst", "analyst", icon = NULL),
               menuSubItem("UX/UI Designer", "designer", icon = NULL),
               menuSubItem("Content", "content", icon = NULL),
               menuSubItem("Developer (front-end)", "dev-front", icon = NULL),
               menuSubItem("Developer (back-end)", "dev-back", icon = NULL),
               menuSubItem("Developer (full-stack)", "dev-full", icon = NULL),
               menuSubItem("DevOps", "devops", icon = NULL),
               menuSubItem("Manager", "manager", icon = NULL),
               menuSubItem("QA", "qa", icon = NULL)
               ),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overall",
              fluidRow(
                HTML('<center><table width = "95%"><tr><td align = "right">'),
                downloadButton("totals.download"),
                dataTableOutput("totals"),
                HTML('</td></tr></table></center>')
              )
      ),
      
      tabItem(tabName = "analyst", details("analyst")),
      tabItem(tabName = "designer", details("designer")),
      tabItem(tabName = "content", details("content")),
      tabItem(tabName = "dev-front", details("dev.front")),
      tabItem(tabName = "dev-back", details("dev.back")),
      tabItem(tabName = "dev-full", details("dev.full")),
      tabItem(tabName = "devops", details("devops")),
      tabItem(tabName = "manager", details("manager")),
      tabItem(tabName = "qa", details("qa")),
      
      tabItem(tabName = "about",
              HTML('<center><img src="salmon.png" width="200"><p/><p/>',
                   readChar("about.txt", file.info("about.txt")$size),
                  '<small><div>Icons made by <a href="http://www.freepik.com" title="Freepik">Freepik</a> from <a href="https://www.flaticon.com/" title="Flaticon">www.flaticon.com</a> is licensed by <a href="http://creativecommons.org/licenses/by/3.0/" title="Creative Commons BY 3.0" target="_blank">CC 3.0 BY</a></div>
                   </small><p/></center>
                   ')
    ))
    ,
    tags$head(HTML(
      '<link href="https://fonts.googleapis.com/css?family=Raleway" rel="stylesheet"/>

      <link rel="icon" type="image/png" sizes="32x32" href="/favicon-32x32.png">
      <link rel="icon" type="image/png" sizes="16x16" href="/favicon-16x16.png">
      
      <style>
.main-header .logo {
      font-family: "Raleway", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
}
      </style>
      '), tags$head(tags$link(rel="shortcut icon", href="favicon.ico")))
  )
)