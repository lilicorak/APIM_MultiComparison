#install.packages("dplyr", "shiny", "shinyWidgets", "ggplot2", "scales", "shinythemes", "shinydashboardPlus")

require(dplyr)
require(shiny)
require(shinyWidgets)
require(ggplot2)
require(scales)
require(shinythemes)
require(shinydashboardPlus)

# Read in the data
read_data <- function (filename) {
  dat <- subset(read.csv(paste0("data/", filename, ".csv"), head=T, sep=","), 
                select = c("VAR", "CLASS_FLAG", "PROV_RES", "SEX", "AGE", "WITH_VAL_0", "N", "MEAN", "NWGT", "SUM", 
                           "P75", "P25", "P_0", "P_10", "P_20", "P_30", "P_40", "P_50", "P_60", "P_70", "P_80", "P_90", "P_100"))
  
  dat$CLASS_FLAG <- factor(dat$CLASS_FLAG, levels = c("ALL", "1", "2", "3", "4", "5"))
  dat$PROV_RES <- factor(dat$PROV_RES, levels = c("ALL", "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YT", "NT", "NU"))
  dat$SEX <- factor(dat$SEX, levels = c("ALL", "1", "2"))
  dat$AGE <- factor(dat$AGE, levels = c("All", "0-14", "15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65+", "Not Stated"))

  return(dat)
}

APIM_2017 <- read_data("APIM_tab_2017")
APIM_2018 <- read_data("APIM_tab_2018")

CHS_2017 <- read_data("CHS_tab_2017")
CHS_2018 <- read_data("CHS_tab_2018")

CIS_2017 <- read_data("CIS_tab_2017")
CIS_2018 <- read_data("CIS_tab_2018")

CISPlus_2017 <- read_data("CISPlus_tab_2017")
CISPlus_2018 <- read_data("CISPlus_tab_2018")

SHS_2017 <- read_data("SHS_tab_2017")
SHS_2018 <- read_data("SHS_tab_2018")


# Set up the UI
ui <- (fluidPage(
  theme = shinytheme("paper"),
  useShinydashboardPlus(),
  tags$style(
    HTML(
      ".navbar-default .navbar-brand {color: #666666;}
      .navbar-default .navbar-brand:hover {color: #666666;}
      .btn {-webkit-box-shadow: none; box-shadow: none; position: relative;}
      .box-header .box-title {font-size: 14px;}"
    )
  ),
  navbarPage(
    "Multi-source comparison of survey income data", 
    
  ### Overview page
    tabPanel("Overview",
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 selectInput("overviewFiles",
                             "Select files to compare:",
                             choices = c("APIM", "CIS", "CISPlus", "CHS", "SHS"),
                             selected = c("APIM", "CIS", "CISPlus", "CHS", "SHS"),
                             multiple = T),
                 radioButtons("overviewYear",
                              "Year:",
                              choices = seq(2015, 2018, 1),
                              selected = 2018),
                 selectInput("overviewVar",
                             "Variable:",
                             choices = unique(APIM_2017$VAR),
                             selected = "TOTINC"),
                 selectInput("overviewClass",
                             "Class flag:",
                             choices = unique(APIM_2017$CLASS_FLAG),
                             selected = "ALL"),
                 selectInput("overviewAge",
                             "Age group:",
                             choices = unique(APIM_2017$AGE),
                             selected = "All"),
                 selectInput("overviewProv",
                             "Province of residence:",
                             choices = unique(APIM_2017$PROV_RES),
                             selected = "ALL")
               ),
               mainPanel(
                 width = 10,
                 fluidRow(column(4, wellPanel(p("mean by prov"),
                                              plotOutput("overviewMean"))),
                          column(4, wellPanel(p("NGWT by prov"),
                                              plotOutput("overviewNGWT"))),
                          column(4, wellPanel(p("P50 by prov"),
                                              plotOutput("overviewP50")))),
                 fluidRow(boxPlus(title = "overview data table",
                                  closable= F,
                                  collapsible = T,
                                  collapsed = T,
                                  status = "info",
                                  width = 12,
                                  dataTableOutput("overviewData")))
               )
             )),
  
  ### Percentiles page
    tabPanel("Percentiles",
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 selectInput("percentileFiles",
                             "Select files to compare:",
                             choices = c("APIM", "CIS", "CISPlus", "CHS", "SHS"),
                             selected = c("APIM", "CIS", "CISPlus", "CHS", "SHS"),
                             multiple = T),
                 radioButtons("percentileYear",
                              "Year:",
                              choices = seq(2015, 2018, 1),
                              selected = 2018),
                 selectInput("percentileVar",
                             "Variable:",
                             choices = unique(APIM_2017$VAR),
                             selected = "TOTINC"),
                 selectInput("percentileClass",
                             "Class flag:",
                             choices = unique(APIM_2017$CLASS_FLAG),
                             selected = "ALL"),
                 selectInput("percentileAge",
                             "Age group:",
                             choices = unique(APIM_2017$AGE),
                             selected = "All"),
                 selectInput("percentileProv",
                             "Province of residence:",
                             choices = unique(APIM_2017$PROV_RES),
                             selected = "ALL")
               ),
               mainPanel(
                 width = 10,
                 fluidRow(wellPanel(p("percentiles plot"),
                                    plotOutput("percentilePlot"))),
                 fluidRow(boxPlus(title = "percentile data table",
                                  closable= F,
                                  collapsible = T,
                                  collapsed = T,
                                  status = "info",
                                  width = 12,
                                  dataTableOutput("percentileData"))))
             )),
  
  ### Demographics page
    tabPanel("Demographics",
             sidebarLayout(
               sidebarPanel(
                 width = 2, 
                 selectInput("demoFiles",
                             "Select files to compare:",
                             choices = c("APIM", "CIS", "CISPlus", "CHS", "SHS"),
                             selected = c("APIM", "CIS", "CISPlus", "CHS", "SHS"),
                             multiple = T),
                 radioButtons("percentileYear",
                              "Year:",
                              choices = seq(2015, 2018, 1),
                              selected = 2018),
                 selectInput("demoVar",
                             "Variable:",
                             choices = unique(APIM_2017$VAR),
                             selected = "TOTINC"),
                 selectInput("demoStat",
                             "Statistic:",
                             choices = c("N", "MEAN", "NWGT", "SUM", "P25", "P75", "P_0", "P_10", "P_20", "P_30", 
                                         "P_40", "P_50", "P_60", "P_70", "P_80", "P_90", "P_100"),
                             selected = "MEAN"),
                 selectInput("demoClass",
                             "Class flag:",
                             choices = unique(APIM_2017$CLASS_FLAG),
                             selected = "ALL"),
                 selectInput("demoAge",
                             "Age group:",
                             choices = unique(APIM_2017$AGE),
                             selected = "All"),
                 selectInput("demoProv",
                             "Province of residence:",
                             choices = unique(APIM_2017$PROV_RES),
                             selected = "ALL")
                 ),
               mainPanel(
                 width = 10,
                 fluidRow(
                   column(5, wellPanel(p("years plot"),
                                       plotOutput("demoYearPlot"))),
                   column(7, wellPanel(p("age plot"),
                                       plotOutput("demoAgePlot")))
                 ),
                 fluidRow(
                   column(7, wellPanel(p("prov plot"),
                                       plotOutput("demoProvPlot"))),
                   column(5, wellPanel(p("sex plot"),
                                       plotOutput("demoSexPlot")))
                 ),
                 fluidRow(boxPlus(title = "demographic data table",
                                  closable= F,
                                  collapsible = T,
                                  collapsed = T,
                                  status = "info",
                                  width = 12,
                                  dataTableOutput("demoData"))))
                 ))
             )
    ))

# Set up the server function
server <- function(input, output, session) {
  
### Overview page
  
  # Create data file
  
  
  
### Percentiles page
  
  
### Demographics page
  
  
}

# Call the shiny function
shinyApp(ui, server)

