
# Do not need to run install.packages if using SIP_runApp script
#install.packages(c("dplyr", "shiny", "shinyWidgets", "ggplot2", "scales", "shinythemes", "shinydashboardPlus", 
#"DT", "ggsci", "plotly", "ggiraph", "reshape2", "data.table"))

require(dplyr)
require(shiny)
require(shinyWidgets)
require(ggplot2)
require(scales)
require(shinythemes)
require(shinydashboardPlus)
require(DT)
require(ggsci)
require(ggiraph)
require(reshape2)
require(data.table)

filesList <- c("APIM", "CIS", "CISPlus", "CHS", "SHS")
minYear <- 2015
maxYear <- 2018

dataPath <- "data/"

# Read in the data
read_data <- function (filename, vars_select) {
  dat <- subset(fread(paste0(dataPath, filename, ".csv"), sep=",", data.table = FALSE), 
                select = vars_select)
  
  dat$CLASS_FLAG <- factor(dat$CLASS_FLAG, levels = c("ALL", "1", "2", "3", "4", "5"))
  dat$PROV_RES <- factor(dat$PROV_RES, levels = c("ALL", "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YT", "NT", "NU", "99"))
  dat$SEX <- factor(dat$SEX, levels = c("ALL", "1", "2"))
  dat$AGE <- factor(dat$AGE, levels = c("All", "0-14", "15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65+", "Not Stated"))
  
  return(dat)
}

data_vars <- c("YEAR", "FILE", "PROV_RES", "VAR", "CLASS_FLAG", "SEX", "AGE", "WITH_VAL_0", "N", "MEAN", "NWGT", "SUM", 
               "P25", "P75", "P_0", "P_10", "P_20", "P_30", "P_40", "P_50", "P_60", "P_70", "P_80", "P_90", "P_100")

APIM <<- rbind(#read_data("APIM_1_2015_tab_T", data_vars),
               #read_data("APIM_1_2016_tab_T", data_vars),
               read_data("APIM_1_2017_tab_T", data_vars), 
               read_data("APIM_1_2018_tab_T", data_vars))

CHS <<- rbind(read_data("CHS_99_2018_tab_T", data_vars))

CIS <<- rbind(read_data("CIS_2_2017_tab_T", data_vars), 
              read_data("CIS_7_2018_tab_T", data_vars))

CISPlus <<- rbind(read_data("CISPlus_2_2017_tab_T", data_vars))

SHS <<- rbind(read_data("SHS_3_2018_tab_T", data_vars))


# Set up the UI
ui <- (fluidPage(
  theme = shinytheme("paper"),
  useShinydashboardPlus(),
  tags$style(
    HTML(
      ".navbar-default .navbar-brand {color: #666666;}
      .navbar-default .navbar-brand:hover {color: #666666;}
      .btn {-webkit-box-shadow: none; box-shadow: none; position: relative;}
      .box-header .box-title {font-size: 14px;}
      .box.box-info {border-top-color: #2196f3;}
      .dropdown-menu {box-shadow: none; -webkit-box-shadow: none; margin-top: 5px; background-color: #eeeeee;}"
    )
    ),
  navbarPage(
    "Multi-source comparison of survey income data",
    
    ### Overview page
    tabPanel("Overview",
             fluidPage(
               dropdownButton(
                 circle = TRUE,
                 status = "primary",
                 icon = icon("gear"),
                 tooltip = tooltipOptions(title = "Click to open inputs"),
                 radioGroupButtons(
                   "overviewYear",
                   "Year:",
                   choices = seq(minYear, maxYear, 1),
                   selected = maxYear,
                   status = "primary",
                   individual = TRUE
                 ),
                 fluidRow(column(
                   width = 6,
                   selectInput(
                     "overviewFiles",
                     "Select files to compare:",
                     choices = filesList,
                     selected = filesList,
                     multiple = T
                   )
                 ),
                 column(
                   width = 6,
                   selectInput(
                     "overviewProv",
                     "Province of residence:",
                     choices = c("ALL", "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YT", "NT", "NU", "99"),
                     selected = c("ALL", "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YT", "NT", "NU", "99"),
                     multiple = T
                   )
                 )),
                 fluidRow(column(
                   width = 6,
                   selectInput(
                     "overviewVar",
                     "Variable:",
                     choices = unique(APIM$VAR),
                     selected = "TOTINC"
                   )
                 ),
                 column(
                   width = 6,
                   selectInput(
                     "overviewClass",
                     "Class flag:",
                     choices = c("ALL", "1", "2", "3", "4", "5"),
                     selected = "ALL"
                   )
                 )),
                 fluidRow(column(
                   6,
                   selectInput(
                     "overviewAge",
                     "Age group:",
                     choices = c("All", "0-14", "15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65+", "Not Stated"),
                     selected = "All"
                   )
                 ),
                 column(
                   6,
                   selectInput(
                     "overviewSex",
                     "Sex:",
                     choices = c("ALL", "1", "2"),
                     selected = "ALL"
                   )
                 )),
                 actionButton(
                   "overviewUpdate",
                   label = "Update",
                   width = "100%",
                   class = "btn btn-primary btn-custom"
                 )
               ),
               fluidRow(
                 column(4, girafeOutput("overviewMean", height = "400px")),
                 column(4, girafeOutput("overviewNGWT", height = "400px")),
                 column(4, girafeOutput("overviewP50", height = "400px"))
               ),
               fluidRow(
                 boxPlus(
                   title = "Data table",
                   closable = F,
                   collapsible = T,
                   collapsed = T,
                   status = "info",
                   width = 12,
                   DTOutput("overviewData")
                 )
               )
               
             )),
    
    ### Percentiles page
    tabPanel(
      "Percentiles",
      fluidPage(
        dropdownButton(
          circle = TRUE,
          status = "primary",
          icon = icon("gear"),
          tooltip = tooltipOptions(title = "Click to open inputs"),
          radioGroupButtons(
            "percentileYear",
            "Year:",
            choices = seq(minYear, maxYear, 1),
            selected = maxYear,
            status = "primary",
            individual = TRUE
          ),
          fluidRow(column(
            width = 6,
            selectInput(
              "percentileFiles",
              "Select files to compare:",
              choices = filesList,
              selected = filesList,
              multiple = T
            )
          ),
          column(
            width = 6,
            selectInput(
              "percentileProv",
              "Province of residence:",
              choices = c("ALL", "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YT", "NT", "NU", "99"),
              selected = "ALL"
            )
          )),
          fluidRow(column(
            width = 6,
            selectInput(
              "percentileVar",
              "Variable:",
              choices = unique(APIM$VAR),
              selected = "TOTINC"
            )
          ),
          column(
            width = 6,
            selectInput(
              "percentileClass",
              "Class flag:",
              choices = c("ALL", "1", "2", "3", "4", "5"),
              selected = "ALL"
            )
          )),
          fluidRow(column(
            6,
            selectInput(
              "percentileAge",
              "Age group:",
              choices = c("All", "0-14", "15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65+", "Not Stated"),
              selected = "All"
            )
          ),
          column(
            6,
            selectInput(
              "percentileSex",
              "Sex:",
              choices = c("ALL", "1", "2"),
              selected = "ALL"
            )
          )),
          actionButton(
            "percentileUpdate",
            label = "Update",
            width = "100%",
            class = "btn btn-primary btn-custom"
          )
        ),
        fluidRow(girafeOutput("percentilePlot", height = "400px")),
        fluidRow(
          boxPlus(
            title = "Data table",
            closable = F,
            collapsible = T,
            collapsed = T,
            status = "info",
            width = 12,
            dataTableOutput("percentileData")
          )
        )
      )
    ),
    
    ### Demographics page
    tabPanel("Demographics",
          fluidPage(
            dropdownButton(
              circle = TRUE,
              status = "primary",
              icon = icon("gear"),
              tooltip = tooltipOptions(title = "Click to open inputs"),
              radioGroupButtons(
                "demoYear",
                "Year:",
                choices = seq(minYear, maxYear, 1),
                selected = maxYear,
                status = "primary",
                individual = TRUE
              ),
              selectInput(
                "demoFiles",
                "Select files to compare:",
                choices = filesList,
                selected = filesList,
                multiple = T
              ),
              fluidRow(
                column(6,
                       selectInput(
                         "demoVar",
                         "Variable:",
                         choices = unique(APIM$VAR),
                         selected = "TOTINC"
                       )),
                column(6,
                       selectInput(
                         "demoStat",
                         "Statistic:",
                         choices = c("N", "MEAN", "NWGT", "SUM", "P25", "P75", "P_0", "P_10", "P_20", "P_30", 
                                     "P_40", "P_50", "P_60", "P_70", "P_80", "P_90", "P_100"),
                         selected = "MEAN"
                       ))
              ),
              fluidRow(
                column(6,             
                       selectInput(
                          "demoProv",
                          "Province of residence:",
                          choices = c("ALL", "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YT", "NT", "NU", "99"),
                          selected = "ALL"
                        )),
                column(6,
                       selectInput(
                          "demoClass",
                          "Class flag:",
                          choices = c("ALL", "1", "2", "3", "4", "5"),
                          selected = "ALL"
                        ))
              ),
              fluidRow(
                column(6,
                  selectInput(
                   "demoge",
                   "Age group:",
                   choices = c("All", "0-14", "15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65+", "Not Stated"),
                   selected = "All"
                  )
                ),
                column(6,
                   selectInput(
                     "demoSex",
                     "Sex:",
                     choices = c("ALL", "1", "2"),
                     selected = "ALL"
                   )
                 )),
             actionButton(
               "demoUpdate",
                label = "Update",
                width = "100%",
                class = "btn btn-primary btn-custom"
              )
            ),
            fluidRow(
              column(4, wellPanel(
                p("year plot"),
                plotOutput("demoYearPlot")
              )),
              column(4, wellPanel(p("sex plot"),
                                  plotOutput("demoSexPlot"))),
              column(4, wellPanel(
                p("class plot"),
                plotOutput("demoClassPlot")
              ))
            ),
            fluidRow(column(6, wellPanel(
                   p("prov plot"),
                   plotOutput("demoProvPlot")
                 )),
                 column(6, wellPanel(
                   p("age plot"),
                   plotOutput("demoAgePlot")
                 ))
              ),
              fluidRow(
                   boxPlus(
                     title = "demographic data table",
                     closable = F,
                     collapsible = T,
                     collapsed = T,
                     status = "info",
                     width = 12,
                     dataTableOutput("demoData")
                   )
                 )
             ))
  )
    ))



# Set up the server function
server <- function(input, output, session) {
  
### Overview page
  
  # Create data file
  overviewDataFile <- reactive({
    overview <- do.call("rbind", lapply(input$overviewFiles, get))
    overview <- subset(overview, YEAR == input$overviewYear &
                         VAR == input$overviewVar &
                         CLASS_FLAG == input$overviewClass &
                         AGE == input$overviewAge &
                         SEX == input$overviewSex &
                         PROV_RES %in% input$overviewProv)
    isNum <- sapply(overview, is.numeric)
    overview[isNum] <- lapply(overview[isNum], round, 2)
    return(overview)
  })
  
  output$overviewMean <- renderGirafe({
    input$overviewUpdate
    isolate({overviewMeanPlot <- ggplot(overviewDataFile()) + 
                                  geom_col_interactive(aes(x=PROV_RES, y= MEAN, fill = FILE, 
                                                           tooltip=paste0(FILE, ": ", format(MEAN, big.mark = ","))), width=0.8, position = "dodge") + 
                                  theme_classic() + 
                                  scale_fill_jama() +
                                  scale_y_continuous(labels = comma) + 
                                  theme(legend.position = "bottom") + 
                                  labs(title=paste0("Mean ", input$overviewVar, " by province and file, ", input$overviewYear), 
                                       fill = NULL,
                                       subtitle = paste0("Class: ", input$overviewClass, "; Age: ", input$overviewAge, "; Sex: ", input$overviewSex))
            girafe(ggobj = overviewMeanPlot, width_svg = 5, height_svg = 5)})
  })
  
  output$overviewNGWT <- renderGirafe({
    input$overviewUpdate
    isolate({overviewNWGTPlot <- ggplot(overviewDataFile()) + 
                                  geom_col_interactive(aes(x=PROV_RES, y= NWGT, fill = FILE, 
                                                           tooltip=paste0(FILE, ": ", format(NWGT, big.mark = ","))), width=0.8, position = "dodge") + 
                                  theme_classic() + 
                                  scale_fill_jama() +
                                  scale_y_continuous(labels = comma) + 
                                  theme(legend.position = "bottom") + 
                                  labs(title=paste0("NWGT ", input$overviewVar, " by province and file, ", input$overviewYear), 
                                       fill = NULL,
                                       subtitle = paste0("Class: ", input$overviewClass, "; Age: ", input$overviewAge, "; Sex: ", input$overviewSex))
            girafe(ggobj = overviewNWGTPlot, width_svg = 5, height_svg = 5)})
  })

  output$overviewP50 <- renderGirafe({
    input$overviewUpdate
    isolate({overviewP50Plot <- ggplot(overviewDataFile()) + 
                                  geom_bar_interactive(stat="identity", 
                                                       aes(x=PROV_RES, y= P_50, fill = FILE, 
                                                          tooltip=paste0(FILE, ": ", format(P_50, big.mark = ","))), width=0.8, position = "dodge") + 
                                  theme_classic() + 
                                  scale_fill_jama() +
                                  scale_y_continuous(labels = comma) + 
                                  theme(legend.position = "bottom") + 
                                  labs(title=paste0("P_50 ", input$overviewVar, " by province and file, ", input$overviewYear), 
                                       fill = NULL,
                                       subtitle = paste0("Class: ", input$overviewClass, "; Age: ", input$overviewAge, "; Sex: ", input$overviewSex))
            girafe(ggobj = overviewP50Plot, width_svg = 5, height_svg = 5)})
  })
  
  output$overviewData <- renderDataTable({
    input$overviewUpdate
    isolate({overviewDataTable <- overviewDataFile()
            isNum <- sapply(overviewDataTable, is.numeric)
            overviewDataTable[isNum] <- format(overviewDataTable[isNum], big.mark = ",")
            DT::datatable(subset(overviewDataTable[order(overviewDataTable$PROV_RES),], 
                                 select = c("PROV_RES", "FILE","N", "MEAN", "NWGT", "SUM", "P25", "P75", 
                                             "P_0", "P_10", "P_20", "P_30", "P_40", "P_50", "P_60", "P_70", "P_80", "P_90", "P_100")), 
                          options=list(paging = F, searching=F, scrollX = TRUE, scrollY = "400px"), rownames=F)})
  })
  
### Percentiles page
  
  # Create data file
  percentilesDataFile <- reactive({
    full_percent_data <- do.call("rbind", lapply(input$percentileFiles, get))
    percent_data <- subset(full_percent_data,
                           select = c("YEAR", "FILE", "VAR", "CLASS_FLAG", "AGE", "SEX", "PROV_RES", "WITH_VAL_0",
                                      "P_0", "P_10", "P_20", "P_30", "P_40", "P_50", "P_60", "P_70", "P_80", "P_90", "P_100"),
                           YEAR == input$percentileYear &
                             VAR == input$percentileVar &
                             CLASS_FLAG == input$percentileClass &
                             AGE == input$percentileAge &
                             SEX == input$percentileSex &
                             PROV_RES == input$percentileProv)
    percent_data_t <- reshape2::melt(percent_data, id = c("YEAR", "FILE", "VAR", "CLASS_FLAG", "AGE", "SEX", "PROV_RES", "WITH_VAL_0"))
    percent_data_t <- dplyr::rename(percent_data_t, DECILE = variable,VALUE = value)
    percent_data_t$VALUE <- round(percent_data_t$VALUE, 2)
    return(percent_data_t)
  })
  
  # Create percentiles plot
  output$percentilePlot <- renderGirafe({
    input$percentileUpdate
    isolate({
      percentPlot <- ggplot(percentilesDataFile()) + 
                      geom_line(aes(x = DECILE, y = VALUE, colour = FILE, group = FILE), size = 1.25) +
                      geom_point_interactive(aes(x = DECILE, y = VALUE, colour = FILE, 
                                                 tooltip = paste0(FILE, ": ", format(VALUE, big.mark = ","))), size = 1.75) +
                      theme_classic() + 
                      scale_colour_jama() +
                      scale_y_continuous(labels = comma) + 
                      theme(legend.position = "bottom") + 
                      labs(title=paste0("Percentiles of ", input$percentileVar, " by file, ", input$percentileYear), 
                           fill = NULL,
                           x = "Percentiles",
                           y = input$percentileVar,
                           subtitle = paste0("Province: ", input$percentileProv, "; Class: ", input$percentileClass, 
                                             "; Age: ", input$percentileAge, "; Sex: ", input$percentileSex))
      girafe(ggobj = percentPlot, width_svg = 15, height_svg = 5)
    })
  })
  
  output$percentileData <- renderDataTable({
    input$percentileUpdate
    isolate({percentilesDataTable <- percentilesDataFile()
              percentilesDataTable$VALUE <- format(percentilesDataTable$VALUE, big.mark = ",")
              DT::datatable(subset(percentilesDataTable[order(percentilesDataTable$PROV_RES),], 
                                   select = c("PROV_RES", "FILE", "DECILE", "VALUE")), 
                            options=list(paging = F, searching=F, scrollX = TRUE, scrollY = "400px"), rownames=F)})
  })
  
### Demographics page
  
  
  
}

# Call the shiny function
shinyApp(ui, server)



