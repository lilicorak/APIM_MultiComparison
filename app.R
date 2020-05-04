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


# Read in the data
read_data <- function (filename, vars_select) {
  dat <- subset(fread(paste0("data/", filename, ".csv"), sep=",", data.table = FALSE), 
                select = vars_select)
  
  dat$CLASS_FLAG <- factor(dat$CLASS_FLAG, levels = c("ALL", "1", "2", "3", "4", "5"))
  dat$PROV_RES <- factor(dat$PROV_RES, levels = c("ALL", "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YT", "NT", "NU"))
  dat$SEX <- factor(dat$SEX, levels = c("ALL", "1", "2"))
  dat$AGE <- factor(dat$AGE, levels = c("All", "0-14", "15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65+", "Not Stated"))
  
  return(dat)
}

data_vars <- c("year", "FILE", "PROV_RES", "VAR", "CLASS_FLAG", "SEX", "AGE", "WITH_VAL_0", "N", "MEAN", "NWGT", "SUM", 
               "P25", "P75", "P_0", "P_10", "P_20", "P_30", "P_40", "P_50", "P_60", "P_70", "P_80", "P_90", "P_100")

percentile_vars <- c("year", "FILE", "PROV_RES", "VAR", "CLASS_FLAG", "SEX", "AGE", "WITH_VAL_0", 
                     "P_0", "P_02", "P_04", "P_06", "P_08", "P_10", "P_12", "P_14", "P_16", "P_18", "P_20", "P_22",
                     "P_24", "P_26", "P_28", "P_30", "P_32", "P_34", "P_36", "P_38", "P_40", "P_42", "P_44", "P_46",
                     "P_48", "P_50", "P_52", "P_54", "P_56", "P_58", "P_60", "P_62", "P_64", "P_66", "P_68", "P_70",
                     "P_72", "P_74", "P_76", "P_78", "P_80", "P_82", "P_84", "P_86", "P_88", "P_90", "P_92", "P_94",
                     "P_96", "P_98", "P_100")

APIM <<- rbind(read_data("APIM_tab_2017", data_vars), read_data("APIM_tab_2018", data_vars))
CHS <<- rbind(read_data("CHS_tab_2017", data_vars), read_data("CHS_tab_2018", data_vars))
CIS <<- rbind(read_data("CIS_tab_2017"), read_data("CIS_tab_2018", data_vars))
CISPlus <<- rbind(read_data("CISPlus_tab_2017", data_vars), read_data("CISPlus_tab_2018", data_vars))
SHS <<- rbind(read_data("SHS_tab_2017", data_vars), read_data("SHS_tab_2018", data_vars))

APIM_percentiles_2017 <<- read_data("APIM_percentiles_2017", percentile_vars)
APIM_percentiles_2018 <<- read_data("APIM_percentiles_2018", percentile_vars)

CIS_percentiles_2017 <<- read_data("CIS_percentiles_2017", percentile_vars)
CIS_percentiles_2018 <<- read_data("CIS_percentiles_2018", percentile_vars)

CISPlus_percentiles_2017 <<- read_data("CISPlus_percentiles_2017", percentile_vars)
CISPlus_percentiles_2018 <<- read_data("CISPlus_percentiles_2018", percentile_vars)

CHS_percentiles_2017 <<- read_data("CHS_percentiles_2017", percentile_vars)
CHS_percentiles_2018 <<- read_data("CHS_percentiles_2018", percentile_vars)

SHS_percentiles_2017 <<- read_data("SHS_percentiles_2017", percentile_vars)
SHS_percentiles_2018 <<- read_data("SHS_percentiles_2018", percentile_vars)

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
      .box.box-info {border-top-color: #2196f3;}"
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
                             choices = unique(APIM$VAR),
                             selected = "TOTINC"),
                 selectInput("overviewClass",
                             "Class flag:",
                             choices = unique(APIM$CLASS_FLAG),
                             selected = "ALL"),
                 fluidRow(column(6, selectInput("overviewAge",
                                                "Age group:",
                                                choices = unique(APIM$AGE),
                                                selected = "All")),
                          column(6, selectInput("overviewSex",
                                                "Sex:",
                                                choices = unique(APIM$SEX),
                                                selected = "ALL"))),
                 selectInput("overviewProv",
                             "Province of residence:",
                             choices = unique(APIM$PROV_RES),
                             selected = unique(APIM$PROV_RES),
                             multiple = T),
                 actionButton("overviewUpdate",
                              label = "Update",
                              width = "100%",
                              class = "btn btn-primary btn-custom")
               ),
               mainPanel(
                 width = 10,
                 fluidRow(column(4, girafeOutput("overviewMean")),
                          column(4, girafeOutput("overviewNGWT")),
                          column(4, girafeOutput("overviewP50"))),
                 fluidRow(boxPlus(title = "Data table",
                                  closable= F,
                                  collapsible = T,
                                  collapsed = T,
                                  status = "info",
                                  width = 12,
                                  DTOutput("overviewData")))
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
                             choices = unique(APIM$VAR),
                             selected = "TOTINC"),
                 selectInput("percentileClass",
                             "Class flag:",
                             choices = unique(APIM$CLASS_FLAG),
                             selected = "ALL"),
                 fluidRow(column(6, selectInput("percentileAge",
                                                "Age group:",
                                                choices = unique(APIM$AGE),
                                                selected = "All")),
                          column(6, selectInput("percentileSex",
                                                "Sex:",
                                                choices = unique(APIM$SEX),
                                                selected = "ALL"))),
                 selectInput("percentileProv",
                             "Province of residence:",
                             choices = unique(APIM$PROV_RES),
                             selected = "ALL"),
                 actionButton("percentileUpdate",
                              label = "Update",
                              width = "100%",
                              class = "btn btn-primary btn-custom")
               ),
               mainPanel(
                 width = 10,
                 fluidRow(girafeOutput("percentilePlot")),
                 fluidRow(boxPlus(title = "Data table",
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
                             choices = unique(APIM$VAR),
                             selected = "TOTINC"),
                 selectInput("demoStat",
                             "Statistic:",
                             choices = c("N", "MEAN", "NWGT", "SUM", "P25", "P75", "P_0", "P_10", "P_20", "P_30", 
                                         "P_40", "P_50", "P_60", "P_70", "P_80", "P_90", "P_100"),
                             selected = "MEAN"),
                 selectInput("demoClass",
                             "Class flag:",
                             choices = unique(APIM$CLASS_FLAG),
                             selected = "ALL"),
                 fluidRow(column(6, selectInput("demoge",
                                                "Age group:",
                                                choices = unique(APIM$AGE),
                                                selected = "All")),
                          column(6, selectInput("demoSex",
                                                "Sex:",
                                                choices = unique(APIM$SEX),
                                                selected = "ALL"))),
                 selectInput("demoProv",
                             "Province of residence:",
                             choices = unique(APIM$PROV_RES),
                             selected = "ALL"),
                 actionButton("demoUpdate",
                              label = "Update",
                              width = "100%",
                              class = "btn btn-primary btn-custom")
                 ),
               mainPanel(
                 width = 10,
                 fluidRow(
                   column(6, wellPanel(p("prov plot"),
                                       plotOutput("demoProvPlot"))),
                   column(6, wellPanel(p("age plot"),
                                       plotOutput("demoAgePlot")))
                 ),
                 fluidRow(
                   column(4, wellPanel(p("year plot"),
                                       plotOutput("demoYearPlot"))),
                   column(4, wellPanel(p("sex plot"),
                                       plotOutput("demoSexPlot"))),
                   column(4, wellPanel(p("class plot"),
                                       plotOutput("demoClassPlot")))
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
  overviewDataFile <- reactive({
    overview <- do.call("rbind", lapply(input$overviewFiles, get))
    overview <- subset(overview, year == input$overviewYear &
                         VAR == input$overviewVar &
                         CLASS_FLAG == input$overviewClass &
                         AGE == input$overviewAge &
                         SEX == input$overviewSex &
                         PROV_RES %in% input$overviewProv)
    return(overview)
  })
  
  output$overviewMean <- renderGirafe({
    input$overviewUpdate
    isolate({overviewMeanPlot <- ggplot(overviewDataFile()) + 
                                  geom_col_interactive(aes(x=PROV_RES, y= MEAN, fill = FILE, tooltip=paste0(FILE, ": ", MEAN)), width=0.8, position = "dodge") + 
                                  theme_classic() + 
                                  scale_fill_jama() +
                                  scale_y_continuous(labels = comma) + 
                                  theme(legend.position = "bottom") + 
                                  labs(title=paste0("Mean ", input$overviewVar, " by province and file, ", input$overviewYear), 
                                       fill = NULL,
                                       subtitle = paste0("Class: ", input$overviewClass, "; Age: ", input$overviewAge, "; Sex: ", input$overviewSex))
            girafe(ggobj = overviewMeanPlot, width_svg = 7, height_svg = 6)})
  })
  
  output$overviewNGWT <- renderGirafe({
    input$overviewUpdate
    isolate({overviewNWGTPlot <- ggplot(overviewDataFile()) + 
                                  geom_col_interactive(aes(x=PROV_RES, y= NWGT, fill = FILE, tooltip=paste0(FILE, ": ", NWGT)), width=0.8, position = "dodge") + 
                                  theme_classic() + 
                                  scale_fill_jama() +
                                  scale_y_continuous(labels = comma) + 
                                  theme(legend.position = "bottom") + 
                                  labs(title=paste0("NWGT ", input$overviewVar, " by province and file, ", input$overviewYear), 
                                       fill = NULL,
                                       subtitle = paste0("Class: ", input$overviewClass, "; Age: ", input$overviewAge, "; Sex: ", input$overviewSex))
            girafe(ggobj = overviewNWGTPlot, width_svg = 7, height_svg = 6)})
  })

  output$overviewP50 <- renderGirafe({
    input$overviewUpdate
    isolate({overviewP50Plot <- ggplot(overviewDataFile()) + 
                                  geom_col_interactive(aes(x=PROV_RES, y= P_50, fill = FILE, tooltip=paste0(FILE, ": ", P_50)), width=0.8, position = "dodge") + 
                                  theme_classic() + 
                                  scale_fill_jama() +
                                  scale_y_continuous(labels = comma) + 
                                  theme(legend.position = "bottom") + 
                                  labs(title=paste0("P_50 ", input$overviewVar, " by province and file, ", input$overviewYear), 
                                       fill = NULL,
                                       subtitle = paste0("Class: ", input$overviewClass, "; Age: ", input$overviewAge, "; Sex: ", input$overviewSex))
            girafe(ggobj = overviewP50Plot, width_svg = 7, height_svg = 6)})
  })
  
  output$overviewData <- renderDataTable({
    input$overviewUpdate
    isolate({DT::datatable(subset(overviewDataFile()[order(overviewDataFile()$PROV_RES),], select = c("PROV_RES", "FILE","N", "MEAN", "NWGT", "SUM", "P25", "P75", 
                                          "P_0", "P_10", "P_20", "P_30", "P_40", "P_50", "P_60", "P_70", "P_80", "P_90", "P_100")), 
                            options=list(paging = F, searching=F, scrollX = TRUE, scrollY = "400px"), rownames=F)})
  })
  
### Percentiles page
  
  # Create data file
  percentilesDataFile <- reactive({
    percentile_files <- c()
    for (f in input$percentileFiles) {
      percentile_files <- c(percentile_files, paste0(f, "_percentiles_", input$percentileYear))
    }
    full_percent_data <- do.call("rbind", lapply(percentile_files, get))
    percent_data <- subset(full_percent_data,
                               VAR == input$percentileVar &
                               CLASS_FLAG == input$percentileClass &
                               AGE == input$percentileAge &
                               SEX == input$percentileSex &
                               PROV_RES == input$percentileProv)
    percent_data_t <- reshape2::melt(percent_data, id = c("year", "FILE", "VAR", "CLASS_FLAG", "AGE", "SEX", "PROV_RES", "WITH_VAL_0"))
    return(percent_data_t)
  })
  
  # Create percentiles plot
  output$percentilePlot <- renderGirafe({
    input$percentileUpdate
    isolate({
      percentPlot <- ggplot(percentilesDataFile()) + 
                      geom_line(aes(x = variable, y = value, colour = FILE, group = FILE), size = 1.25) +
                      geom_point_interactive(aes(x = variable, y = value, colour = FILE, tooltip = paste0(FILE, " ", variable, ": ", round(value, 0.2))), size = 0.5) +
                      theme_classic() + 
                      scale_colour_jama() +
                      scale_y_continuous(labels = comma) + 
                      theme(legend.position = "bottom", axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
                      labs(title=paste0("Percentiles of ", input$percentileVar, " by file, ", input$percentileYear), 
                           fill = NULL,
                           x = "Percentiles",
                           y = input$percentileVar,
                           subtitle = paste0("Province: ", input$percentileProv, "; Class: ", input$percentileClass, 
                                             "; Age: ", input$percentileAge, "; Sex: ", input$percentileSex))
      girafe(ggobj = percentPlot, width_svg = 18, height_svg = 6)
    })
  })
  
  output$percentileData <- renderDataTable({
    input$percentileUpdate
    isolate({DT::datatable(subset(percentilesDataFile()[order(percentilesDataFile()$PROV_RES),], 
                                  select = c("PROV_RES", "FILE", "variable", "value"),
                                  variable %in% c("P_0", "P_10", "P_20", "P_30", "P_40", "P_50", "P_60", "P_70", "P_80", "P_90", "P_100")), 
                           options=list(paging = F, searching=F, scrollX = TRUE, scrollY = "400px"), rownames=F)})
  })
  
### Demographics page
  
  
  
}

# Call the shiny function
shinyApp(ui, server)



