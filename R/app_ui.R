#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  library(shiny)
  library(fullcalendar)
  library(tidyverse)
  library(magrittr)
  library(openxlsx)
  NeURon::plot_theme_set()

  exp.list <- read.xlsx("~/株式会社ＮｅＵ/こんさる\ -\ General/実験スケジュール管理表v1.xlsm",
                        sheet = 2, cols = 1:18, startRow = 3) %>%
    dplyr::mutate(
      title = paste(実験ID, プロジェクト名),
      start = as.Date(開始, tz = "Japan", origin = "1899-12-30"),
      end = as.Date(終了 + 1, tz = "Japan", origin = "1899-12-30"),
      color = ifelse(is.na(フロント名), "grey",
                     ifelse(フロント名 == "岡田", "green",
                                 ifelse(フロント名 == "吉田", "#EE6363",
                                             ifelse(フロント名 == "小路", "blue",
                                                         ifelse(フロント名 == "板坂", "purple",
                                                                     ifelse(フロント名 == "戸村", "orange", "grey"))))))
    )
  exp.list

  dev.list <- read.xlsx("~/株式会社ＮｅＵ/こんさる\ -\ General/実験スケジュール管理表v1.xlsm",
                        sheet = 2, cols = 19, skipEmptyRows = F)
  idxRowST <- which(str_detect(dev.list$Start_Date, "Deviceカレンダー用")) + 2
  idxRowED <- which(str_detect(dev.list$Start_Date, "最終表示用")) - 2
  dev.list <- read.xlsx("~/株式会社ＮｅＵ/こんさる\ -\ General/実験スケジュール管理表v1.xlsm",
                        sheet = 2, startRow = idxRowST + 2, rows = (idxRowST + 1):(idxRowST + 10)) %>%
    pivot_longer(cols = 2:(ncol(.)-1), names_to = "date", values_to = "nUse") %>%
    set_colnames(c("Device", "Date", "nUse")) %>%
    mutate(Date = as.Date(as.numeric(Date), tz = "Japan", origin = "1899-12-30"))


  df.maxN <- read.xlsx("~/株式会社ＮｅＵ/こんさる\ -\ General/実験スケジュール管理表v1.xlsm",
                       sheet = "リスト", cols = 5:6) %>%
    set_colnames(c("Device", "maxN"))

  navbarPage("NeU Experiment Scheduler",
             tabPanel("実験",
                      sidebarLayout(
                        sidebarPanel(
                          h2("凡例"),
                          plotOutput("legendPlot"),
                          width = 2
                        ),
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("カレンダー", fullcalendarOutput("calendar", width = "95%")),
                                      tabPanel("テーブル", DT::dataTableOutput("expTable"))),
                          width = 10
                        )
                      )
             ),
             tabPanel("機材",
                      fluidRow(
                        tabsetPanel(type = "tabs",
                                    tabPanel("カレンダー", fullcalendarOutput("deviceCalendar", width = "90%")),
                                    tabPanel("プロット",
                                             wellPanel(sliderInput(inputId = "selected_date", label = "プロットを表示する期間",
                                                                   min = min(dev.list$Date), max = max(dev.list$Date), value = c(Sys.Date(), Sys.Date() + 30))),
                                             sidebarPanel(
                                               radioButtons("data_choice", label = "表示するデータを選択",
                                                            choices = c("fNIRS", "Plux", "視線", "すべて"),
                                                            selected = "すべて")
                                             ),
                                             mainPanel(div(plotOutput("devicePlot", height = "80%"), style = "height: 100vh")))

                        )
                      ))

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'ExpSchedulerApp'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

