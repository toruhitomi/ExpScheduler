#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
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

  # List the first level callModules here
  output$legendPlot <- renderPlot({
    print(
      data.frame(
        front.name = c("板坂", "岡田", "小路", "戸村", "吉田", "その他")
      ) %>%
        ggplot(aes(x = front.name)) +
        geom_bar(aes(y = 1, fill = front.name), stat = "identity", width = 0.5) +
        geom_label(aes(label = front.name, y = 0.5), family = "Hiragino Kaku Gothic Pro W6") +
        labs(x = NULL, y = NULL) +
        scale_x_discrete(breaks = NULL) +
        scale_y_continuous(breaks = NULL) +
        scale_fill_manual(values = rev(c("#8A2BE2", "orange", "green3", "royalblue1", "#EE6363", "grey"))) +
        theme_void(base_family = "Hiragino Kaku Gothic Pro W6", base_size = 15) +
        theme(legend.position = "none", axis.text.x = element_blank()) +
        coord_flip()
    )
  })

  output$calendar <- renderFullcalendar({
    fullcalendar(exp.list[, c("title", "start", "end", "color")])
  })

  output$expTable <- DT::renderDataTable({
    DT::datatable(data = exp.list %>%
                    dplyr::filter(end > Sys.Date()) %>%
                    mutate(開始 = as.Date(開始, tz = "Japan", origin = "1899-12-30"),
                             終了 = as.Date(終了, tz = "Japan", origin = "1899-12-30")) %>%
                    .[, c("title", "フロント名", "開始", "終了", "日数",  "Device")])
  })

  output$deviceCalendar <- renderFullcalendar({
    exp.list %>%
      dplyr::mutate(title = paste(プロジェクト名, Device),
                    start,
                    end) %>%
      dplyr::mutate(color = case_when(
        # fNIRS only
        デバイス == "HOT-2000" & is.na(デバイス2) ~ "royalblue1",
        デバイス == "HOT-1000" & is.na(デバイス2) ~ "lightblue",
        デバイス == "WOT-HS" & is.na(デバイス2) ~ "grey",
        デバイス == "WOT-220" & is.na(デバイス2) ~ "blue",

        # Multi-modal (fNIRS + Plux)
        デバイス == "HOT-2000" & デバイス2 == "Plux" ~ "#66CD00",
        デバイス == "HOT-1000" & デバイス2 == "Plux" ~ "#00EE00",
        デバイス == "WOT-HS" & デバイス2 == "Plux" ~ "#8B864E",
        デバイス == "WOT-220" & デバイス2 == "Plux" ~ "#008B45",

        # fNIRS + Pupilizer
        デバイス == "HOT-2000" & デバイス2 == "Pupilizer" ~ "#FF7F24",
        デバイス == "HOT-1000" & デバイス2 == "Pupilizer" ~ "#FFB90F",
        デバイス == "WOT-HS" & デバイス2 == "Pupilizer" ~ "#8B4500",
        デバイス == "WOT-220" & デバイス2 == "Pupilizer" ~ "#FFD700",

        TRUE ~ "grey"
      )) %>%
      # dplyr::select(title, start, end, color) %>%
      fullcalendar()
  })

  output$devicePlot <- renderPlot({
    if (input$data_choice == "すべて") {
      plot.data <- dev.list
    } else if (input$data_choice == "fNIRS") {
      plot.data <- dev.list %>%
        dplyr::filter(Device %in% c("HOT-2000", "HOT-1000", "WOT-HS", "WOT-220", "WOT-100"))
    } else if (input$data_choice == "Plux") {
      plot.data <- dev.list %>%
        dplyr::filter(Device %in% c("Plux"))
    } else if (input$data_choice == "視線") {
      plot.data <- dev.list %>%
        dplyr::filter(Device %in% c("Pupil Invisible", "Pupilizer"))
    }
    plot.data %>%
      merge.data.frame(df.maxN) %>%
      dplyr::filter(Date >= input$selected_date[1] & Date <= input$selected_date[2]) %>%
      ggplot(aes(x = Date, y = nUse, color = Device)) +
      geom_hline(yintercept = 0) +
      geom_hline(aes(yintercept = maxN), color = "red", lty = "dashed") +
      geom_line(size = 1.2) +
      scale_x_continuous(breaks = seq(from = input$selected_date[1], to = input$selected_date[2], by = 1),
                         minor_breaks = NULL) +
      theme_bw(base_family = "Hiragino Kaku Gothic Pro W6", base_size = 20) +
      theme(legend.position = "none") +
      facet_wrap(~ Device, nrow = 1) +
      coord_flip()
  }, height = "auto")
}
