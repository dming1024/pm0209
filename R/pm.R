
#' ProjectManage
#'
#' A simple Project manage web server, designed for project manage.
#' In this web server,you can add,delete and edit the Project.
#' Yixuetongji Group developed this shiny App for his fans.
#' If you have any questions, pls feel free to contact me(dongliulou@126.com).
#' Furthermore, you can follow this guy by wechat:yixuetongji001.
#'
#' @author Dming1024
#' @import dplyr DBI DT RSQLite pool shiny shinydashboard shinyjs uuid
#' @importFrom dplyr select filter
#' @export
ProjectManage<-function(){
  pool <- pool::dbPool(RSQLite::SQLite(), dbname = "db.sqlite")
  responses_df <- data.frame(row_id = character(),
                             ProjectID = character(),
                             Description = character(),
                             SourceFrom = character(),
                             Participant = character(),
                             Status=character(),
                             dateStart = as.Date(character()),
                             dateEnd = as.Date(character()),
                             stringsAsFactors = FALSE)
  #Create responses table in sql database
  RSQLite::dbWriteTable(pool, "responses_df", responses_df, overwrite = FALSE, append = TRUE)

  #必须要输入的项目
  labelMandatory <- function(label) {
    tagList(
      label,
      span("*", class = "mandatory_star")
    )
  }
  appCSS <- ".mandatory_star { color: red; }"


  ui <- shinydashboard::dashboardPage(

    shinydashboard::dashboardHeader(title = "Project Manage",
                                    shiny::tags$li(shiny::a(href ="javascript:window.location.reload(true)",
                                                            shiny::icon("home"),
                                                            style = "cursor: pointer;"),
                                                   class = "dropdown")
    ),

    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        # Setting id makes input$tabs give the tabName of currently-selected tab
        id = "tabs",
        shinydashboard::menuItem("ProjectOverview", tabName = "dashboard", icon = shiny::icon("dashboard")),
        shinydashboard::menuItem("ProjectDetail", icon = shiny::icon("th"), tabName = "widgets", badgeLabel = "new",
                                 badgeColor = "green")
      )
    ),
    shinydashboard::dashboardBody(
      # Boxes need to be put in a row (or column)
      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "dashboard",
                                shiny::fluidRow(
                                  #box(width = 12,actionButton("count","IncrementProcess")),
                                  shinydashboard::valueBoxOutput("progressBox",width = 6),
                                  shinydashboard::valueBoxOutput("approvalBox",width = 6),
                                  shiny::column(shiny::plotOutput("summary"),width=10,offset = 1)

                                )
        ),
        shinydashboard::tabItem(tabName = "widgets",
                                shiny::fluidRow(
                                  shinyjs::useShinyjs(),
                                  shinyjs::inlineCSS(appCSS),
                                  shiny::column(DT::dataTableOutput("dataout"),width = 12,offset=0.5),
                                  shiny::column(shiny::actionButton("add_button", "Add", shiny::icon("plus")),
                                                shiny::actionButton("edit_button", "Edit",shiny::icon("edit")),
                                                #actionButton("copy_button", "Copy", icon("copy")),
                                                shiny::actionButton("view_button","View",shiny::icon("info-circle")),
                                                shiny::actionButton("delete_button", "Delete", shiny::icon("trash-alt"))
                                                ,width=11,offset=0.5)
                                )
        )
      )
    )
  )

  server <- function(input, output,session) {

    output$progressBox<-shinydashboard::renderValueBox({
      table <- responses_df() %>% dplyr::select(-row_id)
      todo_jobs=nrow(table[table$Status=="Processing",])
      #total_jobs=nrow(table)

      shinydashboard::valueBox(
        paste0(todo_jobs," needed to do"),"Progressing",icon=icon("list"),
        color="purple"
      )
    })

    output$approvalBox<-shinydashboard::renderValueBox({
      table <- responses_df() %>% dplyr::select(-row_id)
      done_jobs=nrow(table[table$Status=="Done",])
      total_jobs=nrow(table)
      #finshedRates=round((done_jobs/total_jobs)*100,2)
      shinydashboard::valueBox(
        paste0(done_jobs," jobs Completed!"),'Archived',icon=icon("thumbs-up",lib="glyphicon"),
        color="yellow"
      )
    })


    #details 视图的设置

    responses_df <- shiny::reactive({
      input$submit
      input$submit_edit
      #input$copy_button
      input$delete_button
      #把数据responses_df读出来
      RSQLite::dbReadTable(pool, "responses_df")
    })

    fieldsMandatory <- c("name", "sex")

    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)

      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })

    #录入数据的对话框
    entry_form <- function(button_id){

      shiny::showModal(
        shiny::modalDialog(
          shiny::tags$div(id=("entry_form"),
                          tags$head(tags$style(".modal-dialog{ width:400px}")), #Modify the width of the dialog
                          tags$head(tags$style(HTML(".shiny-split-layout > shiny::tags$div {overflow: visible}"))), #Necessary to show the input options
                          shiny::fluidPage(
                            shiny::fluidRow(
                              shiny::splitLayout(
                                cellWidths = c("250px", "100px"),
                                cellArgs = list(style = "vertical-align: top"),
                                textInput("ProjectID", labelMandatory("ProjectID"), placeholder = ""),
                                selectInput("Status", labelMandatory("Status"), multiple = FALSE, choices = c("Processing", "Done"),selected = "Processing")
                              ),
                              #sliderInput("age", "Age", 0, 100, 1, ticks = TRUE, width = "354px"),
                              textAreaInput("Description", "Description", placeholder = "项目描述", height = 100, width = "354px"),
                              splitLayout(
                                cellWidths = c("170px", "170px"),
                                cellArgs = list(style = "vertical-align: top"),
                                textInput("SourceFrom", labelMandatory("SourceFrom"), placeholder = ""),
                                selectInput("Participant", labelMandatory("Participant"), multiple = FALSE, choices = c("QQ", "ZY","Bio_group"))
                              ),
                              dateInput("dateEnd","End Date:"),
                              helpText(labelMandatory(""), paste("Mandatory field.")),
                              actionButton(button_id, "Submit")
                            ),
                            easyClose = TRUE
                          )
          )
        )
      )
    }

    #获取输入的数据，同时自动产生row_id，universal unique identifier
    formData <- shiny::reactive({

      #如果还是Processing的状态，就不给出dateEnd，以...代替
      dateEnd<-shiny::reactive({
        if(input$Status=="Processing"){
          dateEnd="..."
        }else{
          dateEnd=as.character(input$dateEnd)
        }
      })

      formData <- data.frame(row_id = uuid::UUIDgenerate(),
                             ProjectID = input$ProjectID,
                             Description = input$Description,
                             SourceFrom = input$SourceFrom,
                             Participant = input$Participant,
                             Status = input$Status,
                             dateStart = as.character(Sys.Date()),
                             dateEnd= dateEnd(),
                             stringsAsFactors = FALSE)
      return(formData)
    })

    #从数据库读取数据
    appendData <- function(data){
      quary <- DBI::sqlAppendTable(pool, "responses_df", data, row.names = FALSE)
      RSQLite::dbExecute(pool, quary)
    }

    #如果有add_button时间，唤醒entry_form表格
    shiny::observeEvent(input$add_button, priority = 20,{

      entry_form("submit")

      inputStatus=shiny::reactive({
        if(shiny::isTruthy(input$Status)){
          if(input$Status=="Done")
          {
            inputStatus="T"
          }else{inputStatus="F"}
        }else{inputStatus="F"}
        return(inputStatus)
      })

      observe({
        shinyjs::hide("dateEnd")
        if((inputStatus())){
          shinyjs::show("dateEnd")
        }
      })


    })


    #若果有submit事件，调取appendData，同时remove/初始化 表格数据
    shiny::observeEvent(input$submit, priority = 20,{

      appendData(formData())
      shinyjs::reset("entry_form")
      shiny::removeModal()

    })


    #从数据库中删除数据，根据response_table_rows_selected
    deleteData <- shiny::reactive({

      #数据表格在网页展示前，已发生排序的变化66666
      SQL_df <- RSQLite::dbReadTable(pool, "responses_df")%>% dplyr::arrange(by_group=Status) %>% dplyr::arrange(desc(Status))

      row_selection <- shiny::eventReactive(input$dataout_rows_selected,{
        SQL_df$row_id[c(input$dataout_rows_selected)]
      })
      #row_selection <- SQL_df[input$dataout_rows_selected, "row_id"]

      row_selections=row_selection()
      quary <- lapply(row_selections, function(nr){
        RSQLite::dbExecute(pool, sprintf('DELETE FROM "responses_df" WHERE "row_id" == ("%s")', nr))
      })
    })

    #将delet button事件与deleteData函数连接
    shiny::observeEvent(input$delete_button, priority = 20,{

      if(length(input$dataout_rows_selected)>=1 ){
        deleteData()
      }

      shiny::showModal(

        if(length(input$dataout_rows_selected) < 1 ){
          shiny::modalDialog(
            title = "Warning",
            paste("Please select row(s)." ),easyClose = TRUE
          )
        })
    })


    #在ui界面进行数据编辑,done显示dateEnd，Processing无dateEnd
    shiny::observeEvent(input$edit_button, priority = 20,{
      inputStatus=shiny::reactive({
        if(shiny::isTruthy(input$Status)){
          if(input$Status=="Done")
          {
            inputStatus="T"
          }else{inputStatus="F"}
        }else{inputStatus="F"}
        return(inputStatus)
      })

      shiny::observe({
        shinyjs::hide("dateEnd")
        if((inputStatus())){
          shinyjs::show("dateEnd")
        }
      })


      SQL_df <- RSQLite::dbReadTable(pool, "responses_df")%>% arrange(by_group=Status) %>% arrange(desc(Status))

      shiny::showModal(
        if(length(input$dataout_rows_selected) > 1 ){
          shiny::modalDialog(
            title = "Warning",
            paste("Please select only one row." ),easyClose = TRUE)
        } else if(length(input$dataout_rows_selected) < 1){
          shiny::modalDialog(
            title = "Warning",
            paste("Please select a row." ),easyClose = TRUE)
        })

      if(length(input$dataout_rows_selected) == 1 ){

        entry_form("submit_edit")

        shiny::updateTextInput(session, "ProjectID", value = SQL_df[input$dataout_rows_selected, "ProjectID"])
        shiny::updateTextAreaInput(session, "Description", value = SQL_df[input$dataout_rows_selected, "Description"])
        shiny::updateTextInput(session, "SourceFrom", value = SQL_df[input$dataout_rows_selected, "SourceFrom"])
        shiny::updateSelectInput(session, "Participant", selected = SQL_df[input$dataout_rows_selected, "Participant"])

        shiny::updateSelectInput(session, "Status", selected = SQL_df[input$dataout_rows_selected, "Status"])
        #updateSliderInput(session, "age", value = SQL_df[input$dataout_rows_selected, "age"])
        #updateDateInput(session,"dateEnd",value = SQL_df[input$dataout_rows_selected, "dateEnd"])
        #updateDateInput(session,"dateEnd",value ="...")



      }
    })

    #使用view_button，对记录的信息进行view
    shiny::observeEvent(input$view_button,priority = 20,{

      SQL_df <- RSQLite::dbReadTable(pool, "responses_df")%>% arrange(by_group=Status) %>% arrange(desc(Status))

      shiny::showModal(
        if(length(input$dataout_rows_selected) > 1 ){
          shiny::modalDialog(
            title = "Warning",
            paste("Please select only one row." ),easyClose = TRUE)
        } else if(length(input$dataout_rows_selected) < 1){
          shiny::modalDialog(
            title = "Warning",
            paste("Please select a row." ),easyClose = TRUE)
        })


      if(length(input$dataout_rows_selected) == 1 ){

        shiny::showModal(
          shiny::modalDialog(
            shiny::tags$div(id="view_form",
                            #tags$body(tags$style(HTML("view_form{ background-color: blue;}"))),

                            shiny::tags$style(HTML(".modal-dialog{ width:400px;}")), #Modify the width of the dialog
                            #tags$head(tags$style(HTML(".shiny-split-layout > shiny::tags$div {overflow: visible}"))), #Necessary to show the input options
                            shiny::fluidPage(
                              #tags$style(HTML(".container-fluid{background-color:#8DB6CD;}")),
                              shiny::fluidRow(
                                #cellWidths = c("250px", "100px"),
                                #cellArgs = list(style = "vertical-align: top"),
                                #helpText("项目名称"),
                                column(
                                  tags$style(HTML("#ProjectID_view{background-color:#8DB6CD;font-family:'arial';font-weight:bold;font-size:15px;text-align:center;}")),
                                  textOutput("ProjectID_view"),
                                  width = 12),
                                column(
                                  tags$br(),
                                  width = 12
                                ),
                                column(
                                  textOutput("summary_view"),
                                  width=12
                                )
                                #helpText(labelMandatory(""), paste("Mandatory field.")),
                                #actionButton(button_id, "Submit")
                              ),
                              easyClose = TRUE
                            )
            )
          )
        )

        output$ProjectID_view<-shiny::renderText({
          SQL_df[input$dataout_rows_selected, "ProjectID"]
        })
        output$Status_view<-shiny::renderText({
          SQL_df[input$dataout_rows_selected, "Status"]
        })
        output$Description_view<-shiny::renderText({
          SQL_df[input$dataout_rows_selected, "Description"]
        })
        output$SourceFrom_view<-shiny::renderText({
          SQL_df[input$dataout_rows_selected, "SourceFrom"]
        })
        output$Participant_view<-shiny::renderText({
          SQL_df[input$dataout_rows_selected, "Participant"]
        })
        output$dateEnd_view<-shiny::renderText({
          SQL_df[input$dataout_rows_selected, "dateEnd"]
        })



        output$summary_view<-shiny::renderText({

          project_status<-shiny::reactive({
            SQL_df[input$dataout_rows_selected, "Status"]
          })

          if(project_status()=="Done"){
            sprintf("该项目是由%s在%s提出的，主要目的是进行%s，%s已在%s完成！",
                    SQL_df[input$dataout_rows_selected, "SourceFrom"],
                    SQL_df[input$dataout_rows_selected, "dateStart"],
                    SQL_df[input$dataout_rows_selected, "Description"],
                    SQL_df[input$dataout_rows_selected, "Participant"],
                    SQL_df[input$dataout_rows_selected, "dateEnd"])
          }
          else{
            sprintf("该项目是由%s在%s提出的，主要目的是进行%s，%s正在努力进行中~",
                    SQL_df[input$dataout_rows_selected, "SourceFrom"],
                    SQL_df[input$dataout_rows_selected, "dateStart"],
                    SQL_df[input$dataout_rows_selected, "Description"],
                    SQL_df[input$dataout_rows_selected, "Participant"])
          }
        })


      }

    })



    #更新后台数据库
    shiny::observeEvent(input$submit_edit, priority = 20, {

      SQL_df <- RSQLite::dbReadTable(pool, "responses_df")%>% arrange(by_group=Status) %>% arrange(desc(Status))
      row_selection <- SQL_df[input$dataout_row_last_clicked, "row_id"]
      RSQLite::dbExecute(pool, sprintf('UPDATE "responses_df" SET "ProjectID" = ?, "Description" = ?, "SourceFrom" = ?,
                                       "Participant" = ?, "Status" = ?, "dateEnd" = ? WHERE "row_id" = ("%s")', row_selection),
                         param = list(input$ProjectID,
                                      input$Description,
                                      input$SourceFrom,
                                      input$Participant,
                                      input$Status,
                                      as.character(input$dateEnd)))
      shiny::removeModal()

    })


    output$dataout <- DT::renderDataTable({
      #https://www.displayr.com/how-to-create-customized-tables-in-displayr-using-r/
      table <- responses_df() %>% select(-row_id)
      #names(table) <- c("ProjectID", "Description", "SourceFrom", "Participant", "Status","dateStart","dateEnd")
      table <- DT::datatable(table[order(table$Status,decreasing = T),],
                             extensions = 'RowGroup',
                             rownames = FALSE,
                             options = list(
                               rowGroup=list(dataSrc=4)
                             )
      )
    })


  }

  shiny::shinyApp(ui,server)
}



#shiny::shinyApp(ui, server)
