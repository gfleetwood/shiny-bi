server <- function(input, output, session) {

  get_report <- function(rname){
    
    rep_id = reports %>% 
      dplyr::filter(report_name == rname) %>% 
      pull(report_id)
    
    uid <- as.character(reactiveValuesToList(res_auth)$user_id)
    
    user_group_ids <- user_groups %>% 
      dplyr::filter(user_id == uid) %>% 
      pull(user_group_id)
    
    user_reports <- user_groups_reports_access %>% 
      dplyr::filter(user_group_id %in% user_group_ids) %>% 
      dplyr::filter(report_id == rep_id)
    
    if(nrow(user_reports) == 0) return(tagList(verbatimTextOutput("permissions_msg")))
    
    switch(
      rep_id, 
      "5" = r1, 
      "6" = r2
      )

  }

  res_auth <- secure_server(check_credentials = check_credentials(users))
  output$auth_output <- renderPrint({reactiveValuesToList(res_auth)}) 
  output$permissions_msg <- renderText("You don't have access to this report.")
  output$display_report <- renderUI({get_report(input$report_choice)})
  
  output$report_one <- renderPlot({
    x  <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })

  output$report_two <- renderPlot({
    x  <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })

}

# my_custom_check_creds <- function(dbname, host, port, db_user, db_password) {
# 
#   function(user, password) {
#     
#     con <- dbConnect(dbDriver("PostgreSQL"), dbname = dbname, 
#                      host = host, port = port,
#                      user = db_user, password = db_password)
#     
#     on.exit(dbDisconnect(con))
#     
#     req <- glue_sql("SELECT * FROM my_table WHERE \"user\" = ({user}) AND \"password\" = ({password})", 
#                     user = user, password = password, .con = con
#     )
#     
#     req <- dbSendQuery(con, req)
#     res <- dbFetch(req)
#     if (nrow(res) > 0) {
#       list(result = TRUE, user_info = list(user = user, something = 123))
#     } else {
#       list(result = FALSE)
#     }
#   }
# }

