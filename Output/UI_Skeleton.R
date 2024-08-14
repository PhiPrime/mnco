#
# UI Skeleton
#
#####################################
#             TO DO:                #
# Add css stylesheet to format      #
# Add deck suppression section      #
# Unify display buttons             #
#####################################

#############################     LIBRARIES     #############################
library(knitr)
library(shiny)
library(miniUI)
library(mnco)
library(shinyjs)
library(stringi)
########################     FULL SHINY FUNCTION     ########################

CO_UI <- function() {

  #checks if centerOverviewSettings.dat exists
  #creates file if not
  if (!file.exists(get_file_name()))
    initialize_settings_file()

  #Generate the UI
  #Args are contained as though they are braces
  ui <- fluidPage(
    mainPanel (
      verticalLayout(

        #Allow for shinyjs
        useShinyjs(),

        #Title Bar
        gadgetTitleBar("Center\nOverview", NULL, NULL),

        #Display buttons
        splitLayout(
          actionButton("r_display_toggle", "Report"),
          actionButton("v_display_toggle", "Vacation"),
          actionButton("m_display_toggle", "Map"),
          actionButton("s_display_toggle", "Save"),
          actionButton("settings_display_toggle", "Settings")
        ),

        #Layout for the function buttons
        span(
          id = "App_Ctrl_Display",

          # Vacation section
          hidden(
            div(id = "display_vacation",
                verticalLayout(
                  textAreaInput("v_input_field", "Input Student Names:", "", width = "1000px"),
                  actionButton("v_input_button", "Send on vacation"),
                  actionButton("v_return_button", "Return from vacation"),
                )
            )
          ),

          # Map Section
          hidden(
            div(id = "display_map",
                verticalLayout(
                  actionButton("map_button", "Make map")
                )
            )
          ),

          # Report section
          hidden(
            div(id = "display_report",
                splitLayout(
                  actionButton("r_button", "Generate Report"),
                  actionButton("rank_button", "Generate Ranking")
                )
            )
          ),
          # Save Section
          hidden(
            div(id = "display_save",
                verticalLayout(
                  actionButton("save_button", "Save All Data"),
                )
            )
          ),
          #Settings Section
          hidden(
            div(id = "display_settings",
                verticalLayout(
                  splitLayout(
                    actionButton("assessment_settings", "Assessments"),
                    actionButton("attendance_settings", "Attendance"),
                    actionButton("deck_settings", "Deck"),
                  ),
                  splitLayout(
                    actionButton("pricing_settings", "Pricing"),
                    actionButton("contract_settings", "Contracts"),
                    actionButton("settings_reset", "Reset Settings")
                  ),
                  hidden(
                    div(id = "assessment_settings_panel",
                    dateInput(
                      "assessment_date_input",
                      "Recent Assessments Date:",
                      value = retrieve_variable("Recent_Assessments_Date", FALSE),
                      format = "m_d_yyyy"
                    ),
                    actionButton("assessment_day_submission", "Submit")
                    )
                  ),
                  hidden(
                    div(id = "attendance_settings_panel",
                      numericInput("attendance_allowed_days_input",
                                   "Allowable Days For Attendance: ",
                                   value = retrieve_variable("Attendance_Allowed_Days", TRUE),
                                   min = 0,
                                   step = 1
                                   ),
                      actionButton("attendance_allowed_days_submission", "Submit")
                    )
                  ),
                  hidden(
                    div(id = "deck_settings_panel",
                          verticalLayout(
                            numericInput("deck_minimum_input",
                                         "Deck Minimum: ",
                                         value = retrieve_variable("Deck_Minimum_Threshold", TRUE),
                                         min = 0,
                                         step = 1
                            ),
                            numericInput("deck_warning_input",
                                         "Deck Warning Duration: ",
                                         value = retrieve_variable("Deck_Warning_Duration", TRUE),
                                         min = 0,
                                         step = 1
                            ),
                            numericInput("deck_suppression_max_input",
                                         "Deck Suppression Maximum Time: ",
                                         value = retrieve_variable("Deck_Suppression_Maximum_Time", TRUE),
                                         min = 0,
                                         step = 1
                            ),
                            actionButton("deck_submission", "Submit")
                          )
                        )
                  ),
                  hidden(
                    div(id = "price_settings_panel",
                      verticalLayout(
                        numericInput("price_upper_bound_input",
                                     "Price Upper Bound: ",
                                     value = retrieve_variable("Price_Upper_Bound", TRUE),
                                     min = 0,
                                     step = 1
                        ),
                        numericInput("price_lower_bound_input",
                                     "Price Lower Bound: ",
                                     value = retrieve_variable("Price_Lower_Bound", TRUE),
                                     min = 0,
                                     step = 1
                        ),
                        numericInput("student_no_upper_bound_input",
                                     "Students Upper Bound: ",
                                     value = retrieve_variable("Student_Upper_Bound", TRUE),
                                     min = 0,
                                     step = 1
                        ),
                        numericInput("student_no_lower_bound_input",
                                     "Students Lower Bound: ",
                                     value = retrieve_variable("Student_Lower_Bound", TRUE),
                                     min = 0,
                                     step = 1
                        ),
                        actionButton("price_submission", "Submit")
                      )
                    )
                  ),
                  hidden(
                    div(id = "contract_settings_panel",
                        verticalLayout(
                          numericInput("contract_length_input",
                                       "Standard Contract Length: ",
                                       value = retrieve_variable("Standard_Contract_Length", TRUE),
                                       min = 0,
                                       step = 1
                          ),
                          numericInput("no_sessions_input",
                                       "Standard Number of Sessions: ",
                                       value = retrieve_variable("Standard_Number_Sessions", TRUE),
                                       min = 0,
                                       step = 1
                          ),
                          numericInput("length_modifer_input",
                                       "Length Modifier: ",
                                       value = retrieve_variable("Length_Modifier", TRUE),
                                       min = 0,
                                       step = 1
                          ),
                          numericInput("contract_lower_bound_input",
                                       "Contract Adjustment Lower Bound: ",
                                       value = retrieve_variable("Contract_Adjustment_Lower_Bound", TRUE),
                                       step = 1
                          ),
                          numericInput("contract_upper_bound_input",
                                       "Contract Adjustment Upper Bound: ",
                                       value = retrieve_variable("Contract_Adjustment_Upper_Bound", TRUE),
                                       step = 1
                          ),
                          numericInput("contract_length_upper_bound_input",
                                       "Standard Contract Upper Bound: ",
                                       value = retrieve_variable("Contract_Length_Upper_Bound", TRUE),
                                       min = 0,
                                       step = 1
                          ),
                          numericInput("contract_length_lower_bound_input",
                                       "Standard Contract Lower Bound: ",
                                       value = retrieve_variable("Contract_Length_Lower_Bound", TRUE),
                                       min = 0,
                                       step = 1
                          ),
                          actionButton("contract_submission", "Submit")
                        )
                    )
                  )
                )
            )
          )
        ),
        #Close the program
        actionButton("close_button", "Close",)
      ),
    )
  )

  #Generate the server for the UI
  server <- function(input, output, session) {
    # Closes the application
    observeEvent(input$close_button, {
      stopApp()
    })

    # Display Functions
    # When adding new section, add it to the
    #-"hideAll" list so it can be flushed

    # Hide All display
    hideAll <- function() {
      hide("display_report")
      hide("display_vacation")
      hide("display_map")
      hide("display_save")
      hide("display_settings")
      hide("assessment_settings_panel")
      hide("attendance_settings_panel")
      hide("deck_settings_panel")
      hide("price_settings_panel")
      hide("contract_settings_panel")
    }

    # Toggles vacation display
    observeEvent(input$r_display_toggle, {
      hideAll()
      toggle("display_report")
    })

    # Toggles vacation display
    observeEvent(input$v_display_toggle, {
      hideAll()
      toggle("display_vacation")
    })

    # Toggles the map display
    observeEvent(input$m_display_toggle, {
      hideAll()
      toggle("display_map")
    })

    # Toggles the save display
    observeEvent(input$s_display_toggle, {
      hideAll()
      toggle("display_save")
    })

    # Toggles the save display
    observeEvent(input$settings_display_toggle, {
      hideAll()
      toggle("display_settings")
    })

    #
    # Secondary Display Settings
    #

    observeEvent(input$assessment_settings, {
      hideAll()
      toggle("display_settings")
      toggle("assessment_settings_panel")
    })

    observeEvent(input$attendance_settings, {
      hideAll()
      toggle("display_settings")
      toggle("attendance_settings_panel")
    })

    observeEvent(input$deck_settings, {
      hideAll()
      toggle("display_settings")
      toggle("deck_settings_panel")
    })

    observeEvent(input$pricing_settings, {
      hideAll()
      toggle("display_settings")
      toggle("price_settings_panel")
    })

    observeEvent(input$contract_settings, {
      hideAll()
      toggle("display_settings")
      toggle("contract_settings_panel")
    })

    #
    # Settings Inputs
    #

    observeEvent(input$settings_reset, {
      #Resets the settings file
      initialize_settings_file()
    })

    observeEvent(input$assessment_day_submission, {
      #Gather the date, FORMAT ISSUE UNRESOLVED
      edit_variable("Recent_Assessments_Date", input$assessment_date_input)
    })

    observeEvent(input$attendance_allowed_days_submission, {
      #Gather the input data
      edit_variable("Attendance_Allowed_Days", input$attendance_allowed_days_input)
    })

    observeEvent(input$deck_submission, {
      #Gather the input data
      edit_variable("Deck_Minimum_Threshold", input$deck_minimum_input)
      edit_variable("Deck_Warning_Duration", input$deck_warning_input)
      edit_variable("Deck_Suppression_Maximum_Time", input$deck_suppression_max_input)
    })

    observeEvent(input$price_submission, {
      #Gather the input data
      edit_variable("Price_Upper_Bound", input$price_upper_bound_input)
      edit_variable("Price_Lower_Bound", input$price_lower_bound_input)
      edit_variable("Student_Upper_Bound", input$student_no_upper_bound_input)
      edit_variable("Student_Lower_Bound", input$student_no_lower_bound_input)
    })

    observeEvent(input$contract_submission, {
      edit_variable("Standard_Contract_Length", input$contract_length_input)
      edit_variable("Standard_Number_Sessions", input$no_sessions_input)
      edit_variable("Length_Modifier", input$length_modifer_input)
      edit_variable("Contract_Adjustment_Lower_Bound", input$contract_lower_bound_input)
      edit_variable("Contract_Adjustment_Upper_Bound", input$contract_upper_bound_input)
      edit_variable("Contract_Length_Upper_Bound", input$contract_length_upper_bound_input)
      edit_variable("Contract_Length_Lower_Bound", input$contract_length_lower_bound_input)
    })
    #
    # Main Inputs
    #

    # Sends input students on vacation
    observeEvent(input$v_input_button, {
      #Gather the input
      str_list <- stringr::str_split_1(input$v_input_field, "\n")
      for (x in str_list) {
        sendOnVacation(x)
        message("Sent ", x, " on vacation")
      }
    })

    # Returns input students from vacation
    observeEvent(input$v_return_button, {
      #Gather the input
      str_list <- stringr::str_split_1(input$v_input_field, "\n")
      for (x in str_list) {
        returnStudentFromVacation(x)
        message("Returned ", x, " from vacation")
      }
    })

    # Generate the map
    ## BROKEN: NOT CONNECTED TO MAP FUNCTION
    observeEvent(input$map_button, {
      generateMap(TRUE)
    })

    # Save center data
    # BROKEN: NOT CONNECTED TO SAVE FUNCTION
    observeEvent(input$save_button, {
      saveAllCenterData()
    })

    # Generate the report
    observeEvent(input$r_button, {
      rmarkdown::render("centerOverview.rmd")
    })


    #Generate the rankings
    #utilize sink function
    observeEvent(input$rank_button, {
      #Get the ranking
      rank_data <- getStudentRanking()

      #initialize parameter
      no_included <- dim(rank_data)[1]

      o_file <- "rankings.html"

      fileConn <- file(o_file)

      #Generate the output here
      writeLines(
        c("<html style=\"display:table;margin:auto\"><body>
          <h1 style=\"
          text-align:center;
          border-style:solid;
          \">
          Most Productive Students<//h1>",
          "<table style=\"text-align:center;border: solid;border-collapse:collapse\">",
          "<tr style=\"border-style:solid;border-color:black\">
          <td style=\"text-align:center;border: solid\">Rank<//td>
          <td style=\"text-align:center;border: solid\">Name<//td>
          <td style=\"text-align:center;border: solid\">Predicted Mastery Checks Per Session<//td><//tr>"
        ),
        o_file
      )

      for (x in 1:no_included) {
        write(c("<tr style=\"border-style:solid;border-color:black\">
                <td style=\"font-size:",
                rank_data[x,7],"\">",
                rank_data[x,8],
                "<//td><td style=\"font-size:",
                rank_data[x,7],"\">",
                rank_data[x,2],
                "<//td><td style=\"font-size:",
                rank_data[x,7],"\">",
                rank_data[x,5],
                "<//td><//tr>"
        ), o_file, append = TRUE)
      }

      #end output
      write(c("<//table>","<//body>","<//html>"), o_file, append = TRUE)

      close(fileConn)
      message("Rankings produced")
    })
  }
  #run the gadget
  runGadget(ui, server)
}

##############################     RUN UI     ###############################
##Had to comment it out since sourcing this file will run the function
#CO_UI()
