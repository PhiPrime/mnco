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
library(shinyjs)
library(stringi)
########################     FULL SHINY FUNCTION     ########################

#Backwards initialization
rmdfiles <- c("centerOverview.Rmd")
sapply(rmdfiles, knit, quiet = T)

CO_UI <- function() {
  
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
          actionButton("s_display_toggle", "Save")
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
          div(id = "display_report",
              verticalLayout(
                actionButton("r_button", "Generate Report")
              )    
          ),
          
          # Save Section
          hidden(
            div(id = "display_save",
              verticalLayout(
                actionButton("save_button", "Save All Data"),
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
    
    # Sends input students on vacation
    observeEvent(input$v_input_button, {
      #Gather the input
      str_list <- str_split_1(input$v_input_field, "\n")
      for (x in str_list) {
        sendOnVacation(x)
        message("Sent ", x, " on vacation")
      }
    })
    
    # Returns input students from vacation
    observeEvent(input$v_return_button, {
      #Gather the input
      str_list <- str_split_1(input$v_input_field, "\n")
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
    observeEvent(input$report_button, {
      #Not exactly sure what should go here but, like, its cool I guess
    })
  }
  #run the gadget
  runGadget(ui, server)
}

##############################     RUN UI     ###############################
##Had to comment it out since sourcing this file will run the function
#CO_UI()
