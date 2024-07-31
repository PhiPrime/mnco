#
# UI Skeleton
# All buttons currently broken
#####################################
#             TO DO:                #
##Incorporate vacation functions    #
###"send" and "return"              #
###Create input to accept multiple  #
###-"who" args and a date           #
##Incorporate pricing functions     #
##Incorporate all save functions    #
##Add output / export               #
##Add css stylesheet to format      #
##Fix the visibility toggle         #
#####################################

#############################     LIBRARIES     #############################
library(shiny)
library(miniUI)
library(shinyjs)
library(stringi)
########################     FULL SHINY FUNCTION     ########################
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
      
        #Layout for the function buttons
        splitLayout(
          verticalLayout(
            textAreaInput("v_input_field", "Input Student Names:", "", width = "1000px"),
            actionButton("v_input_button", "Send on vacation"),
            actionButton("v_return_button", "Return from vacation"),
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
  }
  #run the gadget
  runGadget(ui, server)
}

##############################     RUN UI     ###############################
##Had to comment it out since sourcing this file will run the function
#CO_UI()
