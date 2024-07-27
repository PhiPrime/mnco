#
# UI Skeleton
# Currently just functions as a proof of concept
#####################################
#             TO DO:                #
##Incorporate attendence functions  #
##Incorporate deck functions        #
##Incorporate pricing functions     #
##Incorporate all save functions    #
##Add output / export               #
##Add css stylesheet to format      #
##Fix the visibility toggle         #
#####################################

# Library
library(shiny)
library(miniUI)
library(shinyjs)

# Base UI Function
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
          #Update Buttons
          verticalLayout(
            actionButton("update_button", "Update"),
            
            #BROKEN: Conditional Button
            #checkboxInput("update_checkbox", "Hide/Show Update Functions", FALSE),
            
            # Contains lesser functions for display
            tabPanel("hidden_update",
                 verticalLayout(
                    actionButton("update_students_button", "Update Students"),
                    actionButton("update_accounts_button", "Update Accounts"),
                    actionButton("update_progress_button", "Update Progress"),
                    actionButton("update_enrollments_button", "Update Enrollments")
                 )
            )
        
          ),
        
          #Save Button
          actionButton("save_button", "Save"),
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
    
    # Runs getCenterData()
    observeEvent(input$update_button, {
      getCenterData()
      
      #For debug use
      print("Update pressed")
    })
    
    # Handlers for update functions
    observeEvent(input$update_students_button, { getStudentData() })
    observeEvent(input$update_accounts_button, { getAccountData() })
    observeEvent(input$update_progress_button, { getProgressData() })
    observeEvent(input$update_enrollments_button, { getEnrollmentData() })
    
    
    # Runs saveAllCenterData()
    observeEvent(input$save_button, {
      saveAllCenterData()
      
      #For debug use
      print("Save pressed")
    })
    
    #BROKEN: Shows hidden update functions
    #observe({
      #toggle(id = "hidden_update", condition = input$update_checkbox)
    #})
  }
  
  #run the gadget
  runGadget(ui, server)
}

# Run the UI
CO_UI()
