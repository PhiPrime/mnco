#
# UI Skeleton
# Currently just functions as a proof of concept
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
          #Save Section
          verticalLayout(
            #Save Button
            actionButton("save_button", "Save")
          ),
          
          #Attendance Section
          verticalLayout(
            actionButton("a_check_button", "Attendance Check"),
            actionButton("v_check_button", "Vacation Check")
          ),
          
          verticalLayout(
            actionButton("kablize_button", "Funny Kablize Button")
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
    
    #Run attendanceCheck
    observeEvent(input$a_check_button, {
      attendanceCheck()
      
      #For debug use
      print("Attendance check pressed")
    })
    
    #Run getStudentsOnVacation
    observeEvent(input$v_check_button, {
      getStudentsOnVacation()
      
      #For debug use
      print("Attendance check pressed")
    })
    
    #Main display button
    #Essentially just runs the markdown file
    #Does not display the text, only the data
    observeEvent(input$kablize_button, {
      #sink output into a file
      sink("button_test.pdf")
      
      #update and save section
      getCenterData()
      saveCenterData(silent = T)
      
      #new deck section
      print("The following students likely need a new deck made\n")
      needsNewDeck()
      kablize(needsNewDeck())
      
      #attendance check: allowable days set to 5
      stus <- attendanceCheck(5)
      
      #display data
      print("The following students have not been here in the past\n")
      stus
      kablize(stus)

      #End output sink
      sink()
      
      #If not prematurely done, send success to console
      print("Successfully made pdf")
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
