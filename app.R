library(shiny)
library(shinyalert)
library(DT)
library(odbc)
library(DBI)
library(config)
library(uuid)
library(tidyverse)

# db connection -----------------------------------------------------------

#look at config.yml file for db connection either local or in production 
dw <- config::get('dataconnection') 
#wyhen in shinyapps.io in production, select shinyapps, otherwise commentout
Sys.setenv(R_CONFIG_ACTIVE = 'shinyapps') # default or shinyapps. see config.¥ml

#connect to aws server
con <- DBI::dbConnect(odbc::odbc(),
                      Driver = dw$driver,
                      Database = dw$database,
                      UID = dw$uid,
                      PWD = dw$pwd,
                      Server = dw$server,
                      Port = dw$port)

#check tables in con database
# dbListTables(con)


# shiny -------------------------------------------------------------------
shinyApp(
  
# ui ----------------------------------------------------------------------
  ui = fluidPage(
    shinyjs::useShinyjs(),
    
    titlePanel('Short Cut Key Application'),
    helpText('List of short cut key for frequent use, such as keys for IDEs'),
    br(),
    
    #operation button settings
    fluidRow(
      column(12,
             actionButton("add_button_sck", "Add", icon("plus")),
             actionButton("edit_button_sck", "Edit", icon("edit")),
             actionButton("copy_button_sck", "Copy", icon("copy")),
             actionButton("delete_button_sck", "Delete", icon("trash-alt")),
             actionButton("fetch_button_sck", "Fetch", icon("sync-alt")))
    ),
    br(),
    
    #datatable
    fluidRow(
      column(12, #table width
            dataTableOutput('sck_table'))
    ),
    
    #shiny alert setting
    useShinyalert(),
    
    #save, csv download button
    fluidRow(
      column(12,
             actionButton('save_button_sck', 'Save', icon('save')),
             downloadButton('download_button_sck', 'Download', icon('download')))
    )
    
  ), #ui


# server ------------------------------------------------------------------
  server = function(input, session, output) {
    
    #enable shiny alert
    useShinyalert()
    
    #make sck_df reactive on the user event
    sck_df <- reactiveValues()
    
    #load table
    sck_df$data <- dbReadTable(con,'sctky')
    

# form prep ---------------------------------------------------------------

    #Form function for data entry, to be called in both 'add' and 'edit' event
    #button_id_sck is a variable and button id which will navigate to each event
    entry_form_sck <- function(button_id_sck) {
      
      showModal(
        modalDialog(
          title = strong("Entry Form"),
          fluidRow(
            column(6,
                   textInput("IDE", "IDE", placeholder = ""),
                   textInput("Library", "Library", placeholder = ""),
                   textInput("Key", "Key", placeholder = ""),
                   textInput("Ops", "Ops", placeholder = ""),
            )
          ),
          footer = tagList(
            actionButton(button_id_sck, "Submit"),
            modalButton("Cancel")
          ),
          size = "m", #window size. m:medium
          easyClose = TRUE
        )
      )
      
    }
    
    #Save function for form data into data frame format from form function
    formData_sck <- reactive({
      
      formData_sck <- data.frame(uuid_sck = UUIDgenerate(),
                                 IDE = input$IDE,
                                 Library = input$Library,
                                 Key = input$Key,
                                 Ops = input$Ops
      ) %>%
        mutate_each(funs(as.character), c(uuid_sck,IDE, Library, Key, Ops)) #match variable type to original database
      
      #paste0(str(formData))
      return(formData_sck)
      
    })
    
    # Add ---------------------------------------------------------------------
    
    #Add data button
    observeEvent(input$add_button_sck, {
      entry_form_sck("submit_new_sck")
    })
    observeEvent(input$submit_new_sck, {
      sck_df$data <- bind_rows(sck_df$data, formData_sck()) #add new record at bottom
      shinyjs::reset("entry_form_sck")
      removeModal()
    })  
    
    
    # Delete ------------------------------------------------------------------
    
    observeEvent(input$delete_button_sck, {
      showModal(
        if(length(input$sck_table_rows_selected)>=1) {
          modalDialog(
            h3(strong("Warning")),
            paste("Are you sure you would like to delete selected row(s)?"),
            footer = tagList(
              actionButton("submit_delete_sck", "Yes"),
              modalButton("Cancel")
            ),
            easyClose = TRUE
          )
        } else if(length(input$sck_table_rows_selected)<1) {
          modalDialog(
            h3(strong("Warning")),
            paste("Please select row(s)"),
            easyClose = TRUE
          )
        }
      )
    })
    observeEvent(input$submit_delete_sck, {
      sck_df$data <- sck_df$data[-input$sck_table_rows_selected,]
      removeModal()
    })
    
    
    # Copy --------------------------------------------------------------------
    
    observeEvent(input$copy_button_sck, {
      if(length(input$sck_table_rows_selected)>=1) {
        row_selection_sck <- sck_df$data[input$sck_table_rows_selected, "uuid_sck"] #identify uuid for the selected rows
        copy_df_sck <- sck_df$data %>% filter(uuid_sck %in% row_selection_sck) #filter by selected uuid rows
        paste(copy_df_sck)
        copy_df_sck$uuid_sck <- replicate(nrow(copy_df_sck), UUIDgenerate()) #replace with new uuid for copied rows
        sck_df$data <- bind_rows(sck_df$data, copy_df_sck) #add copy records at bottom
      } else if(length(input$sck_table_rows_selected)<1) {
        showModal(
          modalDialog(
            h3(strong("Warning")),
            paste("Please select row(s)"),
            easyClose = TRUE
          )
        )
      }
    })
    
    # Edit --------------------------------------------------------------------
    
    #modal window. update form based on a selected row
    observeEvent(input$edit_button_sck, {
      
      showModal(
        if(length(input$sck_table_rows_selected)>1) {
          modalDialog(
            h3(strong("Warning")),
            paste("Please select only one row"),
            easyClose = TRUE
          )
        } else if(length(input$sck_table_rows_selected)<1) {
          modalDialog(
            h3(strong("Warning")),
            paste("Please select a row"),
            easyClose = TRUE
          )
        }
      )
      
      if(length(input$sck_table_rows_selected)==1) {
        
        #load entry form
        entry_form_sck("submit_edit_sck")
        
        #update each value in entry form based on selected row
        updateTextInput(session, "IDE", value = sck_df$data[input$sck_table_rows_selected, "IDE"])
        updateTextInput(session, "Library", value = sck_df$data[input$sck_table_rows_selected, "Library"])
        updateTextInput(session, "Key", value = sck_df$data[input$sck_table_rows_selected, "Key"])
        updateTextInput(session, "Ops", value = sck_df$data[input$sck_table_rows_selected, "Ops"])
      }
    })
    
    #update to database
    observeEvent(input$submit_edit_sck, {
      uuid_selection_sck <- sck_df$data[input$sck_table_rows_selected, "uuid_sck"] #identify uuid for the selected row
      uuid_index_sck <- match(uuid_selection_sck, sck_df$data$uuid_sck) #get matched row index using UUID
      sck_df$data[uuid_index_sck, 2:5] <- formData_sck()[,2:5] #replace data with updated data. make sure column order matches. uuid_sck is 1st column in formData while last column in database
      shinyjs::reset("entry_form_sck")
      removeModal()
    })
    
    # Fetch -------------------------------------------------------------------
    
    observeEvent(input$fetch_button_sck, {
      showModal(
        modalDialog(
          h3(strong("Warning")),
          paste("Any unsaved event will be lost. Do you still want to refresh?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok_refresh_sck", "Yes")
          ),
          easyClose = TRUE
        )
      )
    })
    observeEvent(input$ok_refresh_sck, {
      removeModal()
      session$reload()
    })

# Render Table ------------------------------------------------------------
  
  output$sck_table <- DT::renderDataTable({
    table <- sck_df$data %>% select(-uuid_sck)
    dt <- datatable(
      table,
      selection = 'multiple',
      escape = FALSE,
      rownames = FALSE,
      options = list(
        pageLength = 20
      )
    )
  })

# Save to database --------------------------------------------------------
  
  #confirmation window
  observeEvent(input$save_button_sck, {
    showModal(
      modalDialog(
        h3(strong('Worning')),
        paste('Are you sure to save change(s) you made into database?'),
        footer = tagList(
          modalButton('Cancel'),
          actionButton('ok_save_sck', 'Yes')
        ),
        easyClose = TRUE
      )
    )
  })
  
  #save to database
  observeEvent(input$ok_save_sck, {
    dbWriteTable(con, 'sctky', sck_df$data, overwrite = T)
    removeModal()
    shinyalert(title = 'Saved to database', type = 'success')
  })
    
# Download ----------------------------------------------------------------
  output$download_button_sck <- downloadHandler(
    filename = function() {
      paste('ShortCutKey_', Sys.Date(), '.csv', sep='') #sep='' is no space between words
    },
    content = function(file) {
      write.csv(sck_df$data, file, row.names=F)
    }
  )

    
  } #server
) #shinyApp
