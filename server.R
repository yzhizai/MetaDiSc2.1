source("funciones_050922.R")
library(svglite)
library(rsconnect)
library(devtools)
library(shinyvalidate)

# ----------------------------------------------------------------------------------------------- #

shinyServer(function(input, output, session) {
  
# ----------------------------------------------------------------------------------------------- #
  
  #### LANDING PAGE: ####
  
  set.seed(122)
  histdata <- rnorm(500)
  observeEvent(once = TRUE, ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = histdata, {
    # event will be called when histdata changes, which only happens once, when it is initially calculated.
    showModal(modalDialog(HTML('<h1 style="text-align: center;">Welcome to Meta-DiSc 2.0!</h1>
                                 <hr style="height:1px;border-width:0;background-color:black">
                                 <p>Meta-DiSc 2.0 is freeware software to perform Meta-analysis of studies of Diagnostic Test Accuracy</p>
                                 <ul>
                                 <li>Performs statistical pooling of sensitivities, specificities, likelihood ratios and diagnostic odds ratios using current recommendations by means of a bivariate random effects model</li>
                                 <li>It allows to explore sources of heterogeneity using subgroup and meta-regression analysis</li>
                                 <li>It computes several measures of heterogeneity to help reviewers to further describe between studies variations</li>
                                 <li>All computational algorithms have been validated through comparison with different statistical tools and published meta-analyses</li>
                                 </ul>
                                 <p>This application is the &ldquo;highly-expected&rdquo; update of the previous version of Meta-DiSc software</p>
                                 <p>The update has been led by the Clinical Biostatistics Unit of the Ramon y Cajal Research Institute (IRYCIS)</p>
                                 <p>&nbsp;</p>
                                 <button type="button" class="btn btn-default" data-dismiss="modal" style="background-color:#555555; color:white; position: relative; left:44%; font-size: 16px; padding: 12px 28px;">OK!</button>
                                 <hr style="height:1px;border-width:0;background-color:black">
                                 <table style="height: 120px; margin-left: auto; margin-right: auto;" width="100%">
                                 <tbody>
                                 <tr>
                                 <td style="width: 31%;"><img src="IRYCIS-logo.png" alt="" width="80%" height=auto /></td>
                                 <td style="width: 26%;"><img src="CIBERESP-logo.png" alt="" width="80%" height=auto /></td>
                                 <td style="width: 22%;"><img src="UCM-logo.png" alt="" width="80%" height=auto /></td>
                                 <td style="width: 22%;"><img src="UPM-logo.png" alt="" width="80%" height=auto /></td>
                                 </tr>
                                 </tbody>
                                 </table>
                                 <p>&nbsp;</p>')

    ))
  })
  
  
  # ----------------------------------------------------------------------------------------------- #
  
  #### FILE UPLOAD: ####
  
  ## The "values" variable: ##
  
  # "values" stores the state of the "file_upload" input. 
  values <- reactiveValues(
    upload_state = NULL # It is by default set to NULL.
  )
  # If the user uploads a file, its state changes (due to the "observeEvent" environment) to "uploaded".
  observeEvent(input$file_upload, {
    values$upload_state <- "uploaded"
  })
  # If the user loads the example dataset.
  observeEvent(input$ex_dataset, {
    values$upload_state <- "example"
  })
  # If the user presses the reset button, its state changes to "reset" and the "file_upload" input is is emptied.
  observeEvent(input$file_reset, {
    values$upload_state <- "reset"
    shinyjs::reset("file_upload")
  })
  
  # ---------------------------------------------------- #
  
  ## These "values" states are used to determine the value of the "data" variable.
  ## The "data" variable and file upload error checking: ##
  
  # "data" is the variable that stores the input file (if there is any).
  data <- reactive({

    # If the file hasn't been uploaded ("values" state = NULL), then "data" = NULL:
    if (is.null(values$upload_state)) {
      return(NULL)
    }
    
    # If the user loads the example dataset:
    else if (values$upload_state == "example"){
      return(dataset_colnames(as.data.frame(read_excel("./data/example.xlsx", na = "NA"))))
    }
    
    # If the file has been uploaded ("values" state = "uploaded"): 
    else if (values$upload_state == "uploaded") {
      
      # And the user has selected the ".xlsx" option:
      if (input$file_upload_options1 == ".xlsx") {
        
        # The file extension is checked to ensure that it corresponds to the selected format:
        if (tools::file_ext(input$file_upload)[1] == "xlsx" | tools::file_ext(input$file_upload)[1] == "xls") {
          # The file is read, modified using the "dataset_colnames" function 
          # defined in the "funciones.R" file, and stored in "data_check": 
          data_check <- dataset_colnames(as.data.frame(read_excel(input$file_upload$datapath, na = "NA")))
          
          # If the uploaded dataset contains all the necessary columns, the ids are not repeated, 
          # it contains more than one study and the number of studies that do NOT lack diseased 
          # and/or non-diseased patients is less than 2, then "data <- data_check". 
          # If not, various error messages are displayed to tell the user what the problem was, 
          # and the application is returned to its "reset" state:
          if(length(setdiff(c("id", "tp", "tn", "fp", "fn"), colnames(data_check))) == 0) {
            if (nrow(data_check)==length(unique(data_check$id))){
              if (nrow(data_check)>1){
                if (length(which(dataset_XYZ(data_check)$X_uni$n0==0))+length(which(dataset_XYZ(data_check)$X_uni$n1==0)) >= nrow(data_check)-1){
                  error_mess <- HTML("The number of studies that do NOT lack diseased and/or non-diseased patients is less than 2, thus no analysis can be performed. Please check the data or choose another dataset.")
                  showNotification(error_mess, type = "error", duration = 30) 
                  values$upload_state <- "reset"
                  shinyjs::reset("file_upload")
                  return(NULL)
                }
                else{
                  return(data_check)
                }
              }
              else{
                error_mess <- HTML("Error: data contains less than 2 studies. <br/> Please upload a dataset with more than 1 study.")
                showNotification(error_mess, type = "error", duration = 10) 
                values$upload_state <- "reset"
                shinyjs::reset("file_upload")
                return(NULL)
              }
            }
            else{
              error_mess <- HTML("Error: two or more studies have the same ID. <br/> Please make sure that no ID is repeated.")
              showNotification(error_mess, type = "error", duration = 10) 
              values$upload_state <- "reset"
              shinyjs::reset("file_upload")
              return(NULL)
            }
          }
          else {
            error_mess <- HTML(paste0("Error! <br/> The following colmuns are missing: ", toString(setdiff(c("id", "tp", "tn", "fp", "fn"), colnames(data_check))), ". <br/> Please upload a valid dataset."))            
            showNotification(error_mess, type = "error", duration = 10) 
            values$upload_state <- "reset"
            shinyjs::reset("file_upload")
            return(NULL)
          }
        }
        
        # If the file extension does not correspond to the selected format, 
        # an error message is displayed prompting the user to select the correct format,
        # and the application is returned to its "reset" state:
        else {
          showNotification(HTML("Error: Incorrect file format. <br/> Please make sure that the selected format corresponds to the the uploaded file"), type = "error", duration = 10)
          values$upload_state <- "reset"
          shinyjs::reset("file_upload")
          return(NULL)
        }
      }
      
      # If the user has selected the ".csv" option:
      else if (input$file_upload_options1 == ".csv") {
        
        # The file extension is checked to ensure that it corresponds to the selected format:
        if (tools::file_ext(input$file_upload)[1] == "csv") {
          if (input$file_upload_options2 == "Comma"){
            # The file is read, modified using the "dataset_colnames" function 
            # defined in the "funciones.R" file, and stored in "data_check": 
            data_check <- dataset_colnames(read.csv(input$file_upload$datapath, sep = ","))
            
            # If the uploaded dataset contains all the necessary columns, the ids are not repeated, 
            # it contains more than one study and the number of studies that do NOT lack diseased 
            # and/or non-diseased patients is less than 2, then "data <- data_check". 
            # If not, various error messages are displayed to tell the user what the problem was, 
            # and the application is returned to its "reset" state:
            if(length(setdiff(c("id", "tp", "tn", "fp", "fn"), colnames(data_check))) == 0) {
              if (nrow(data_check)==length(unique(data_check$id))){
                if (nrow(data_check)>1){
                  if (length(which(dataset_XYZ(data_check)$X_uni$n0==0))+length(which(dataset_XYZ(data_check)$X_uni$n1==0)) >= nrow(data_check)-1){
                    error_mess <- HTML("The number of studies that do NOT lack diseased and/or non-diseased patients is less than 2, thus no analysis can be performed. Please check the data or choose another dataset.")
                    showNotification(error_mess, type = "error", duration = 30) 
                    values$upload_state <- "reset"
                    shinyjs::reset("file_upload")
                    return(NULL)
                  }
                  else{
                    return(data_check)
                  }
                }
                else{
                  error_mess <- HTML("Error: data contains less than 2 studies. <br/> Please upload a dataset with more than 1 study.")
                  showNotification(error_mess, type = "error", duration = 10) 
                  values$upload_state <- "reset"
                  shinyjs::reset("file_upload")
                  return(NULL)
                }
              }
              else{
                error_mess <- HTML("Error: two or more studies have the same ID. <br/> Please make sure that no ID is repeated.")
                showNotification(error_mess, type = "error", duration = 10) 
                values$upload_state <- "reset"
                shinyjs::reset("file_upload")
                return(NULL)
              }
            }
            else {
              error_mess <- HTML(paste0("Error! <br/> The following colmuns are missing: ", toString(setdiff(c("id", "tp", "tn", "fp", "fn"), colnames(data_check))), ". <br/> Please upload a valid dataset."))
              showNotification(error_mess, type = "error", duration = 10) 
              values$upload_state <- "reset"
              shinyjs::reset("file_upload")
              return(NULL)
            }
          }
          
          # Here the same operations are performed but in the case that the delimiter is a semicolon:
          else if (input$file_upload_options2 == "Semicolon") {
            data_check <- dataset_colnames(read.csv(input$file_upload$datapath, sep = ";"))
            if(length(setdiff(c("id", "tp", "tn", "fp", "fn"), colnames(data_check))) == 0) {
              if (nrow(data_check)==length(unique(data_check$id))){
                if (nrow(data_check)>1){
                  if (length(which(dataset_XYZ(data_check)$X_uni$n0==0))+length(which(dataset_XYZ(data_check)$X_uni$n1==0)) >= nrow(data_check)-1){
                    error_mess <- HTML("The number of studies that do NOT lack diseased and/or non-diseased patients is less than 2, thus no analysis can be performed. Please check the data or choose another dataset.")
                    showNotification(error_mess, type = "error", duration = 30) 
                    values$upload_state <- "reset"
                    shinyjs::reset("file_upload")
                    return(NULL)
                  }
                  else{
                    return(data_check)
                  }
                }
                else{
                  error_mess <- HTML("Error: data contains less than 2 studies. <br/> Please upload a dataset with more than 1 study.")
                  showNotification(error_mess, type = "error", duration = 10) 
                  values$upload_state <- "reset"
                  shinyjs::reset("file_upload")
                  return(NULL)
                }
              }
              else{
                error_mess <- HTML("Error: two or more studies have the same ID. <br/> Please make sure that no ID is repeated.")
                showNotification(error_mess, type = "error", duration = 10) 
                values$upload_state <- "reset"
                shinyjs::reset("file_upload")
                return(NULL)
              }
            }
            else {
              error_mess <- HTML(paste0("Error! <br/> The following colmuns are missing: ", toString(setdiff(c("id", "tp", "tn", "fp", "fn"), colnames(data_check))), ". <br/> Please upload a valid dataset."))              
              showNotification(error_mess, type = "error", duration = 10) 
              values$upload_state <- "reset"
              shinyjs::reset("file_upload")
              return(NULL)
            }
          }
        }
        
        # If the file extension does not correspond to the selected format, 
        # an error message is displayed prompting the user to select the correct format,
        # and the application is returned to its "reset" state:
        else {
          showNotification(HTML("Error: Incorrect file format. <br/> Please make sure that the selected format corresponds to the imported file format"), type = "error", duration = 10)
          values$upload_state <- "reset"
          shinyjs::reset("file_upload")
          return(NULL)
        }
      }
    }
    
    # If the reset button has been pressed ("values" state = "reset"), then "data = NULL" again:
    else if (values$upload_state == "reset") {
      return(NULL)
    }
  })
  
  # ---------------------------------------------------- #
  
  ## Enable or disable the delimiter options: ##
  
  # If the selected file type is not .csv, choosing a delimiter is not allowed:
  observeEvent(input$file_upload_options1, {
    if(input$file_upload_options1 == ".xlsx"){
      shinyjs::disable(id = "file_upload_options2")
    } else {
      shinyjs::enable(id = "file_upload_options2")
    }
  })
  
  # ---------------------------------------------------- #
  
  ## Reset selected tabs: ##
  
  # If the user uploads a new dataset or presses the "Reset" button,
  # the different tabs are returned to their original state:
  observeEvent({
    input$file_reset
    input$file_upload}, {
    
      # Forest plots tab:
      updateTabsetPanel(session, "graph_tabbox",
                        selected = "graph_forest_plots_tab")
      # Bivariate tab:
      updateTabsetPanel(session, "meta_tabbox",
                        selected = "meta_biv_tab")
      # Statistics tab:
      updateTabsetPanel(session, "meta_biv_tabset",
                        selected = "meta_biv_stats_tab")
      
      # Summary statistics tab (in subgroup analysis):
      updateTabsetPanel(session, "subgroup_tabbox",
                        selected = "meta_biv_sub_summary_stats_tab")
    })
  
  # ---------------------------------------------------- #
  
  ## Resulting output: ##
  
  # Text output when no file has been uploaded.
  # If "data" is NULL (which means that the file hasn't been uploaded or the reset button has been pressed), 
  # "output$file_not_uploaded" is TRUE (which enables the conditional panel in the ui.R file, 
  # containing file upload instructions, to show up):
  output$file_not_uploaded <- reactive({
    return(is.null(data()))
  })
  outputOptions(output, 'file_not_uploaded', suspendWhenHidden=FALSE)
  
  # Table output with the user's data in it.
  # When the file has been loaded, "data" is a dataframe which can be rendered with "DT::renderDataTable"
  # ("output$file_not_uploaded" is FALSE enabling the corresponding conditional panel to show up):
  output$file_table <- DT::renderDataTable({
    if (!is.null(data())){
      DT::datatable(data(), options = list(columnDefs = list(list(className = 'dt-center', targets = 2:ncol(data())-1)), # center align all columns except first.
                                           lengthMenu = c(5, 10, 25, 50), # options for the number of rows that can be displayed at a time.
                                           pageLength = 10, # number of rows by default.
                                           orderClasses = TRUE), # shade the column in which the data is sorted by.
                                           rownames = FALSE) # display the row identifier.
    }
  })
  
  
  # ----------------------------------------------------------------------------------------------- #

  #### FILE UPLOAD - DATASET SUMMARY: ####
  
  ## The "summary" variable: ##
  
  # "summary" is the variable that stores the summary table.
  summary <- reactive({
    
    # if the file hasn't been uploaded, "summary = NULL":
    if (is.null(data())) {
      return(NULL)
    }
    # else (if the file has been uploaded), 
    # the "dataset_summary" function is applied to the user's data:
    else {
      return(dataset_summary(data())) 
    }
  })
  
  ## Table output with the summary table: ##
  
  output$file_dataset_summary <- DT::renderDataTable({
    DT::datatable(summary(), rownames = FALSE, options = list(columnDefs = list(list(className = 'dt-center', targets = 1)), 
                                                              paging = FALSE, # table pagination.
                                                              searching = FALSE, # search (filtering) abilities.
                                                              info = FALSE, # information display field.
                                                              ordering = FALSE # ordering (sorting) abilities.
                  ))
  })
  
  ## Downloading the data summary table: ##
  
  output$dl_data_summary <- downloadHandler(
    filename = function() {
      if (values$upload_state == "example") {
        paste("example_dataset", "summary.csv", sep = "_") 
      } else {
        paste(tools::file_path_sans_ext(input$file_upload), "summary.csv", sep = "_") # Name of the downloaded file ("tools::file_path_sans_ext" removes extensions).
      }
    },
    content = function(file) {
      write.csv(summary(), file, row.names = FALSE) # The table is written as a .csv file.
    }
  )
  
  # ---------------------------------------------------- #
  
  ## Warning message when one or more studies have n0=0 or n1=0: ##
  
  senspe0 <- reactive({
    if (is.null(data())) {
      return(FALSE)
    }
    else if (length(which(dataset_XYZ(data())$X_uni$n0==0))+length(which(dataset_XYZ(data())$X_uni$n1==0)) > 0){
      return(TRUE)
    }
    else {
      return(FALSE)
    }
  })
  
  # The "senspe0_warning" output enables (when TRUE) the conditional panel in the ui.R file
  # to show up, displaying the corresponding warning message ("senspe0_message"):
  output$senspe0_warning <- reactive({
    return(senspe0())
  })
  outputOptions(output, 'senspe0_warning', suspendWhenHidden=FALSE)
  
  # The message that is printed on the screen when one or more studies have n0=0 or n1=0.
  # "renderUI" is used together with "htmltools::HTML" to be able to customize 
  # the appearance of the message, add icons, etc. (via HTML script).
  output$senspe0_message <- renderUI({
    if (is.null(data())==FALSE) {
      senspe0_list <- dataset_XYZ(data())$X_uni$id[dataset_XYZ(data())$X_uni$n0==0|dataset_XYZ(data())$X_uni$n1==0]
      HTML(paste0('<i class="fa fa-exclamation-circle"></i> ', "The following studies lack diseased and/or non-diseased patients, and therefore will not be taken into account in subsequent analyses: ", paste(senspe0_list, collapse=', ')))
    }
  })
  
  # ----------------------------------------------------------------------------------------------- #
  
  #### FILE UPLOAD - DISABLING SIDEBAR: ####
  
  # All tabs except the initial tab and the user guide are disabled 
  # when no dataset has been loaded:
  observe({
    if (is.null(data())){
      addCssClass(selector = "a[data-value='graph_tab']", class = "inactiveLink1")
      addCssClass(selector = "a[data-value='meta_tab']", class = "inactiveLink1")
      addCssClass(selector = "a[data-value='sumfind_tab']", class = "inactiveLink1")
    }
    else{
      removeCssClass(selector = "a[data-value='graph_tab']", class = "inactiveLink1")
      removeCssClass(selector = "a[data-value='meta_tab']", class = "inactiveLink1")
      removeCssClass(selector = "a[data-value='sumfind_tab']", class = "inactiveLink1")
    }
  })
  
  # ----------------------------------------------------------------------------------------------- #

  #### GRAPHICAL DESCRIPTION - COVARIATE SELECTION: ####
  
  ## The "covariate_selection" variable: ##
  
  # "covariate_selection" stores the names of all covariates plus an additional empty option 
  # (in case the user does not want to select any covariate).
  covariate_selection <- reactive({
    if (is.null(data())) {
      return(NULL)
    }
    else {
      not_covariates <- c("tp", "fp", "fn", "tn", "id") # columns that are not covariates.
      covariate_names <- names(data()[, !names(data()) %in% not_covariates, drop = F]) # names of the covariates.
      covariate_names_select <- c("-", covariate_names) # names of the covariates plus the empty option, "-".
      return(covariate_names_select)
    }
  })
  
  ## Drop-down menu contents: ##
  
  # The content of the "graph_covariate_select" drop-down menu is modified 
  # by adding the choices within the "covariate_selection" variable 
  # (all the covariates and the additional empty option):
  observe({
    updateSelectInput(session, "graph_covariate_select",
                      choices = covariate_selection()
    )})
  
  
  ## Selected covariate: ##
  
  # The "covariate_selected" variable stores the name of the covariable selected 
  # by the user in the "graph_covariate_select" drop-down menu.
  covariate_selected <- reactive({
    return(input$graph_covariate_select)
  })
  
  
  # ----------------------------------------------------------------------------------------------- #
  
  #### GRAPHICAL DESCRIPTION - FOREST PLOTS: ####
  
  ## Sensitivity ##
  
  # Variable that stores the sensitivity forest plot:
  forest_sen <- reactive({
    if (is.null(data())) {
      return(NULL)
    }
    else {
      return(forest_plot_sen(data(), covariate_selected()))
    }
  })
  
  # The height of the plot is adjusted according to the number of studies and rendered.
  output$graph_forest_sen <- renderUI({
    plotOutput("sen", height = 130+15*nrow(data())+15*length(unique(data()[1:nrow(data()),covariate_selected()])))
  })
  output$sen <- renderPlot({
    forest_sen()
  })
  
  ## Downloading the sensitivity forest plot: ##

  output$dl_graph_forest_sen <- downloadHandler(
    filename = function(){
      if (values$upload_state == "example") {
        paste("example_dataset", paste("sensitivity_forest_plot", input$dl_forest_sen_options, sep = ""), sep = "_") 
      } else {
        paste(tools::file_path_sans_ext(input$file_upload), paste("sensitivity_forest_plot", input$dl_forest_sen_options, sep = ""), sep = "_") 
      }
    },
    content = function(file){
      # Downlading a .png file:
      if (input$dl_forest_sen_options == ".png") {
        png(file, width = 1500, height = 350+37.5*nrow(data())+37.5*length(unique(data()[1:nrow(data()),covariate_selected()])), units = "px", res = 180)
      }
      # Downlading a .svg file:
      else if (input$dl_forest_sen_options == ".svg") {
        svg(file, width = 10, height = 12+0.2*nrow(data()))
      }  
      forest_plot_sen(data(), covariate_selected())
      dev.off()
    }
  )
  
  # ---------------------------------------------------- #

  ## Specificity ##
  
  # Variable that stores the specificity forest plot:
  forest_spe <- reactive({
    if (is.null(data())) {
      return(NULL)
    }
    else {
      return(forest_plot_spe(data(), covariate_selected()))
    }
  })

  # The height of the plot is adjusted according to the number of studies and rendered.
  output$graph_forest_spe <- renderUI({
    plotOutput("spe", height = 130+15*nrow(data())+15*length(unique(data()[1:nrow(data()),covariate_selected()])))
  })
  output$spe <- renderPlot({
    forest_spe()
  })
  
  ## Downloading the specificity forest plot: ##
  
  output$dl_graph_forest_spe <- downloadHandler(
    filename = function(){
      if (values$upload_state == "example") {
        paste("example_dataset", paste("specificity_forest_plot", input$dl_forest_spe_options, sep = ""), sep = "_") 
      } else {
        paste(tools::file_path_sans_ext(input$file_upload), paste("specificity_forest_plot", input$dl_forest_spe_options, sep = ""), sep = "_") 
      }
    },
    content = function(file){
      # Downlading a .png file:
      if (input$dl_forest_spe_options == ".png") {
        png(file, width = 1500, height = 350+37.5*nrow(data())+37.5*length(unique(data()[1:nrow(data()),covariate_selected()])), units = "px", res = 180)
      }
      # Downlading a .svg file:
      else if (input$dl_forest_spe_options == ".svg") {
        svg(file, width = 10, height = 12+0.2*nrow(data()))
      }  
      forest_plot_spe(data(), covariate_selected())
      dev.off()
    }
  )
  
  
  # ----------------------------------------------------------------------------------------------- #
  
  #### GRAPHICAL DESCRIPTION - ROC PLANE: ####
  
  # Variable that stores the forest plot:
  roc_plane <- reactive({
    if (is.null(data())) {
      return(NULL)
    }
    else {
      return(roc_plane_func(data(), covariate_selected(), input$graph_roc_ci_display))
    }
  })
  
  ## Rendering the plot: ##
  
  output$graph_roc <- plotly::renderPlotly({
    if (covariate_selected() != "-"){
    ggplotly(roc_plane(), tooltip = "text", height = 600, width = 600) %>% # The plot is transformed into a plotly object.
             layout(legend = list(x = 0.82, y = 0.05)) %>% # To set the position of the legend.
             config(displayModeBar = F)} # This hides the plotly embedded menu (known as modebar). 
    else {
      ggplotly(roc_plane(), tooltip = "text", height = 600, width = 600) %>% 
              config(displayModeBar = F)} 
  })
  
  ## Downloading the plot: ##

  # An "observe" environment is initialized to perform different actions 
  # depending on what the user has selected as file format option (.png or .svg): 
  observe({
    
    # Downlading a .png file:
    if (input$dl_graph_roc_options == ".png") {
      output$dl_graph_roc <- downloadHandler(
        filename = function(){
          if (values$upload_state == "example") {
            paste("example_dataset", "ROC_plane.png", sep = "_") 
          } else {
            paste(tools::file_path_sans_ext(input$file_upload), "ROC_plane.png", sep = "_") 
          }
        },
        content = function(file) {
          ggsave(file, plot = roc_plane(), device = "png", width = 6.5, height = 6.5) 
        })
    }
    
    # Downlading a .svg file:
    if (input$dl_graph_roc_options == ".svg") {
      output$dl_graph_roc <- downloadHandler(
        filename = function(){
          if (values$upload_state == "example") {
            paste("example_dataset", "ROC_plane.svg", sep = "_") 
          } else {
            paste(tools::file_path_sans_ext(input$file_upload), "ROC_plane.svg", sep = "_") 
          }
        },
        content = function(file) {
          ggsave(file, plot = roc_plane(), device = "svg", width = 6.5, height = 6.5) 
        })
    }  
  })

  # ----------------------------------------------------------------------------------------------- #
  
  ### METANALYSIS - MODEL SELECTION ####
  
  # "model_select" stores the result of the "BRMA_model_select()" function, 
  # which determines whether a bivariate model can be applied 
  # (if the dataset has more than 4 instances and the model converges) 
  # or a univariate model should be used.
  model_select <- reactive({
    if (is.null(data())) {
      return('NULL')
    }
    else {
      return(BRMA_model_select(data())$model)
    }
  })
  
  # The "bivariate" output is used in the ui.R script to define the 
  # conditionalPanel's condition in the meta-analysis statistics tab. 
  output$bivariate <- reactive({
    return(model_select()=="bivariate")
  })
  outputOptions(output, 'bivariate', suspendWhenHidden=FALSE)
  
  ## Bivariate - Univariate message: ##
  
  # The message that is printed on the screen when the model is univariate (rows < 4 or does not converge).
  # "renderUI" is used together with "htmltools::HTML" to be able to customize 
  # the appearance of the message, add icons, etc. (via HTML script).
  output$univariate_message <- renderUI({
    if (nrow(data()) < 4){
      htmltools::HTML('<p style="text-align:center; font-size:16px"><i class="fa fa-exclamation-circle"></i> Attention: The uploaded dataset contains less than 4 studies, please refer to the results presented in the <strong>Univariate model</strong> tab.</p>')
    }
    else {
      htmltools::HTML('<p style="text-align:center; font-size:16px"><i class="fa fa-exclamation-circle"></i> Attention: The bivariate model has not converged, please refer to the results presented in the <strong>Univariate model</strong> tab.</p>')
    }
  })
  
  # If the bivariate analysis doesn't converge, then all other tabs 
  # (SROC, Subgroup analysis and Sensitivity analysis) are disabled:
  observe({
    if (model_select() == "univariate"){
      addCssClass(selector = "a[data-value='meta_biv_sroc_tab']", class = "inactiveLink2")
      addCssClass(selector = "a[data-value='meta_biv_sub_tab']", class = "inactiveLink2")
      addCssClass(selector = "a[data-value='meta_biv_sens_tab']", class = "inactiveLink2")
    }
    if (model_select() == "bivariate"){
      removeCssClass(selector = "a[data-value='meta_biv_sroc_tab']", class = "inactiveLink2")
      removeCssClass(selector = "a[data-value='meta_biv_sub_tab']", class = "inactiveLink2")
      removeCssClass(selector = "a[data-value='meta_biv_sens_tab']", class = "inactiveLink2")
    }
  })

  
  # ----------------------------------------------------------------------------------------------- #
  
  #### METANALYSIS - BIVARIATE - STATISTICS ####
  
  # The "meta_biv_stats" variable stores a list containing 
  # the three outputs of the "BRMA_statistics" function 
  # (summary statistics, revman parameters and heterogeneity tables)
  meta_biv_stats <- reactive({
    if (is.null(data())) {
      return(NULL)
    }
    else {
      return(BRMA_statistics(data()))
    }
  })
  
  # First table output, the Summary Statistics table:
  output$meta_biv_stats_summary <- DT::renderDataTable({
    DT::datatable(meta_biv_stats()[[1]], options = list(columnDefs = list(list(className = 'dt-center', targets = 1:3)), 
                                                        paging = FALSE,
                                                        searching = FALSE,
                                                        info = FALSE, 
                                                        ordering = FALSE))
  })
  # Second table output, the Revman Parameters table:
  output$meta_biv_stats_revman <- DT::renderDataTable({
    DT::datatable(meta_biv_stats()[[2]], options = list(columnDefs = list(list(className = 'dt-center', targets = 1)), 
                                                        paging = FALSE,
                                                        searching = FALSE,
                                                        info = FALSE, 
                                                        ordering = FALSE))
  })
  # Third table output, the Heterogeneity table:
  output$meta_biv_stats_heterogen <- DT::renderDataTable({
    DT::datatable(meta_biv_stats()[[3]], options = list(columnDefs = list(list(className = 'dt-center', targets = 1)),
                                                        paging = FALSE,
                                                        searching = FALSE,
                                                        info = FALSE, 
                                                        ordering = FALSE))
  })
  
  ## Downloading the three tables: ##
  
  # The first table:
  output$dl_meta_biv_stats_summary <- downloadHandler(
    filename = function() {
      if (values$upload_state == "example") {
        paste("example_dataset", "biv_stats_summary.csv", sep = "_") 
      } else {
        paste(tools::file_path_sans_ext(input$file_upload), "biv_stats_summary.csv", sep = "_") 
      }
    },
    content = function(file) {
      write.csv(meta_biv_stats()[[1]], file, row.names = TRUE) 
    }
  )
  
  # The second table:
  output$dl_meta_biv_stats_revman <- downloadHandler(
    filename = function() {
      if (values$upload_state == "example") {
        paste("example_dataset", "biv_stats_revman.csv", sep = "_") 
      } else {
        paste(tools::file_path_sans_ext(input$file_upload), "biv_stats_revman.csv", sep = "_") 
      }
    },
    content = function(file) {
      write.csv(meta_biv_stats()[[2]], file, row.names = TRUE) 
    }
  )
  
  # The thrid table:
  output$dl_meta_biv_stats_heterogen <- downloadHandler(
    filename = function() {
      if (values$upload_state == "example") {
        paste("example_dataset", "biv_stats_heterogen.csv", sep = "_") 
      } else {
        paste(tools::file_path_sans_ext(input$file_upload), "biv_stats_heterogen.csv", sep = "_") 
      }
    },
    content = function(file) {
      write.csv(meta_biv_stats()[[3]], file, row.names = TRUE) 
    }
  )
  
  # ---------------------------------------------------- #
  
  # Variance and correlation conditions:
  varcorr <- reactive({
    if (is.null(data())) {
      return(NULL)
    }
    else if (!is.null(data()) & model_select()=="bivariate"){
      if (meta_biv_stats()[[2]][8] == 1){
        return("corr1")
      }
      if (meta_biv_stats()[[2]][8] == -1){
        return("corr-1")
      }
      if (meta_biv_stats()[[2]][3] == 0){
        return("sen0")
      }
      if (meta_biv_stats()[[2]][4] == 0){
        return("spe0")
      }
      else{
        return(NULL)
      }
    }
  }) 
  
  # The "bivariate" output is used in the ui.R script to define the 
  # conditionalPanel's condition in the meta-analysis statistics tab. 
  output$varcorr_recommend <- reactive({
    return(!is.null(varcorr()))
  })
  outputOptions(output, 'varcorr_recommend', suspendWhenHidden=FALSE)
  
  # The message that is printed on the screen when the model is univariate (rows < 4 or does not converge).
  # "renderUI" is used together with "htmltools::HTML" to be able to customize 
  # the appearance of the message, add icons, etc. (via HTML script).
  output$varcorr_message <- renderUI({
    if (varcorr() == "corr1"){
      htmltools::HTML('<p style="text-align:center; font-size:16px"><i class="fa fa-exclamation-circle"></i> Attention: Model fitting encountered problems to estimate some parameters of the model.</p>')
    }
    else if (varcorr() == "corr-1"){
      htmltools::HTML('<p style="text-align:center; font-size:16px"><i class="fa fa-exclamation-circle"></i> Attention: Model fitting encountered problems to estimate some parameters of the model.</p>')
    }
    else if (varcorr() == "sen0"){
      htmltools::HTML('<p style="text-align:center; font-size:16px"><i class="fa fa-exclamation-circle"></i> Attention: Model fitting encountered problems to estimate some parameters of the model.</p>')
    }
    else if (varcorr() == "spe0"){
      htmltools::HTML('<p style="text-align:center; font-size:16px"><i class="fa fa-exclamation-circle"></i> Attention: Model fitting encountered problems to estimate some parameters of the model.</p>')
    }
  })
  
  # ----------------------------------------------------------------------------------------------- #
  
  #### METANALYSIS - UNIVARIATE - STATISTICS ####
  
  # The "meta_uni_stats" variable stores a list containing 
  # the two outputs of the "URMA_statistics" function 
  # (summary statistics and heterogeneity tables)
  meta_uni_stats <- reactive({
    if (is.null(data())) {
      return(NULL)
    }
    else {
      return(URMA_statistics(data()))
    }
  })
  
  # First table output, the Summary Statistics table:
  output$meta_uni_stats_summary <- DT::renderDataTable({
    DT::datatable(meta_uni_stats()[[1]], options = list(columnDefs = list(list(className = 'dt-center', targets = 1:3)), # center align all columns except first.
                                                        paging = FALSE,
                                                        searching = FALSE,
                                                        info = FALSE, 
                                                        ordering = FALSE))
  })
 
  # Second table output, the Heterogeneity table:
  output$meta_uni_stats_heterogen <- DT::renderDataTable({
    DT::datatable(meta_uni_stats()[[2]], options = list(columnDefs = list(list(className = 'dt-center', targets = 1)), # center align all columns except first.
                                                        paging = FALSE,
                                                        searching = FALSE,
                                                        info = FALSE, 
                                                        ordering = FALSE))
  })
  
  ## Downloading the two tables: ##
  
  # The first table:
  output$dl_meta_uni_stats_summary <- downloadHandler(
    filename = function() {
      if (values$upload_state == "example") {
        paste("example_dataset", "uni_stats_summary.csv", sep = "_") 
      } else {
        paste(tools::file_path_sans_ext(input$file_upload), "uni_stats_summary.csv", sep = "_") 
      }
    },
    content = function(file) {
      write.csv(meta_uni_stats()[[1]], file, row.names = TRUE) 
    }
  )
  
  # The second table:
  output$dl_meta_uni_stats_heterogen <- downloadHandler(
    filename = function() {
      if (values$upload_state == "example") {
        paste("example_dataset", "uni_stats_heterogen.csv", sep = "_") 
      } else {
        paste(tools::file_path_sans_ext(input$file_upload), "uni_stats_heterogen.csv", sep = "_") 
      }
    },
    content = function(file) {
      write.csv(meta_uni_stats()[[2]], file, row.names = TRUE) 
    }
  )
  
  # ---------------------------------------------------- #
  
  ## Forest Plot- Sensitivity ##
  
  # Variable that stores the sensitivity forest plot:
  uni_forest_sen <- reactive({
    if (is.null(data())) {
      return(NULL)
    }
    else {
      return(forest_plot_sen(data(), NULL))
    }
  })
  
  # The height of the plot is adjusted according to the number of studies and rendered.
  output$uni_forest_sen <- renderUI({
    plotOutput("uni_sen", height = 160+15*nrow(data()))
  })
  output$uni_sen <- renderPlot({
    uni_forest_sen() 
  })
  
  ## Downloading the sensitivity forest plot: ##
  
  output$dl_uni_forest_sen <- downloadHandler(
    
    filename = function(){
      if (values$upload_state == "example") {
        paste("example_dataset", paste("sensitivity_forest_plot", input$dl_uni_forest_sen_options, sep = ""), sep = "_") 
      } else {
        paste(tools::file_path_sans_ext(input$file_upload), paste("sensitivity_forest_plot", input$dl_uni_forest_sen_options, sep = ""), sep = "_")
      }
    },
    content = function(file){
      # Downlading a .png file:
      if (input$dl_uni_forest_sen_options == ".png") {
        png(file, width = 1500, height = 350+40*nrow(data()), units = "px", res = 180)
      }
      # Downlading a .svg file:
      else if (input$dl_uni_forest_sen_options == ".svg") {
        svg(file, width = 10, height = 12+0.2*nrow(data()))
      }  
      forest_plot_sen(data(), NULL)
      dev.off()
    }
  )
  
  # ---------------------------------------------------- #
  
  ## Forest Plot - Specificity ##
  
  # Variable that stores the specificity forest plot (if there is any).
  uni_forest_spe <- reactive({
    if (is.null(data())) {
      return(NULL)
    }
    else {
      return(forest_plot_spe(data(), NULL))
    }
  })
  
  # The height of the plot is adjusted according to the number of studies and rendered.
  output$uni_forest_spe <- renderUI({
    plotOutput("uni_spe", height = 160+15*nrow(data()))
  })
  output$uni_spe <- renderPlot({
    uni_forest_spe()
  })
  
  ## Downloading the specificity forest plot: ##
  
  output$dl_uni_forest_spe <- downloadHandler(
    
    filename = function(){
      if (values$upload_state == "example") {
        paste("example_dataset", paste("specificity_forest_plot", input$dl_uni_forest_spe_options, sep = ""), sep = "_") 
      } else {
        paste(tools::file_path_sans_ext(input$file_upload), paste("specificity_forest_plot", input$dl_uni_forest_spe_options, sep = ""), sep = "_") 
      }
    },
    content = function(file){
      # Downlading a .png file:
      if (input$dl_uni_forest_spe_options == ".png") {
        png(file, width = 1500, height = 350+40*nrow(data()), units = "px", res = 180)
      }
      # Downlading a .svg file:
      else if (input$dl_uni_forest_spe_options == ".svg") {
        svg(file, width = 10, height = 12+0.2*nrow(data()))
      }  
      forest_plot_spe(data(), NULL)
      dev.off()
    }
  )
  

  # ----------------------------------------------------------------------------------------------- #

  #### METANALYSIS - BIVARIATE - SROC CURVE ####
  
  # The "meta_biv_sroc" variable stores the bivariate SROC curve: 
  meta_biv_sroc <- reactive({
    if (is.null(data())) {
      return(NULL)
    }
    else {
      
      # The "BRMA_sroc()" function is used to generate the graph, 
      # and the user inputs are used as arguments of the function 
      # (the output of a checklist is a list that only includes the selected items, 
      # so we check if a certain item is in the "input$meta_biv_sroc_options" list).
      return(BRMA_sroc(data(), 
             summary_point = "sum_point" %in% input$meta_biv_sroc_options,
             ellipse_conf = "conf_ellip" %in% input$meta_biv_sroc_options, 
             ellipse_pred = "pred_ellip" %in% input$meta_biv_sroc_options, 
             curve = "curve" %in% input$meta_biv_sroc_options, 
             points = "points" %in% input$meta_biv_sroc_options))
    }
  })
 
  ## Rendering the plot: ##
  
  meta_biv_sroc_plotly <- reactive({
    plot <- ggplotly(meta_biv_sroc(), tooltip = c("text"), height = 550, width = 760) # The ggplot object is converted to plotlty object.
    # The legend labels are adjusted:
    for (i in 1:length(plot$x$data)){
      if (!is.null(plot$x$data[[i]]$name)){
        plot$x$data[[i]]$name =  gsub("\\(","",strsplit(plot$x$data[[i]]$name,",")[[1]][1])
      }
    }
    return(plot)
  })
  
  output$meta_biv_sroc_curve <- plotly::renderPlotly({
    meta_biv_sroc_plotly() %>% 
      config(displayModeBar = F) # This hides the plotly embedded menu (known as modebar).
  })
  
  ## Downloading the plot: ##
  
  # An "observe" environment is initialized to perform different actions 
  # depending on what the user has selected as file format option (.png or .svg): 
  observe({
    
    # Downlading a .png file:
    if (input$dl_meta_biv_sroc_curve_options == ".png") {
      output$dl_meta_biv_sroc_curve <- downloadHandler(
        filename = function(){
          if (values$upload_state == "example") {
            paste("example_dataset", "SROC_curve.png", sep = "_") 
          } else {
            paste(tools::file_path_sans_ext(input$file_upload), "SROC_curve.png", sep = "_") # Name of the downloaded file ("tools::file_path_sans_ext" removes extensions).
          }
        },
        content = function(file) {
          ggsave(file, plot = meta_biv_sroc(), device = "png", width = 9, height = 6.5) 
        })
    }
    
    # Downlading a .svg file:
    if (input$dl_meta_biv_sroc_curve_options == ".svg") {
      output$dl_meta_biv_sroc_curve <- downloadHandler(
        filename = function(){
          if (values$upload_state == "example") {
            paste("example_dataset", "SROC_curve.svg", sep = "_") 
          } else {
            paste(tools::file_path_sans_ext(input$file_upload), "SROC_curve.svg", sep = "_") # Name of the downloaded file ("tools::file_path_sans_ext" removes extensions).
          }
        },
        content = function(file) {
          ggsave(file, plot = meta_biv_sroc(), device = "svg", width = 9, height = 6.5) 
        })
    }  
  })
  
  
  # ----------------------------------------------------------------------------------------------- #
  
  #### METANALYSIS - BIVARIATE - SUBGROUP ANALYSIS ####
  
  ## The "covariate_selection_subgroup" variable: ##
  
  # "covariate_selection_subgroup" stores the names of all covariates:
  covariate_selection_subgroup <- reactive({
    if (is.null(data())) {
      return(NULL)
    }
    else {
      not_covariates <- c("tp", "fp", "fn", "tn", "id") # columns that are not covariates.
      covariate_names_select <- names(data()[, !names(data()) %in% not_covariates, drop = F]) # names of the covariates.
      return(covariate_names_select)
    }
  })
  
  ## Disabling the "Subgroup analysis" tab: ##
  
  # If the dataset has no covariates, the subgroup analysis tab is disabled:
  observe({
    if (length(covariate_selection_subgroup())==0){
      addCssClass(selector = "a[data-value='meta_biv_sub_tab']", class = "inactiveLink2")
    }
  })  
  
  ## Drop-down menu contents: ##
  
  # The content of the "meta_biv_sub_covariate_select" drop-down menu is modified 
  # by adding the choices within the "covariate_selection_subgroup" variable (all the covariates)
  observe({
    updateSelectInput(session, "meta_biv_sub_covariate_select",
                      choices = covariate_selection_subgroup()
    )})
  
  
  ## Selected covariate: ##
  
  # The "covariate_selected_subgroup" variable stores the name of the covariate selected 
  # by the user in the "meta_biv_sub_covariate_select" drop-down menu.
  covariate_selected_subgroup <- reactive({
    if (is.null(data()) | is.null(covariate_selection_subgroup())){
      return(NULL)
    }
    else{
      return(input$meta_biv_sub_covariate_select)
    }
  })
  
  
  ## Number of categories: ##
  
  # "subgroup_categories_covar" indicates whether the selected covariate 
  # has two or more categories:
  subgroup_categories_covar <- reactive({
    if (!is.null(data())){
      if (length(unique(data()[1:nrow(data()),covariate_selected_subgroup()])) != 2){
        return(TRUE)
      }
      else {
        return(FALSE)
      }
    }
  })
  
  # The "subgroup_categories" output is used in the ui.R script 
  # to enable the corresponding conditional panel:
  output$subgroup_categories <- reactive({
    return(subgroup_categories_covar())
  })
  outputOptions(output, 'subgroup_categories', suspendWhenHidden=FALSE)
  
  
  ## Disabling the "Metaregression" tab: ##
  
  # If the number of categories that the selected covariate has is greater than 2,
  # the "Metaregression" tab is disaled.
  observe({
    if (!is.null(data())){
      if (length(unique(data()[1:nrow(data()),covariate_selected_subgroup()])) > 2){
        addCssClass(selector = "a[data-value='meta_biv_sub_metareg_tab']", class = "inactiveLink2")
      }
      else {
        removeCssClass(selector = "a[data-value='meta_biv_sub_metareg_tab']", class = "inactiveLink2")
      }
    }  
  })  
        
  # ---------------------------------------------------- #
  
  ## Evaluating convergence: ##

  # "model_select_subgroup" stores the result of the "BRMA_model_select()" function,
  # which determines whether a bivariate model can be applied
  # (if the dataset has more than 4 instances and the model converges)
  # or a univariate model should be used.
  model_select_subgroup <- reactive({
    if (is.null(data()) | covariate_selected_subgroup() == "") {
      return(FALSE)
    }
    else {
      return(subgroup_model_select(data(), covariate_selected_subgroup())$model == "bivariate")
    }
  })

  # The "bivariate_subgroup" output is used in the ui.R script to define the
  # conditionalPanel's condition in the subgroup analysis tab.
  output$bivariate_subgroup <- reactive({
    if (is.null(data())) {
      return(FALSE)
    }
    else {
      if (covariate_selected_subgroup() == ""){
        return(FALSE)
      }
      else {
        return(model_select_subgroup())
      }
    }
  })
  outputOptions(output, 'bivariate_subgroup', suspendWhenHidden=FALSE)

  # ---------------------------------------------------- #
  
  ## Building the three tables: ##
  
  # First table output, the subgroup Summary Statistics table:
  stats_summary_sub <- reactive({
    return(subgroup(data(), covariate_selected_subgroup())$summary_statistics_subgroup)
  })
  
  # Second table output, the subgroup Revman table:
  stats_revman_sub <- reactive({
    return(subgroup(data(), covariate_selected_subgroup())$revman_subgroup)
  })
  
  # Third table output, the subgroup Metaregression table:
  stats_metareg_sub <- reactive({
    return(metaregresion(data(), covariate_selected_subgroup()))
  })
  
  ## Rendering and downloading the three tables: ##
  
  # Summary statistics:
  output$meta_biv_sub_summary <- DT::renderDataTable({
    DT::datatable(stats_summary_sub(), rownames = FALSE, options = list(columnDefs = list(list(className = 'dt-center', targets = 2:ncol(stats_summary_sub())-1)), 
                                                                        paging = FALSE, 
                                                                        searching = FALSE, 
                                                                        info = FALSE, 
                                                                        ordering = FALSE 
                                                                        ))
  })
  
  output$dl_meta_biv_sub_summary <- downloadHandler(
    filename = function() {
      if (values$upload_state == "example") {
        paste("example_dataset", "biv_sub_summary.csv", sep = "_") 
      } else {
        paste(tools::file_path_sans_ext(input$file_upload), "biv_sub_summary.csv", sep = "_") 
      }
    },
    content = function(file) {
      write.csv(stats_summary_sub(), file, row.names = FALSE) 
    }
  )
  
  # Revman parameters:
  output$meta_biv_sub_revman <- DT::renderDataTable({
    DT::datatable(stats_revman_sub(), rownames = FALSE, options = list(columnDefs = list(list(className = 'dt-center', targets = 2:ncol(stats_revman_sub())-1)), 
                                                                       paging = FALSE,
                                                                       searching = FALSE,
                                                                       info = FALSE, 
                                                                       ordering = FALSE
                                                                       ))
  })
  
  output$dl_meta_biv_sub_revman <- downloadHandler(
    filename = function() {
      if (values$upload_state == "example") {
        paste("example_dataset", "biv_sub_revman.csv", sep = "_") 
      } else {
        paste(tools::file_path_sans_ext(input$file_upload), "biv_sub_revman.csv", sep = "_") 
      }
    },
    content = function(file) {
      write.csv(stats_revman_sub(), file, row.names = FALSE) 
    }
  )
  
  # Metaregression:
  output$meta_biv_sub_metareg <- DT::renderDataTable({
    DT::datatable(stats_metareg_sub(), rownames = FALSE, options = list(columnDefs = list(list(className = 'dt-center', targets = 1:4)),
                                                                        paging = FALSE,
                                                                        searching = FALSE,
                                                                        info = FALSE, 
                                                                        ordering = FALSE
                                                                        ))
  })
  
  output$dl_meta_biv_sub_metareg <- downloadHandler(
    filename = function() {
      if (values$upload_state == "example") {
        paste("example_dataset", "biv_sub_metareg.csv", sep = "_") 
      } else {
        paste(tools::file_path_sans_ext(input$file_upload), "biv_sub_metareg.csv", sep = "_") 
      }
    },
    content = function(file) {
      write.csv(stats_metareg_sub(), file, row.names = FALSE) 
    }
  )
  
  # ---------------------------------------------------- #
  
  ## SROC curve: ##
  
  # The "meta_biv_sub_sroc" variable stores the subgroup SROC curve: 
  meta_biv_sub_sroc <- reactive({
    if (is.null(data()) | any((data() %>% group_by(get(covariate_selected_subgroup())) %>% summarize(count=n()))$count < 3)) {
      return(NULL)
    }
    else {
      # The "BRMA_sroc()" function is used to generate the graph, 
      # and the user inputs are used as arguments of the function 
      # (the output of a checklist is a list that only includes the selected items, 
      # so we check if a certain item is in the "input$meta_biv_sub_options" list).
      return(BRMA_sroc(data(), covariate = covariate_selected_subgroup(), subgroup = TRUE,
                       summary_point = "sum_point" %in% input$meta_biv_sub_options,
                       ellipse_conf = "conf_ellip" %in% input$meta_biv_sub_options, 
                       ellipse_pred = "pred_ellip" %in% input$meta_biv_sub_options, 
                       curve = "curve" %in% input$meta_biv_sub_options, 
                       points = "points" %in% input$meta_biv_sub_options))
    }
  })
  
  # The "sub_roc_warning" output is used in the ui.R script to define the 
  # conditionalPanel's condition in the subgroup SROC curve box 
  output$sub_roc_warning <- reactive({
    if (is.null(data()) | is.null(covariate_selected_subgroup())) {
      return('NULL')
    }
    else {
      if (any((data() %>% group_by(get(covariate_selected_subgroup())) %>% summarize(count=n()))$count < 3)) { 
        return(FALSE)
      }
      else {
        return(TRUE)
      }
    }
  })
  outputOptions(output, 'sub_roc_warning', suspendWhenHidden=FALSE)
      
  ## Rendering the plot: ##
  
  meta_biv_sub_sroc_plotly <- reactive({
    if (is.null(data()) | any((data() %>% group_by(get(covariate_selected_subgroup())) %>% summarize(count=n()))$count < 3)) {
      return(NULL)
    }
    else {
      plot <- ggplotly(meta_biv_sub_sroc(), tooltip = c("text"), height = 400, width = 560) # The ggplot object is converted to plotlty object.
      # The legend labels are adjusted:
      plot$x$layout$annotations[[1]]$text <- ""
      for (i in 1:length(plot$x$data)) {
        if (!is.null(plot$x$data[[i]]$name)) {
            name <- gsub("^([^,]*,[^,]*),.*","\\1", plot$x$data[[i]]$name)
            plot$x$data[[i]]$name <- gsub(","," ", name)
            plot$x$data[[i]]$name <- gsub("\\(|\\)","", plot$x$data[[i]]$name)
        }
        if (!is.null(plot$x$data[[i]]$marker$symbol)) {
          # To include summary points in the legend, which are not shown by default:
          if (plot$x$data[[i]]$marker$symbol == "square") {
            plot$x$data[[i]]$showlegend <- TRUE
            plot$x$data[[i]]$name <- gsub(":.*","",plot$x$data[[i]]$text)
  
          }
          # To remove the last characters of points names:
          if (plot$x$data[[i]]$marker$symbol != "square") {
            plot$x$data[[i]]$name <- gsub(" .*", "", plot$x$data[[i]]$name)
          }
        }
      }
      return(plot)
    }
  })

  output$meta_biv_sub_sroc <- plotly::renderPlotly({
    meta_biv_sub_sroc_plotly() %>% 
      config(displayModeBar = F) # This hides the plotly embedded menu (known as modebar).
  })
  
  ## Downloading the plot: ##
  
  # An "observe" environment is initialized to perform different actions 
  # depending on what the user has selected as file format option (.png or .svg): 
  observe({
    
    # Downlading a .png file:
    if (input$dl_meta_biv_sub_sroc_options == ".png") {
      output$dl_meta_biv_sub_sroc <- downloadHandler(
        filename = function(){
          if (values$upload_state == "example") {
            paste("example_dataset", "sub_SROC_curve.png", sep = "_") 
          } else {
            paste(tools::file_path_sans_ext(input$file_upload), "sub_SROC_curve.png", sep = "_") 
          }
        },
        content = function(file) {
          ggsave(file, plot = meta_biv_sub_sroc(), device = "png", width = 9, height = 6.5) 
        })
    }
    
    # Downlading a .svg file:
    if (input$dl_meta_biv_sub_sroc_options == ".svg") {
      output$dl_meta_biv_sub_sroc <- downloadHandler(
        filename = function(){
          if (values$upload_state == "example") {
            paste("example_dataset", "sub_SROC_curve.svg", sep = "_") 
          } else {
            paste(tools::file_path_sans_ext(input$file_upload), "sub_SROC_curve.svg", sep = "_")
          }
        },
        content = function(file) {
          ggsave(file, plot = meta_biv_sub_sroc(), device = "svg", width = 9, height = 6.5)  
        })
    }  
  })
  
  
  # ----------------------------------------------------------------------------------------------- #
  
  #### METANALYSIS - BIVARIATE - SENSITIVITY ANALYSIS ####
  
  ## Disabling the "Sensitivity analysis" tab: ##
  
  # If the dataset does not contain covariables this tab is disabled:
  observe({
    if (length(covariate_selection_subgroup())==0){
      addCssClass(selector = "a[data-value='meta_biv_sens_tab']", class = "inactiveLink2")
    }
  })  
  
  ## Drop-down covariate menu contents: ##
  
  # The content of the "meta_biv_sens_covariate_select" drop-down menu is modified 
  # by adding the choices within the "covariate_selection_subgroup" variable (all the covariates)
  observe({
    updateSelectInput(session, "meta_biv_sens_covariate_select",
                      choices = covariate_selection_subgroup()
    )})
  
  
  ## Selected covariate: ##
  
  # The "covariate_selected_sensitivity" variable stores the name of the covariate selected 
  # by the user in the "meta_biv_sens_covariate_select" drop-down menu.
  covariate_selected_sensitivity <- reactive({
    return(input$meta_biv_sens_covariate_select)
  })
  
  ## Drop-down category menu contents: ##

  # The "categories" variable stores the names of all categories within the selected covariate:
  categories <- reactive({
    if (is.null(covariate_selected_sensitivity()) | is.null(data())){
      return(c("Select a covariate..."))
    }
    else{
      all_categories <- data()[1:nrow(data()),covariate_selected_sensitivity()]
      return(unique(all_categories))
    }
  })

  # The content of the "meta_biv_sens_category_select" drop-down menu is modified
  # by adding the choices within the "categories" variable:
  observe({
    updateSelectInput(session, "meta_biv_sens_category_select",
                      choices = categories()
    )})
  
  ## Selected category: ##

  # The "category_selected_sensitivity" variable stores the name of the category selected
  # by the user in the "meta_biv_sens_category_select" drop-down menu.
  category_selected_sensitivity <- reactive({
    return(input$meta_biv_sens_category_select)
  })
  
  # ---------------------------------------------------- #

  ## Subsetting the dataset according to the selected category of the selected covariate: ##

  # "data_sensitivity" is a subset of data() in which only the rows containing
  # the selected category for the selected covariate are included:
  data_sensitivity <- reactive({
    if (covariate_selected_sensitivity() %in% colnames(data())){
      subset <- split(data(), data()[, covariate_selected_sensitivity()])
      for (i in 1:length(subset)){
        if (subset[[i]][1,covariate_selected_sensitivity()] == category_selected_sensitivity()){
          return(subset[[i]])
        }
      }
    }
  })
  
  ## Evaluating convergence: ##
  
  # "model_select_sensitivity" stores the result of the "BRMA_model_select()" function,
  # which determines whether a bivariate model can be applied
  # (if the dataset has more than 4 instances and the model converges)
  # or a univariate model should be used.
  model_select_sensitivity <- reactive({
    if (is.null(data_sensitivity())) {
      return('NULL')
    }
    else if (nrow(data_sensitivity())<2) {
      return(FALSE)
    }
    else {
      return(BRMA_model_select(data_sensitivity())$model=="bivariate")
    }
  })
  
  # The "bivariate_subgroup" output is used in the ui.R script to define the 
  # conditionalPanel's condition in the subgroup analysis tab. 
  output$bivariate_sensitivity <- reactive({
    if (is.null(data())) {
      return('NULL')
    }
    else {
    return(model_select_sensitivity())
    }
  })
  outputOptions(output, 'bivariate_sensitivity', suspendWhenHidden=FALSE)
  
  # Table output corresponding to the selected subset of data:
  output$meta_biv_sens_data <- DT::renderDataTable({
    DT::datatable(data_sensitivity(), options = list(columnDefs = list(list(className = 'dt-center', targets = 2:ncol(data())-1)), 
                                                     lengthMenu = c(5, 10, 25, 50), 
                                                     pageLength = 5, 
                                                     orderClasses = TRUE), 
                                                     rownames = FALSE)
  })
  
  # ---------------------------------------------------- #
  
  ## Summary statistics: ##

  # The "meta_biv_stats" variable stores the first output 
  # of the "BRMA_statistics" function, summary statistics:
  meta_biv_sens_stats <- reactive({
    if (is.null(data_sensitivity()) | model_select_sensitivity() == FALSE) {
      return(NULL)
    }
    else {
      return(BRMA_statistics(data_sensitivity())[[1]])
    }
  })
  
  # Table output corresponding to the sensitivity analysis summary statistics:
  observeEvent(is.null(meta_biv_sens_stats())==FALSE, { # only necessary because otherwise the table wouldn't update correctly.
    output$meta_biv_sens_summary <- DT::renderDataTable({
      DT::datatable(meta_biv_sens_stats(), options = list(columnDefs = list(list(className = 'dt-center', targets = 1:3)), 
                                                          paging = FALSE,
                                                          searching = FALSE,
                                                          info = FALSE, 
                                                          ordering = FALSE))
    })
  })
  
  ## Downloading the table: ##
  output$dl_meta_biv_sens_summary <- downloadHandler(
    filename = function() {
      if (values$upload_state == "example") {
        paste("example_dataset", "biv_sens_summary.csv", sep = "_") 
      } else {
        paste(tools::file_path_sans_ext(input$file_upload), "biv_sens_summary.csv", sep = "_")
      }
    },
    content = function(file) {
      write.csv(meta_biv_sens_stats(), file, row.names = TRUE) 
    }
  )
  
  # ----------------------------------------------------------------------------------------------- #
  
  #### METANALYSIS - HSROC MODEL ####
  
  # Coming soon.
  
  observe({
    if (!is.null(data)){
      addCssClass(selector = "a[data-value='meta_hsroc_tab']", class = "inactiveLink2")
    }
  }) 
  
  
  # ----------------------------------------------------------------------------------------------- #
  
  #### SUMMARY OF FINDINGS ####
  
  ## Fields with validation for patient and prevalence inputs: ##
  
  iv1 <- InputValidator$new()
  iv1$add_rule("patients", sv_numeric())
  iv1$add_rule("patients", sv_gte(0))
  iv1$enable()
  
  iv2 <- InputValidator$new()
  iv2$add_rule("prevalence", sv_numeric())
  iv2$add_rule("prevalence", sv_between(0, 100))
  iv2$enable()
  
  ## Check for errors in the entered values: ##
  
  check_sumfind_table_error <- reactive({
    if (is.na(input$patients) | is.na(input$prevalence)){
      return(TRUE)
    }
    else {
      if (input$patients >= 0 & input$prevalence >= 0 & input$prevalence <= 100){
        return(FALSE)
      }
      else {
        return(TRUE)
      }
    }
  })
  
  # output to display the error message
  # (by activating the corresponding conditional panel in the ui.R script):
  output$sumfind_table_error <- reactive({
    return(check_sumfind_table_error())
  })
  outputOptions(output, 'sumfind_table_error', suspendWhenHidden=FALSE)
  
  # Using the "prevalencia" function to draw the table with the values entered by the user:
  sumfind_table <- reactive({
    if (is.null(data()) | check_sumfind_table_error() == TRUE) {
      return(NULL)
    }
    else {
      if (model_select()=="bivariate" & is.null(varcorr())){
        return(prevalencia(data(), 
                           model = "bivariate", 
                           prevslide = input$prevalence, 
                           input$patients))
      }
      else {
        return(prevalencia(data(), 
                           model = "univariate", 
                           prevslide = input$prevalence, 
                           input$patients))
      }
    }
  })
  
  ## Rendering the plot: ##
  
  output$sumfind_table <- renderPlot({
      sumfind_table()
  })
      
  ## Downloading the plot: ##
  
  # An "observe" environment is initialized to perform different actions
  # depending on what the user has selected as file format option (.png or .svg):
  observe({

    # Downlading a .png file:
    if (input$dl_sumfind_options == ".png") {
      output$dl_sumfind <- downloadHandler(
        filename = function(){
          if (values$upload_state == "example") {
            paste("example_dataset", "summary_of_findings.png", sep = "_") 
          } else {
            paste(tools::file_path_sans_ext(input$file_upload), "summary_of_findings.png", sep = "_") 
          }
        },
        content = function(file) {
          ggsave(file, plot = sumfind_table(), device = "png", width = 8.5, height = 6.5)
        })
    }

    # Downlading a .svg file:
    if (input$dl_sumfind_options == ".svg") {
      output$dl_sumfind <- downloadHandler(
        filename = function(){
          if (values$upload_state == "example") {
            paste("example_dataset", "summary_of_findings.svg", sep = "_") 
          } else {
            paste(tools::file_path_sans_ext(input$file_upload), "summary_of_findings.svg", sep = "_")
          }
        },
        content = function(file) {
          ggsave(file, plot = sumfind_table(), device = "svg", width = 8.5, height = 6.5)
        })
    }
  })
  
})  


########################################################################################
#################################### UNUSED CODE #######################################
########################################################################################
#
# ## Splitting the dataset according to the value of the selected covariate: ##
# 
# # "data_split" is a list in which partitions (based on the covariate selected by the user) 
# # of the original dataset are stored.
# data_split_sub <- reactive({
#   if (covariate_selected_subgroup() %in% colnames(data())){
#     return(split(data(), data()[, covariate_selected_subgroup()]))
#   }
# })
# 
# 
# ## Evaluating stability of the model: ##
# 
# output$bivariate_stability_subgroup <- reactive({
#   if (is.null(data_split_sub())) {
#     return(NULL)
#   }
#   else {
#     stable <- TRUE
#     for (i in 1:length(data_split_sub())){
#       if (is.na(BRMA_statistics(data_split_sub()[[i]])[[2]][8])){
#         stable <- FALSE
#       }
#       else if (BRMA_statistics(data_split_sub()[[i]])[[2]][3] == 0){
#         unstable <- FALSE
#       }
#       else if (BRMA_statistics(data_split_sub()[[i]])[[2]][4] == 0){
#         stable <- FALSE
#       }
#     }
#   }  
#   return((is.null(model_select_subgroup()) | model_select_subgroup()==0) & stable)
# })
# outputOptions(output, 'bivariate_stability_subgroup', suspendWhenHidden=FALSE)
# 
# 
# ## Tables: ##
# 
# stats_summary_sub <- reactive({
#   merge_list <- list()
#   for (i in 1:length(data_split_sub())){
#     merge_list[[i]] <- BRMA_statistics(data_split_sub()[[i]])[[1]]
#     colnames(merge_list[[i]])[1:ncol(merge_list[[i]])] <- paste(colnames(merge_list[[i]])[1:ncol(merge_list[[i]])], data_split_sub()[[i]][1,covariate_selected_subgroup()], sep = " ")
#   }
#   merged_table <- do.call(cbind, merge_list)
#   return(merged_table)
# })
# stats_revman_sub <- reactive({
#   merge_list <- list()
#   for (i in 1:length(data_split_sub())){
#     merge_list[[i]] <- BRMA_statistics(data_split_sub()[[i]])[[2]]
#     colnames(merge_list[[i]])[1:ncol(merge_list[[i]])] <- paste(colnames(merge_list[[i]])[1:ncol(merge_list[[i]])], data_split_sub()[[i]][1,covariate_selected_subgroup()], sep = " ")
#   }
#   merged_table <- do.call(cbind, merge_list)
#   return(merged_table)
# })
# stats_heterogeneity_sub <- reactive({
#   merge_list <- list()
#   for (i in 1:length(data_split_sub())){
#     merge_list[[i]] <- BRMA_statistics(data_split_sub()[[i]])[[3]]
#     colnames(merge_list[[i]])[1:ncol(merge_list[[i]])] <- paste(colnames(merge_list[[i]])[1:ncol(merge_list[[i]])], data_split_sub()[[i]][1,covariate_selected_subgroup()], sep = " ")
#   }
#   merged_table <- do.call(cbind, merge_list)
#   return(merged_table)
# })
#
# # Heterogeneity:
# output$meta_biv_sub_heterogeneity <- DT::renderDataTable({
#   DT::datatable(stats_heterogeneity_sub(), options = list(columnDefs = list(list(className = 'dt-center', targets = 1:ncol(stats_heterogeneity_sub()))), # center align all columns except first.
#                                                           paging = FALSE,
#                                                           searching = FALSE,
#                                                           info = FALSE, 
#                                                           ordering = FALSE
#                                                           ))
# })
# 
# output$dl_meta_biv_sub_heterogeneity <- downloadHandler(
#   filename = function() {
#     paste(tools::file_path_sans_ext(input$file_upload), "biv_sub_heterogeneity.csv", sep = "_") # Name of the downloaded file ("tools::file_path_sans_ext" removes extensions).
#   },
#   content = function(file) {
#     write.csv(stats_heterogeneity_sub(), file, row.names = TRUE) # The table is written as a .csv file.
#   }
# )