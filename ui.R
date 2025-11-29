# Required R packages:
library(shiny) # Rshiny package.
library(shinyjs) # for javascript code.
library(shinydashboard) # for layout purposes.
library(plotly) # for graph building and rendering.
library(DT) # for table building and rendering.
library(shinycssloaders) # for "Loading..." animations.

# ----------------------------------------------------------------------------------------------- #

# Start of the app UI:
shinyUI(tagList( 

  useShinyjs(),
  
  tags$style(HTML(".inactiveLink1 {
                    pointer-events: none;
                    cursor: not-allowed;
                    background-color: #324249 !important;
                    border-color: #324249 !important;}"
                   )), # CSS code to characterize the "inactive" class that will be assigned to the sidebar tabs that cannot be accessible.
  
  tags$style(HTML(".inactiveLink2 {
                    pointer-events: none;
                    cursor: not-allowed !important;
                    color: #CCCCCC !important;}"
                   )), # CSS code to characterize the "inactive" class that will be assigned to the metanalysis tabs that cannot be accessible.  
  
  tags$style(HTML(".modal.in .modal-dialog{width:auto; height:auto; margin:auto;}
                   .modal-content{width:auto; height:auto; background-color:#eeeeee; }
                   .modal-footer{display:none} 
                   .modal {position:absolute; top:28%; left:40%; transform: translate(-33%, -20%);}"
                   )), # CSS code to make the landing page. 
                       # .modal-footer{display:none} to avoid displaying the default "dismiss" button.
  
  tags$style(HTML(".shiny-notification {
                   position:fixed;
                   top: 500px;
                   left: 215px;
                   height: 80px;
                   width: 55%;}"
                   )), # CCS code to adjust the position and size of the notifications box.
  
    tags$head(tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "RyC.png")), # Browser tab icon.
  
    dashboardPage(skin = "blue", # Color of the app's dashboard.
                           
    # HEADER content and format:                                                                                                                          
    dashboardHeader(title="Meta-DiSc 2.0",
                    titleWidth = 200),
    
    # SIDEBAR content and format:
    dashboardSidebar(
              width = 200,
                
              # Sidebar menu contents:
              sidebarMenu(id = "sidebar_tabs",
                 
                 # First item: FILE UPLOAD
                 menuItem("File upload", # Name that will appear on the corresponding item.
                          tabName = "file_tab", # Name that will be used to call (internally) the corresponding item.
                          icon = icon("file-upload") # Icon that will appear, alongside the name, on the corresponding item (to find the different icons available visit: https://fontawesome.com/).
                          ),
                 
                 # Second item: GRAPHICAL DESCRIPTION
                 menuItem("Graphical description", 
                          tabName ="graph_tab", 
                          icon = icon("chart-pie")
                          ),
                 
                 # Third item: METANALYSIS
                 menuItem("Meta-analysis", 
                          tabName ="meta_tab", 
                          icon = icon("chart-bar")
                          ),
                 
                 # Third item: METANALYSIS
                 menuItem("Summary of findings", 
                          tabName ="sumfind_tab", 
                          icon = icon("list-ul")
                          ),
                 
                 # Fourth item: USER GUIDE
                 menuItem("User guide", 
                          tabName ="user_tab", 
                          icon = icon("info-circle")
                 )),
              
              # IMAGE at the bottom of the sidebar (the image is placed within a "div" that is positioned at the bottom of the page):
              tags$div(style = "width:100%; 
                                position: absolute;
                                bottom:50px;",
                       
                       tags$img(src = 'RyC.png', # The image must be saved in a folder called "www" located in the folder where the rest of the app files are stored.
                                alt = "Busto de Ramón y Cajal", # The text to be displayed in case the image cannot be loaded.
                                style = "display: block;
                                         margin-left: auto;
                                         margin-right: auto;
                                         width:70%;"
                                )
                       )
              ),
    
    # BODY content and format (for each of the tabs, separately):
    dashboardBody(
      
      # CSS code to change the default shiny dashboard blue skin colors (top horizontal line and sidebar):
      tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
        background-color: #1f4c6a;
        }
        
        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
        background-color: #1a415b;
        }
        
        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
        background-color: #235779;
        }        
        
        /* main sidebar */
        .skin-blue .main-sidebar {
        background-color: #222d32;
        }
        
        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
        background-color: #1a2226;
        border-left-color: #235779;
        }
        
        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
        background-color: #222d32;
        color: #ffffff;
        }
        
        /* other links in the sidebarmenu when hovered */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
        background-color: #1a2226;
        border-left-color: #235779;
        }

        /* toggle button when hovered  */                    
        .skin-blue .main-header .navbar .sidebar-toggle:hover{
        background-color: #1f4c6a;
        }
        
        /* box header */   
        .box.box-solid.box-success>.box-header {
        color:#fff;
        background:#235779
        }
        
        /* box border */
        .box.box-solid.box-success{
        border-bottom-color:#235779;
        border-left-color:#235779;
        border-right-color:#235779;
        border-top-color:#235779;
        }
        '))),

      
      # Other CSS customizations:
      tags$style(HTML(".nav-tabs-custom .nav-tabs li.active {border-top-color: #235779; font-weight:bold; font-size:16px;}"
                      )), # CSS code to customize the color and font of the top horizontal tabs when selected.
      tags$style(HTML(".tabbable > .nav > li[class=active]>a {background-color: #235779; border-top-color:white; color:white}"
                      )), # CSS code to customize the color of the bottom horizontal tabs when selected.
      tags$style(HTML(".box-header h3.box-title {font-size:16px;}"
                      )), # CSS code to customize the font size of box titles.
      tags$style(HTML(
        ".nav-tabs-custom .nav-tabs a[data-value='meta_biv_sub_summary_stats_tab']{font-size:14px;}
         .nav-tabs-custom .nav-tabs li.active a[data-value='meta_biv_sub_summary_stats_tab']{border-top-color: #235779; font-weight:bold; font-size:14px;}
         .nav-tabs-custom .nav-tabs a[data-value='meta_biv_sub_revman_tab']{font-size:14px;}
         .nav-tabs-custom .nav-tabs li.active a[data-value='meta_biv_sub_revman_tab']{border-top-color: #235779; font-weight:bold; font-size:14px;}
         .nav-tabs-custom .nav-tabs a[data-value='meta_biv_sub_metareg_tab']{font-size:14px;}
         .nav-tabs-custom .nav-tabs li.active a[data-value='meta_biv_sub_metareg_tab']{border-top-color:#235779; font-weight:bold; font-size:14px;}
         .nav-tabs-custom .nav-tabs a[data-value='meta_biv_sub_heterogeneity_tab']{font-size:14px;}
         .nav-tabs-custom .nav-tabs li.active a[data-value='meta_biv_sub_heterogeneity_tab']{border-top-color: #235779; font-weight:bold; font-size:14px;}
        "
          )), # CSS code to customize the color and font of the "Subgroup analysis" tabs. 
      
      tabItems(
        
        # Content for the FILE UPLOAD tab:
        tabItem(tabName="file_tab",
                fluidRow(
                   column(width = 12,
                          box(width = "100%",
                              height = 100,
                              column(width = 9,
                                      fileInput(inputId = "file_upload",
                                                label = "Select file:",
                                                multiple = FALSE,
                                                accept = c("text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv", ".xlsx", ".xls"),
                                                buttonLabel = "Browse",
                                                placeholder = "No file selected..."
                                                )),
                              
                              column(width = 2,
                                     fluidRow(
                                       column(width = 4,
                                              radioButtons(inputId = "file_upload_options1",
                                                           label = "Format:",
                                                           choices = c(".xlsx", ".csv"),
                                                           selected = ".xlsx",
                                                           inline = FALSE)
                                              ),
                                       column(width = 8,
                                               radioButtons(inputId = "file_upload_options2",
                                                            label = "Delimiter:",
                                                            choices = c("Comma", "Semicolon"),
                                                            selected = "Comma",
                                                            inline = FALSE)
                                                               
                                              )
                                              )
                                     ),
        
                              column(width = 1, 
                                      actionButton(inputId = "file_reset",
                                                   label = "Reset",
                                                   style = "position:relative;
                                                            top:25px;"
                                                   )
                                     )
                              )
                          )
                         ),
                
                fluidRow(
                  column(width = 8,
                         box(width = "100%",
                             style = "overflow-x: scroll;overflow-y:hidden",
                             conditionalPanel(condition = "output.file_not_uploaded == true",
                                              tags$h4(tags$strong("File upload instructions")),
                                              tags$p("Before uploading a dataset, ", tags$strong("select a format"), "for the file: '.csv' or '.xlsx'."),
                                              tags$p("If you want to upload a '.csv' file, you will be asked to ", tags$strong("choose a delimiter"), ": 'comma' (,) or 'semicolon' (;)."),
                                              tags$p("The 'comma' delimiter implies that a ",  tags$strong("period (.)"), " is used to indicate decimals."),
                                              tags$p("The 'semicolon' as delimiter implies that a ", tags$strong("comma (,)"), " is used to indicate decimals."),
                                              tags$p("The file must contain the following columns: ", tags$strong("ID, TP, FP, TN, FN"), ". It is important that these columns have the specified names so that the program can recognize them.", tags$strong("Order as well as upper or lower case are not important"), ". In addition, the file may contain additional columns that will be identified as covariates."),
                                              actionButton("ex_dataset", "Load an example dataset")
                                              ),
                             withSpinner(DT::dataTableOutput(outputId = "file_table"), 
                                                             type = 6,
                                                             size = 0.5,
                                                             color = "#235779")
                            )
                         ),
                
                  column(width = 4,
                        conditionalPanel(condition = "output.file_not_uploaded == false",
                                         box(width = "100%",
                                             style = "overflow-x: scroll;",
                                             title = "Data summary",
                                             status = "success",
                                             solidHeader = TRUE,
                                             collapsible = TRUE,
                                             withSpinner(DT::dataTableOutput(outputId = "file_dataset_summary"),
                                                                             type = 6,
                                                                             size = 0.5,
                                                                             color = "#235779"),
                                             tags$br(),
                                             tags$div(style="float:right;",
                                                      downloadButton("dl_data_summary", "Download"))
                                             ),
                                         conditionalPanel(condition = "output.senspe0_warning == true",
                                                          box(status = "warning",
                                                              background = "orange",
                                                              width = "100%",
                                                              uiOutput("senspe0_message")))
                                          )
                         )
                        )
                ),
        
        # Content for the GRAPHICAL DESCRIPTION tab:
        tabItem(tabName = "graph_tab",
                fluidRow(
                  column(width = 12,
                         box(width = "100%",
                             selectInput(inputId = "graph_covariate_select",
                                         label = "Select a covariate if you want to perform a graphical description by subgroups:",
                                         choices = ""
                                         )
                            )
                         )
                         ),
                
                tabBox(id = "graph_tabbox",
                       width = "100%",
                       height = "auto",
                       tabPanel("Forest plots",
                                id = "graph_forest_plots_tab",
                                value = "graph_forest_plots_tab",
                                fluidRow(
                                  column(width = 6,
                                         box(width = "100%",
                                             style = "overflow-x: scroll;",
                                             title = "Sensitivity",
                                             status = "success",
                                             solidHeader = TRUE,
                                             collapsible = TRUE,
                                             conditionalPanel("output.file_not_uploaded == true",
                                                              tags$p("Upload a dataset!")),
                                             conditionalPanel("output.file_not_uploaded == false",
                                                              tags$div(align = "center",
                                                                       withSpinner(uiOutput("graph_forest_sen"),
                                                                                   type = 6,
                                                                                   size = 0.5,
                                                                                   color = "#235779")),
                                                              tags$br(),
                                                              tags$div(style="display:inline-block; float:right;", 
                                                              downloadButton("dl_graph_forest_sen", "Download")
                                                              ),
                                                              tags$div(style="display:inline-block; vertical-align:top; width:120px; float:right;",
                                                                       radioButtons(inputId = "dl_forest_sen_options",
                                                                                    label = NULL,
                                                                                    choices = c(".png", ".svg"),
                                                                                    selected = ".png",
                                                                                    inline = TRUE)
                                                              )
                                                              
                                             ) 
                                            )
                                         ),
                                  
                                  column(width = 6,
                                         box(width = "100%",
                                             style = "overflow-x: scroll;",
                                             title = "Specificity",
                                             status = "success",
                                             solidHeader = TRUE,
                                             collapsible = TRUE,
                                             conditionalPanel("output.file_not_uploaded == true",
                                                              tags$p("Upload a dataset!")),
                                             conditionalPanel("output.file_not_uploaded == false",
                                                              tags$div(align = "center",
                                                                       withSpinner(uiOutput("graph_forest_spe"),
                                                                                   type = 6,
                                                                                   size = 0.5,
                                                                                   color = "#235779")),
                                                              tags$br(),
                                                              tags$div(style="display:inline-block; float:right;", 
                                                                       downloadButton("dl_graph_forest_spe", "Download")
                                                              ),
                                                              tags$div(style="display:inline-block; vertical-align:top; width:120px; float:right;",
                                                                       radioButtons(inputId = "dl_forest_spe_options",
                                                                                    label = NULL,
                                                                                    choices = c(".png", ".svg"),
                                                                                    selected = ".png",
                                                                                    inline = TRUE)
                                                              )
                                                              
                                             ) 
                                         )
                                  )
                                )
                       ),
                       
                       tabPanel("ROC plane",
                                id = "graph_ROC_plane_tab",
                                fluidRow(
                                column(width = 7,
                                       box(width = "100%",
                                           style = "overflow-x: scroll;",
                                           title = "ROC plane",
                                           status = "success",
                                           solidHeader = TRUE,
                                           collapsible = TRUE,
                                           conditionalPanel("output.file_not_uploaded == true",
                                                            tags$p("Upload a dataset!")),
                                           conditionalPanel("output.file_not_uploaded == false",
                                                            
                                                            # The plot is placed inside a "div" so it can be centered:
                                                            tags$div(style="display:inline-block; float:left; font-weight: bold; width:230px;", 
                                                                     "Display confidence interval bars:"),
                                                            tags$div(style="display:inline-block; float:left;", 
                                                            radioButtons(inputId = "graph_roc_ci_display",
                                                                         label = NULL,
                                                                         choices = c("Yes" = TRUE,
                                                                                     "No" = FALSE),
                                                                         selected = FALSE,
                                                                         inline = TRUE)),
                                                            tags$br(),
                                                            tags$br(),
                                                            tags$div(align = "center", 
                                                                     withSpinner(plotlyOutput("graph_roc", height = "600px"),
                                                                                type = 6,
                                                                                size = 0.5,
                                                                                color = "#235779")),
                                                            tags$br(),
                                                            tags$div(style="display:inline-block; float:right;", 
                                                                     downloadButton("dl_graph_roc", "Download")
                                                                     ),
                                                            tags$div(style="display:inline-block; vertical-align:top; width:120px; float:right;",
                                                                     radioButtons(inputId = "dl_graph_roc_options",
                                                                                  label = NULL,
                                                                                  choices = c(".png", ".svg"),
                                                                                  selected = ".png",
                                                                                  inline = TRUE)
                                                                     )
                                                            )
                                           )
                                       )
                                       )
              )
            )
        ),
        
        # Content for the METANALYSIS tab:
        tabItem(tabName = "meta_tab",
        tabBox(id = "meta_tabbox",
               width = "100%",
               height = "auto",
                    
                    tabPanel("Bivariate model",
                             id = "meta_biv_tab",
                             value = "meta_biv_tab",
                             tabsetPanel(type = "pills",
                                         id = "meta_biv_tabset",
                                         
                                         tabPanel("Statistics",
                                                  id = "meta_biv_stats_tab",
                                                  value = "meta_biv_stats_tab",
                                                  tags$hr(),
                                                  conditionalPanel(condition = "output.bivariate == true",
                                                  
                                                    conditionalPanel(condition = "output.varcorr_recommend == true",
                                                                     fluidRow(
                                                                       column(width = 12,
                                                                              box(status = "warning",
                                                                                  background = "orange",
                                                                                  width = "80%",
                                                                                  uiOutput("varcorr_message"))))),
                                                    fluidRow(
                                                    column(width = 4,
                                                           box(width = "100%",
                                                               style = "overflow-x: scroll;",
                                                               title = "Summary statistics",
                                                               status = "success",
                                                               solidHeader = TRUE,
                                                               collapsible = TRUE,
                                                               withSpinner(DT::dataTableOutput(outputId = "meta_biv_stats_summary"), 
                                                                                               type = 6,
                                                                                               size = 0.5,
                                                                                               color = "#235779"),
                                                               tags$br(),
                                                               tags$div(style="float:right;",
                                                                        downloadButton("dl_meta_biv_stats_summary", "Download"))
                                                               )   
                                                           ),
                                                    
                                                    column(width = 4,
                                                           box(width = "100%",
                                                               style = "overflow-x: scroll;",
                                                               title = "RevMan parameters",
                                                               status = "success",
                                                               solidHeader = TRUE,
                                                               collapsible = TRUE,
                                                               withSpinner(DT::dataTableOutput(outputId = "meta_biv_stats_revman"), 
                                                                                               type = 6,
                                                                                               size = 0.5,
                                                                                               color = "#235779"),
                                                               tags$br(),
                                                               tags$div(style="float:right;",
                                                                        downloadButton("dl_meta_biv_stats_revman", "Download"))
                                                               )
                                                           ),
                                                    
                                                    column(width = 4,
                                                           box(width = "100%",
                                                               style = "overflow-x: scroll;",
                                                               title = "Heterogeneity",
                                                               status = "success",
                                                               solidHeader = TRUE,
                                                               collapsible = TRUE,
                                                               withSpinner(DT::dataTableOutput(outputId = "meta_biv_stats_heterogen"), 
                                                                                               type = 6,
                                                                                               size = 0.5,
                                                                                               color = "#235779"),
                                                               tags$br(),
                                                               tags$div(style="float:right;",
                                                                        downloadButton("dl_meta_biv_stats_heterogen", "Download"))
                                                               )
                                                           )
                                                           )
                                                  ),
                                                  
                                                  conditionalPanel(condition = "output.bivariate == false",
                                                                   fluidRow(
                                                                     column(width = 12,
                                                                           box(status = "warning",
                                                                               background = "orange",
                                                                               width = "80%",
                                                                               uiOutput("univariate_message")
                                                                               )
                                                                           )
                                                                           )
                                                          )
                                                  ),
                                         
                                         tabPanel("SROC curve", 
                                                  id = "meta_biv_sroc_tab",
                                                  value = "meta_biv_sroc_tab",
                                                  tags$hr(),
                                                  fluidRow(
                                                    column(width = 8,
                                                           box(width = "100%",
                                                               style = "overflow-x: scroll;",
                                                               title = "SROC curve",
                                                               status = "success",
                                                               solidHeader = TRUE,
                                                               collapsible = TRUE,
                                                               
                                                               # The plot is placed inside a "div" so it can be centered:
                                                               tags$div(align="center", 
                                                                        withSpinner(plotlyOutput("meta_biv_sroc_curve", height = "600px"),
                                                                                     type = 6,
                                                                                     size = 0.5,
                                                                                     color = "#235779")),
                                                               
                                                               tags$div(style="display:inline-block; float:right;", 
                                                                        downloadButton("dl_meta_biv_sroc_curve", "Download")
                                                                        ),
                                                               tags$div(style="display:inline-block; vertical-align:top; width:120px; float:right;",
                                                                        radioButtons(inputId = "dl_meta_biv_sroc_curve_options",
                                                                                     label = NULL,
                                                                                     choices = c(".png", ".svg"),
                                                                                     selected = ".png",
                                                                                     inline = TRUE)
                                                                        )
                                                               )   
                                                           ),
                                                    
                                                    column(width = 4,
                                                           box(width = "100%",
                                                               style = "overflow-x: scroll;",
                                                               title = "SROC display options",
                                                               status = "success",
                                                               solidHeader = TRUE,
                                                               collapsible = TRUE,
                                                               checkboxGroupInput(inputId = "meta_biv_sroc_options",
                                                                                  label = "Select the features you would like to display:",
                                                                                  choices = c("Summary point" = "sum_point",
                                                                                              "Confidence ellipse" = "conf_ellip",
                                                                                              "Prediction ellipse" = "pred_ellip",
                                                                                              "Curve" = "curve",
                                                                                              "Points" = "points"
                                                                                              ),
                                                                                  selected = c("sum_point", "conf_ellip", "pred_ellip")
                                                                                  )
                                                               )
                                                           )
                                                           )
                                                 ),
                                         
                                        tabPanel("Subgroup analysis", 
                                                 id = "meta_biv_sub_tab",
                                                 value = "meta_biv_sub_tab",
                                                 tags$hr(),
                                                 fluidRow(
                                                   column(width = 6,
                                                          selectInput(inputId = "meta_biv_sub_covariate_select",
                                                                      label = "Select a covariate to perform a subgroup analysis:",
                                                                      choices = "",
                                                                      width = "100%"
                                                                      )
                                                          ),
                                                   column(width = 6,
                                                          checkboxGroupInput(inputId = "meta_biv_sub_options",
                                                                             label = "Select the features you would like to display:",
                                                                             choices = c("Summary point" = "sum_point",
                                                                                         "Confidence ellipse" = "conf_ellip",
                                                                                         "Prediction ellipse" = "pred_ellip",
                                                                                         "Curve" = "curve",
                                                                                         "Points" = "points"
                                                                                         ),
                                                                             selected = c("sum_point", "conf_ellip", "pred_ellip"),
                                                                             inline = TRUE
                                                                              )
                                                            )
                                                          ),
                                                 
                                                 conditionalPanel(condition = "output.subgroup_categories == true",
                                                                  fluidRow(
                                                                    column(width = 12,
                                                                           box(status = "warning",
                                                                               background = "orange",
                                                                               width = "80%",
                                                                               HTML("<p style='text-align:center';><i class='fa fa-exclamation-circle'></i> The selected covariate has only one category (in which case the subgroup analysis is not applicable) or more than two categories. Currently Meta-DiSc 2.0 does not perform subgroup analysis for more than two categories. Please select another covariate.</p>")))
                                                                  )),
                                                 
                                                 conditionalPanel(condition = "output.subgroup_categories == false",
                                                 
                                                 conditionalPanel(condition = "output.bivariate_subgroup == false",
                                                                  fluidRow(
                                                                  column(width = 12,
                                                                  box(status = "warning",
                                                                      background = "orange",
                                                                      width = "80%",
                                                                      HTML("<p style='text-align:center';><i class='fa fa-exclamation-circle'></i> One or more subgroups has less than 4 studies or did not reach convergence for the bivariate model. Please select another covariate for subgroup analysis.</p>")))
                                                                  )),

                                                 conditionalPanel(condition = "output.bivariate_subgroup == true",
                                                 fluidRow(
                                                   column(width = 6,
                                                          box(width = "100%",
                                                              style = "overflow-x: scroll;",
                                                              title = "Statistics",
                                                              status = "success",
                                                              solidHeader = TRUE,
                                                              collapsible = TRUE,
                                                              
                                                              tabBox(width = "100%",
                                                                     id = "subgroup_tabbox",
                                                                     
                                                                     tabPanel("Summary statistics",
                                                                              id = "meta_biv_sub_summary_stats_tab",
                                                                              value = "meta_biv_sub_summary_stats_tab",
                                                                              fluidRow(
                                                                                column(width = 12,
                                                                                       style = "overflow-x: scroll;",
                                                                                       
                                                                                       withSpinner(DT::dataTableOutput("meta_biv_sub_summary"),
                                                                                                   type = 6,
                                                                                                   size = 0.5,
                                                                                                   color = "#235779"),
                                                                                       tags$br(),
                                                                                       tags$div(style="float:right;",
                                                                                                downloadButton("dl_meta_biv_sub_summary", "Download"))
                                                                                       )
                                                                                       )
                                                                              ),
                                                              
                                                                     tabPanel("RevMan parameters",
                                                                              id = "meta_biv_sub_revman_tab",
                                                                              value = "meta_biv_sub_revman_tab",
                                                                              fluidRow(  
                                                                                column(width = 12,
                                                                                       style = "overflow-x: scroll;",
                                                                                       withSpinner(DT::dataTableOutput("meta_biv_sub_revman"),
                                                                                                   type = 6,
                                                                                                   size = 0.5,
                                                                                                   color = "#235779"),
                                                                                       tags$p("* The model has been fitted assuming that the variances of the two categories are equal."),
                                                                                       tags$br(),
                                                                                       tags$div(style="float:right;",
                                                                                                downloadButton("dl_meta_biv_sub_revman", "Download"))
                                                                                       )
                                                                                       )
                                                                              ),
                                                            
                                                                     tabPanel("Metaregression",
                                                                              id = "meta_biv_sub_metareg_tab",
                                                                              value = "meta_biv_sub_metareg_tab",
                                                                              fluidRow( 
                                                                                column(width = 12,
                                                                                       style = "overflow-x: scroll;",
                                                                                       withSpinner(DT::dataTableOutput("meta_biv_sub_metareg"),
                                                                                                   type = 6,
                                                                                                   size = 0.5,
                                                                                                   color = "#235779"),
                                                                                       tags$br(),
                                                                                       tags$div(style="float:right;",
                                                                                                downloadButton("dl_meta_biv_sub_metareg", "Download"))
                                                                                )
                                                                              )
                                                                     )
                                                                     )   
                                                           )
                                                          ),
                                                
                                                   column(width = 6,
                                                          box(width = "100%",
                                                              style = "overflow-x: scroll;",
                                                              title = "SROC curve",
                                                              status = "success",
                                                              solidHeader = TRUE,
                                                              collapsible = TRUE,
                                                              conditionalPanel(condition = "output.sub_roc_warning == true",
                                                              withSpinner(plotlyOutput("meta_biv_sub_sroc"),
                                                                          type = 6,
                                                                          size = 0.5,
                                                                          color = "#235779"),
                                                              
                                                              tags$div(style="display:inline-block; float:right;",  
                                                                       downloadButton("dl_meta_biv_sub_sroc", "Download")),
                                                      
                                                              tags$div(style="display:inline-block; vertical-align:top; width:120px; float:right;",
                                                                       radioButtons(inputId = "dl_meta_biv_sub_sroc_options",
                                                                                    label = NULL,
                                                                                    choices = c(".png", ".svg"),
                                                                                    selected = ".png",
                                                                                    inline = TRUE))),
                                                              conditionalPanel(condition = "output.sub_roc_warning == false",
                                                                               box(status = "warning",
                                                                                   background = "orange",
                                                                                   width = "80%",
                                                                                   HTML("<i class='fa fa-exclamation-circle'></i> One of the categories for the selected covariate has less than 3 studies. The SROC curve cannot be computed."))
                                                                              )
                                                                  )
                                                              )
                                                          )
                                                 ))
                                                 ),
                                        
                                        tabPanel("Sensitivity analysis", 
                                                 id = "meta_biv_sens_tab",
                                                 value = "meta_biv_sens_tab",
                                                 tags$hr(),
                                                 fluidRow(
                                                   column(width = 6,
                                                          selectInput(inputId = "meta_biv_sens_covariate_select",
                                                                      label = "Select a covariate to perform a sensitivity analysis:",
                                                                      choices = "",
                                                                      width = "100%"
                                                                      )
                                                          ),
                                                   column(width = 6,
                                                          selectInput(inputId = "meta_biv_sens_category_select",
                                                                      label = "Select a category within the selected covariate to perform a sensitivity analysis:",
                                                                      choices = "",
                                                                      width = "100%"
                                                                      )
                                                          )
                                                          ),
                                                 
                                                 fluidRow(
                                                   column(width = 8,
                                                          box(width = "100%",
                                                              style = "overflow-x: scroll;",
                                                              title = "Dataset",
                                                              status = "success",
                                                              solidHeader = TRUE,
                                                              collapsible = TRUE,
                                                              withSpinner(DT::dataTableOutput(outputId = "meta_biv_sens_data"), 
                                                                          type = 6,
                                                                          size = 0.5,
                                                                          color = "#235779")
                                                              )
                                                          ),
                                                   
                                                   column(width = 4,
                                                          box(width = "100%",
                                                              style = "overflow-x: scroll;",
                                                              title = "Summary statistics",
                                                              status = "success",
                                                              solidHeader = TRUE,
                                                              collapsible = TRUE,
                                                              conditionalPanel(condition = "output.bivariate_sensitivity == true",
                                                              withSpinner(DT::dataTableOutput(outputId = "meta_biv_sens_summary"), 
                                                                          type = 6,
                                                                          size = 0.5,
                                                                          color = "#235779"),
                                                              tags$br(),
                                                              tags$div(style="float:right;",
                                                                       downloadButton("dl_meta_biv_sens_summary", "Download"))),
                                                              conditionalPanel(condition = "output.bivariate_sensitivity == false",
                                                                                      box(status = "warning",
                                                                                          background = "orange",
                                                                                          width = "80%",
                                                                                          HTML("<i class='fa fa-exclamation-circle'></i> The selected data subset has less than 4 studies or did not achieve convergence for the bivariate model. Please select another data subset."))
                                                                               )
                                                              )
                                                          )
                                                          )
                                                 )
                                         )
                             ),
               
                    tabPanel("Univariate model",
                             id = "meta_univariate_tab",
                             value = "meta_univariate_tab",
                             
                             fluidRow(
                               column(width = 5,
                                      box(width = "100%",
                                          title = "Summary statistics (univariate)",
                                          status = "success",
                                          solidHeader = TRUE,
                                          collapsible = TRUE,
                                          withSpinner(DT::dataTableOutput(outputId = "meta_uni_stats_summary"), 
                                                      type = 6,
                                                      size = 0.5,
                                                      color = "#235779"),
                                          tags$br(),
                                          tags$div(style="float:right;",
                                                   downloadButton("dl_meta_uni_stats_summary", "Download"))
                                      ),
                                      
                                      box(width = "100%",
                                          title = "Heterogeneity (univariate)",
                                          status = "success",
                                          solidHeader = TRUE,
                                          collapsible = TRUE,
                                          withSpinner(DT::dataTableOutput(outputId = "meta_uni_stats_heterogen"), 
                                                      type = 6,
                                                      size = 0.5,
                                                      color = "#235779"),
                                          tags$br(),
                                          tags$div(style="float:right;",
                                                   downloadButton("dl_meta_uni_stats_heterogen", "Download"))
                                      )
                               ),
                               
                               column(width=7,
                                      box(width = "100%",
                                          style = "overflow-x: scroll;",
                                          title = "Sensitivity Forest Plot",
                                          status = "success",
                                          solidHeader = TRUE,
                                          collapsible = TRUE,
                                          tags$div(align = "center",
                                                   withSpinner(uiOutput("uni_forest_sen"),
                                                               type = 6,
                                                               size = 0.5,
                                                               color = "#235779")),
                                          tags$br(),
                                          tags$div(style="display:inline-block; float:right;", 
                                                   downloadButton("dl_uni_forest_sen", "Download")
                                          ),
                                          tags$div(style="display:inline-block; vertical-align:top; width:120px; float:right;",
                                                   radioButtons(inputId = "dl_uni_forest_sen_options",
                                                                label = NULL,
                                                                choices = c(".png", ".svg"),
                                                                selected = ".png",
                                                                inline = TRUE)
                                          )
                                      ),
                                      
                                      box(width = "100%",
                                          style = "overflow-x: scroll;",
                                          title = "Specificity Forest Plot",
                                          status = "success",
                                          solidHeader = TRUE,
                                          collapsible = TRUE,
                                          tags$div(align = "center",
                                                   withSpinner(uiOutput("uni_forest_spe"),
                                                               type = 6,
                                                               size = 0.5,
                                                               color = "#235779")),
                                          tags$br(),
                                          tags$div(style="display:inline-block; float:right;", 
                                                   downloadButton("dl_uni_forest_spe", "Download")
                                          ),
                                          tags$div(style="display:inline-block; vertical-align:top; width:120px; float:right;",
                                                   radioButtons(inputId = "dl_uni_forest_spe_options",
                                                                label = NULL,
                                                                choices = c(".png", ".svg"),
                                                                selected = ".png",
                                                                inline = TRUE)
                                          )
                                      )
                               )
                             )
                    ),
                    
                    tabPanel("HSROC model",
                             id = "meta_hsroc_tab",
                             value = "meta_hsroc_tab",
                             tabsetPanel(type = "pills",
                                         
                                         tabPanel("Statistics",
                                                  id = "meta_hsroc_stats_tab",
                                                  tags$hr(),
                                                  fluidRow(
                                                    column(width = 4,
                                                           box(width = "100%",
                                                               title = "Summary statistics",
                                                               status = "success",
                                                               solidHeader = TRUE,
                                                               collapsible = TRUE,
                                                               tableOutput("meta_hsroc_stats_summary"),
                                                               tags$div(style="float:right;",
                                                                        downloadButton("dl_meta_hsroc_stats_summary", "Download"))
                                                           )   
                                                    ),
                                                    
                                                    column(width = 4,
                                                           box(width = "100%",
                                                               title = "RevMan parameters",
                                                               status = "success",
                                                               solidHeader = TRUE,
                                                               collapsible = TRUE,
                                                               tableOutput("meta_hsroc_stats_revman"),
                                                               tags$div(style="float:right;",
                                                                        downloadButton("dl_meta_hsroc_stats_revman", "Download"))
                                                           )
                                                    ),
                                                    
                                                    column(width = 4,
                                                           box(width = "100%",
                                                               title = "Heterogeneity",
                                                               status = "success",
                                                               solidHeader = TRUE,
                                                               collapsible = TRUE,
                                                               tableOutput("meta_hsroc_stats_heterogen"),
                                                               tags$div(style="float:right;",
                                                                        downloadButton("dl_meta_hsroc_stats_heterogen", "Download"))
                                                           )
                                                    )
                                                  )
                                         ),
                                         
                                         tabPanel("SROC curve", 
                                                  id = "meta_hsroc_sroc_tab",
                                                  tags$hr(),
                                                  fluidRow(
                                                    column(width = 8,
                                                           box(width = "100%",
                                                               title = "SROC curve",
                                                               status = "success",
                                                               solidHeader = TRUE,
                                                               collapsible = TRUE,
                                                               plotlyOutput("meta_hsroc_sroc_summary"),
                                                               downloadButton("dl_meta_hsroc_sroc_summary", "Download")
                                                           )   
                                                    ),
                                                    
                                                    column(width = 4,
                                                           box(width = "100%",
                                                               title = "SROC display options",
                                                               status = "success",
                                                               solidHeader = TRUE,
                                                               collapsible = TRUE,
                                                               checkboxGroupInput(inputId = "meta_hsroc_sroc_options",
                                                                                  label = "Select the features you would like to display:",
                                                                                  choices = c("Ellipse" = 1,
                                                                                              "Summary point" = 2,
                                                                                              "Curve" = 3)
                                                                                  )
                                                               )
                                                            )
                                                            )
                                                  )
                                         )
                             )
               )
               ),
        
      
      # SUMMARY OF FINDINGS
      tabItem(tabName = "sumfind_tab",
              box(width = 9,
              fluidRow(
                column(width = 6,
                      numericInput("prevalence", "Prevalence (%):",
                              value = 50)),
                column(width = 6, 
                       numericInput("patients", "Size of the cohort:",
                             value = 100)))
              ),
              fluidRow(
                column(width = 12,
                       box(width = 9,
                           style = "overflow-x: scroll;",
                           collapsible = TRUE,
                           
                           conditionalPanel(condition = "output.sumfind_table_error == true",
                                            HTML('<p><strong><span style="color: rgb(209, 72, 65);">Error:</span></strong><span style="color: rgb(209, 72, 65);"> Please check that values for both inputs are correct. Prevalence &nbsp;must be a <strong>number</strong> <strong>between 0 and 100</strong>, while the cohort size must be a<strong>&nbsp;number greater than or equal to 0</strong>. Decimals are indicated by a <strong>dot</strong>.</span></p>')),
                           
                           conditionalPanel(condition = "output.sumfind_table_error == false",
                           
                             tags$div(align = "center", 
                                      withSpinner(plotOutput("sumfind_table", width = "700px", height = "600px"),
                                                  type = 6,
                                                  size = 0.5,
                                                  color = "#235779")),
                             tags$br(),
                             tags$div(style="display:inline-block; float:right;", 
                                      downloadButton("dl_sumfind", "Download")
                             ),
                             tags$div(style="display:inline-block; vertical-align:top; width:120px; float:right;",
                                      radioButtons(inputId = "dl_sumfind_options",
                                                   label = NULL,
                                                   choices = c(".png", ".svg"),
                                                   selected = ".png",
                                                   inline = TRUE))
                           )
                           )
                       )
                )
              ),
        
              
      # REFERENCES
      tabItem(tabName = "user_tab",
              h3("Video tutorial"),
              HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/vxc-oYpdEBs" title="MetaDiSc2.0 Tutorial" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
              tags$br(),
              h3("References"),
              tags$ol(type = "1",
                      tags$li("Chu H, Cole SR. Bivariate meta-analysis of sensitivity and specificity with sparse data: a generalized linear mixed model approach. J Clin Epidemiol. 2006;59(12):1331-2."), 
                      tags$li("Bates D, Maechler M, Bolker B, Walker S. Fitting Linear Mixed-Effects Models Using lme4. Journal of Statistical Software. 2015;67(1):1-48."), 
                      tags$li("Plana MN, Pérez T, Zamora J. New measures improved the reporting of heterogeneity in diagnostic test accuracy reviews: a metaepidemiological study. J Clin Epidemiol. 2021;131:101-112."), 
                      tags$li("Reitsma JB, Glas AS, Rutjes AWS, Scholten RJPM, Bossuyt PM, Zwinderman AH. Bivariate analysis of sensitivity and specificity produces informative summary measures in diagnostic reviews. J Clin Epidemiol. 2005;58(10):982–90."), 
                      tags$li("Zamora J, Abraira V, Muriel A, Khan K, Coomarasamy A. Meta-DiSc: a software for meta-analysis of test accuracy data. BMC Med Res Methodol. 2006;6:31."), 
                      tags$li("Zhou Y, Dendukuri N. Statistics for quantifying heterogeneity in univariate and bivariate meta-analyses of binary data: The case of meta-analyses of diagnostic accuracy. Stat Med. 2014;33(16):2701–17.")
              )
              
      )
      
      )
  )
  ),
  tags$footer("Meta-DiSc 2.0, 2021.", align = "center", style = "
              position:fixed;
              bottom:0;
              width:100%;
              height:3%;
              color:white;
              padding:2px;
              background-color:#686A6F;
              z-index:1000;
              ")
  ))
