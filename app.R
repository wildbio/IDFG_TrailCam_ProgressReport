# Remove existing objects from global environment
objs <- ls(pos = ".GlobalEnv")
rm(list = objs, pos = ".GlobalEnv")

# Install/load required packages
dependencies<-c("shiny","shinyBS", "shinydashboard","stringr","htmltools","knitr", "rmarkdown", "purrr")

for(i in 1:length(dependencies)){
  if(dependencies[i] %in% installed.packages()==FALSE){
    install.packages(dependencies[i])
    require(dependencies[i],character.only=TRUE)
  } else{
    require(dependencies[i],character.only=TRUE)
  }
}

# UI
ui <- dashboardPage(title = "Photo Processing Progress Report Generator",
  dashboardHeader(
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 80px}"),
            tags$style(".main-header .logo {
                       height: 80px; 
                       line-height: 75px !important;
                       padding: 0 0px;}")
    ),
    # Use image in title
    titleWidth='100%',
    title = span(
      tags$img(src="header.png", width = '100%'),
      column(12, class="title-box")
      ),
     tags$li(a(href = 'http://idfg.idaho.gov',
              img(src = "idfglogo.png",
                  title = "Idaho Fish and Game", height = "60px"),
              style = "padding-top:10px; padding-bottom:10px;"),
            class = "dropdown")
  ),
  dashboardSidebar(disable=TRUE),
  dashboardBody(
    fluidPage(
      titlePanel(h1(HTML("<font color=\"#546f39\" style = \"text-shadow: 1px 1px #42572d\"><b>Photo Processing Progress Report Generator</b></font>"))),
      br(),
      h4(HTML("<font color=\"#546f39\" style = \"text-shadow: 1px 1px #42572d\"><b>SYSTEM REQUIREMENTS:</b></font> 
      If not already located on computer, the following programs must be downloaded and installed:
      <ol>
      <li><a href=\"http://strawberryperl.com\">Strawberry Perl - Windows 64-bit</a></li>
        <ol>
        <li>Download Strawberry Perl to its default location in <font face=\"Courier New\">C:\\Strawberry</font></li>
        <li>If only options given by install wizard are to remove or update, you already have it!</li>
        </ol>
      <li><a href=\"https://docs.microsoft.com/en-us/azure/storage/common/storage-use-azcopy-v10\">AzCopy v10 - Windows 64-bit</a> to the <font face=\"Courier New\">C:\\</font> drive 
        <ol style=\"list-style-type: lower-alpha;\">
        <li>Download AzCopy zip folder to Downloads folder (should appear as <font face=\"Courier New\">azcopy_windows_amd64_10.3.2</font>)</li>
        <li>Right click anywhere in the zip folder window (click thumbnail in downloads bar at bottom of browser screen if window doesn't open automatically), and select \"Extract all\"</li>
        <li>Extract all contents to the <font face=\"Courier New\">C:\\</font> drive</li>
        </ol>
      </li>")),
      br(),
      sidebarLayout(
                   sidebarPanel(
                                          h2(HTML("<font color=\"#6c8c4c\" style = \"text-shadow: 1px 1px #3f4533\"><b>Instructions</b></font>")),
                     h3(HTML("<ol>
                                        <li>Select folder with photos for <b>REGION</b> of interest</li>
                                        <br>
                                        <li>Click <b>\"Generate Report\"</b></li>
                                        </ol>")),
                     br(),
                     actionButton(inputId = "chooseSourceButton", label = HTML("<font size = 4>Click to Choose Folder</font>"), 
                                  style="color: #fff; background-color: #8ead6e; border-color: #6c8c4c; width: 100%"),
                     uiOutput("selectedSource"),
                     br(),
                     actionButton(inputId = "downloadReport", label = HTML('<font size = 5>Generate Report</font>'), 
                                  style="color: #fff; background-color: #546f39; border-color: #42572d; width: 100%"),
                     br()
                     
                                
                   ),
                   mainPanel(uiOutput(outputId = "display_report"),
                             uiOutput(outputId = "sent_report")
                   )
                                  
                   )# sidebarLayout
    ) # fluidpage
  ) # dashboard body
) # dashboard page

  
    
server <- function(input, output, session) {
  observeEvent(input$chooseSourceButton, {
    current_folder <- choose.dir("")
    if(!is.na(current_folder)){
      output$selectedSource <- renderUI({
        checkboxGroupInput(inputId = "selectedSource", label = NULL,
                           choices = current_folder, selected = current_folder)
      })
    }
  })
  
  input_folder <- reactive({
    req(input$selectedSource)
    gsub("\\\\","/", input$selectedSource)
  })
  
  observeEvent(input$selectedSource, {
    report_folder <- file.path(dirname(input_folder()),"progress_reports")
    if(!dir.exists(report_folder)){
      dir.create(report_folder)
    }
  })
  
  report_folder <- reactive({
    req(input$selectedSource)
    file.path(dirname(input_folder()),"progress_reports")
  })
  
  csv_folder <- reactive({
    req(input$selectedSource)
    file.path(dirname(input_folder()),"combined_csvs")
  })
  
  reg_name <- reactive({
    req(input$selectedSource)
    max <- max(str_count(input_folder(), "/"))
    str_split(input_folder(),"/")[[1]][max+1]
  })
  
  report_file <- reactive({
    req(input_folder())
    paste0(report_folder(),"/",reg_name(),"_ProgressReport.html")
  })
  
  observeEvent(input$downloadReport, {
    shiny::withProgress(
      value = 0, {
        incProgress(3/10, message = "Creating report and combining csvs...")
        writeReport <- "progressreport_app_report.Rmd"
        
        # Set up parameters to pass to Rmd document
        params <- list(folder = input$selectedSource)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(writeReport, output_file = report_file(),
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        
        owd <- getwd()
        setwd("C:/azcopy_windows_amd64_10.3.2")
        
        # Send csv to cloud
        incProgress(3/10, message = "Sending csv to cloud...")
        cloud_loc_csv <- "\"https://idfgupload.blob.core.windows.net/idfg/VMuploads/combined_csvs?st=2019-10-30T16%3A10%3A57Z&se=2032-10-31T16%3A10%3A00Z&sp=racwdl&sv=2018-03-28&sr=c&sig=3ObVsb2Rb9gmdBW0Rvh%2FmyhLuB0w5ZoLwpmjVaR%2B2cI%3D\""
        speed_csv <- "--cap-mbps 20"
        push_csv <- paste("azcopy sync", csv_folder(), cloud_loc_csv, speed_csv)
        system(push_csv, intern = T)
        
        # Send report to cloud
        incProgress(3/10, message = "Sending report to cloud...")
        cloud_loc_rpt <- "\"https://idfgupload.blob.core.windows.net/idfg/VMuploads/progress_reports?st=2019-10-30T16%3A10%3A57Z&se=2032-10-31T16%3A10%3A00Z&sp=racwdl&sv=2018-03-28&sr=c&sig=3ObVsb2Rb9gmdBW0Rvh%2FmyhLuB0w5ZoLwpmjVaR%2B2cI%3D\""
        speed_rpt <- "--cap-mbps 20"
        push_report <- paste("azcopy sync", report_folder(), cloud_loc_rpt, speed_rpt)
        system(push_report, intern = T)
        
        setwd(owd)
        
        output$display_report <- renderUI({
          list(h2(HTML("<b>Progress report generated!<b>")),
               br(),
               h3(HTML(paste0("Report is available at <font face=\"Courier New\">",
                              report_file(),"</font> <br><br>Both report and combined Timelapse csv have also been uploaded to the cloud
                              <br><br><b>Thanks for checking in!</b>")))
          )
        })
      }
    )
  })
  
  }

# Create a Shiny app object
shinyApp(ui = ui, server = server)
