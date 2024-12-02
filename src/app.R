if (!require("pacman")) install.packages("pacman")
pacman::p_load(
    shiny,
    webchem,
    DT,
    dplyr,
    shinyjs,
    shinydashboard,
    leaflet,
    sf,
    tidyr,
    markdown,
    rmarkdown
)

source(file = "VIG_UI_functions.R")

# UI ----------------------------------------------------------------------
ui <- dashboardPage(
    dashboardHeader(title = "eData Import GUI"),

## Sidebar ----------------------------------------------------------------
    
    dashboardSidebar(
        width = 200,
### Sidebar 1. General ----------------------------------------------------
        box(
            title = "1. General information",
            width = NULL,
            class = "box-grey",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            tags$div(class = "nav flex-column",
                     tags$a(href = "#general", "1.1 Introduction", class = "nav-link"),
                     tags$a(href = "#general", "1.2 Dataset general information", class = "nav-link")
            )
        ),
        

### Sidebar 2. Sites --------------------------------------------------------

        box(
            title = "2. Sites",
            width = NULL,
            class = "box-red",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            tags$div(class = "nav flex-column",
                     tags$a(href = "#site-coordinates", "2.1 Input site coordinates", class = "nav-link"),
                     tags$a(href = "#site-features", "2.2 Enter site features", class = "nav-link"),
                     tags$a(href = "#export-parameters", "2.3 Export sampled parameters", class = "nav-link")
            )
        ),
        
### Sidebar 3. Compartments ------------------------------------------------
        box(
            title = "3. Compartments",
            width = NULL,
            class = "box-yellow",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            tags$div(class = "nav flex-column",
                     tags$a(href = "#compartments", "3.1 Compartments", class = "nav-link"),
                     tags$a(href = "#input-compartments", "3.2 Input sampled compartments", class = "nav-link"),
                     tags$a(href = "#validate-compartments", "3.3 Validate sampled compartments", class = "nav-link")
            )
        ),
        
### Sidebar 4. Parameters ------------------------------------------------------
        box(
            title = "4. Parameters",
            width = NULL,
            class = "box-green",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            tags$div(class = "nav flex-column",
                     tags$a(href = "#input-parameters", "4.1 Input sampled parameters", class = "nav-link"),
                     tags$a(href = "#validate-parameters", "4.2 Validate sampled parameters", class = "nav-link"),
                     tags$a(href = "#parameter-details", "4.3 Enter parameter details", class = "nav-link"),
                     tags$a(href = "#export-parameters", "4.4 Export sampled parameters", class = "nav-link")
            )
        ),
        
### Sidebar 5. Samples ------------------------------------------------------
        box(
            title = "5. Samples",
            width = NULL,
            class = "box-blue",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            tags$div(class = "nav flex-column",
                     tags$a(href = "#input-combinations", "5.1 Input sample combinations", class = "nav-link"),
                     tags$a(href = "#validate-combinations", "5.2 Validate sample combinations", class = "nav-link"),
                     tags$a(href = "#export-combinations", "5.3 Export sample combinations", class = "nav-link")
            )
        ),
        
### Sidebar 6. Measurements ------------------------------------------------------

        box(
            title = "6. Measurements",
            width = NULL,
            class = "box-purple",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            tags$div(class = "nav flex-column",
                     tags$a(href = "#export-template", "6.1 Export Excel template", class = "nav-link"),
                     tags$a(href = "#fill-template", "6.2 Fill in Excel template", class = "nav-link"),
                     tags$a(href = "#upload-template", "6.3 Upload Excel template", class = "nav-link"),
                     tags$a(href = "#submit-radb", "6.4 Submit to RaDB", class = "nav-link")
            )
        )
    ),

## Dashboard ----------------------------------------------------------
    dashboardBody(
        useShinyjs(),
        tags$head(
            tags$style(HTML("
                .nav-link { 
                    padding: 0.5rem 1rem !important;
                    margin: 0.5rem 0 !important;
                    color: #333 !important;
                    display: block !important;
                    line-height: 1.5 !important;
                }
                .box {
                    margin-bottom: 10px;
                }
                                /* Position notifications */
                .shiny-notification {
                    position: fixed;
                    bottom: 10px;
                    right: 10px;
                    opacity: 0.95;
                }
                
                /* Stack multiple notifications */
                .shiny-notification-panel {
                    z-index: 99999;
                }
                
        /* Google-inspired rainbow palette */
        .box.box-red > .box-header { background-color: #db4437 !important; color: #fff !important; }
        .box.box-orange > .box-header { background-color: #f4b400 !important; color: #fff !important; }
        .box.box-yellow > .box-header { background-color: #f4b400 !important; color: #000 !important; }
        .box.box-green > .box-header { background-color: #0f9d58 !important; color: #fff !important; }
        .box.box-blue > .box-header { background-color: #4285f4 !important; color: #fff !important; }
        .box.box-purple > .box-header { background-color: #7b1fa2 !important; color: #fff !important; }
        .box.box-grey > .box-header { background-color: #9e9e9e !important; color: #fff !important; }
        .box.box-darkgrey > .box-header { background-color: #616161 !important; color: #fff !important; }
            "))
        ),
   
### Dashboard 1. General ------------------------------------------------------
        box(
            title = "1.1 Introduction",
            width = NULL,
            class = "box-grey",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            
            div(
                class = "markdown-content",
                includeMarkdown("../md/ui_intro.md")
            )
            ),

        box(
            title = "1.2 Enter general information",
            width = NULL,
            class = "box-grey",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            
            # Dataset ID section
            radioButtons("createID",
                         infoMessage(fieldText = "Would like to retrieve a previously saved dataset?",
                                     infoText = ""),
                         choices = c("Yes", "No"),
                         inline = TRUE,
                         selected = "No"),
            
            conditionalPanel("input.createID == 'Yes'",
                             textAreaInput("datasetID",
                                           label = "Dataset ID",
                                           value = "A dataset ID will be automatically created once the campaign name and entered by fields have been filled"),
                             actionButton("datasetSearch", "Search for dataset")),
            
            # Campaign section
            selectizeInput("CAMPAIGN",
                           label = infoMessage(fieldText = "Campaign",
                                               infoText = ""),
                           choices = c("New Campaign", "Existing Campaign")), # Replace with actual campaigns
            
            conditionalPanel("input.CAMPAIGN == 'New Campaign'",
                             textInput("campaignOther",
                                       "Campaign name")),
            textOutput("warn-campaignOther"),
            tags$style("#warn-campaignOther { color: red; font-weight: bold; display: none; }"),
            
            # Date and reliability section
            dateInput("campaignDate",
                      infoMessage(fieldText = "Campaign date", 
                                  infoText = ""),
                      value = Sys.Date(),
                      format = "dd.mm.yyyy"),
            
            numericInput(
                "RELIABILITY_SCORE",
                infoMessage(fieldText = "Reliability score",
                            infoText = "Reliability score for study (e.g. 1,2,3..20)"),
                min = 1,
                max = 20,
                value = 1
            ),
            
            selectizeInput(
                "RELIABILITY_EVAL_SYS",
                infoMessage(fieldText = "Reliability evaluation system",
                            infoText = "Reliability score for study (e.g. Klimisch, CRED)"),
                choices = c("Klimisch", "CRED"),  # Added some default choices
                multiple = FALSE
            ),
            
            # Reference and confidentiality section
            textAreaInput("MEASURED_REFERENCE_ID",
                          infoMessage(fieldText = "Measured reference ID",
                                      infoText = "")),
            
            radioButtons("CONFIDENTIALITY_EXPIRY_DATE",
                         infoMessage(fieldText = "Would you like add an embargo date?",
                                     infoText = "Date for confidentiality expiration"),
                         choices = c("Yes", "No"),
                         selected = "No",
                         inline = TRUE),
            
            conditionalPanel("input.CONFIDENTIALITY_EXPIRY_DATE == 'Yes'",
                             dateInput(
                                 "configExpDate",
                                 infoMessage(fieldText = "Data embargo date",
                                             infoText = "Date for confidentiality expiration"),
                                 value = Sys.Date(),
                                 format = "dd.mm.yyyy"
                             )),
            
            # Organization and entry details
            textAreaInput(
                "ORGANISATION",
                infoMessage(fieldText = "Organisation",
                            infoText = "Data provider organisation (e.g. NIVA)")
            ),
            
            textAreaInput("ENTERED_BY",
                          infoMessage(fieldText = "Entered by",
                                      infoText = "Initials (e.g. KET)")
            ),
            
            dateInput(
                "ENTERED_DATE",
                infoMessage(fieldText = "Entered date",
                            infoText = "Date of entry"),
                value = Sys.Date(),
                format = "dd.mm.yyyy"
            ),
            
            textAreaInput("COMMENTS",
                          "Comment to data entry",
                          height = "100px")
        ),
        
### Dashboard 2. Sites ------------------------------------------------------
#### Dashboard 2.1 Site Coords ----------------------------------------------
box(
    title = "2.1 Input site coordinates",
    width = NULL,
    class = "box-red",
    solidHeader = TRUE,
    collapsible = TRUE,
    collapsed = TRUE,
    tabsetPanel(
        tabPanel("Manual Entry",
                 textInput("siteCode", "Site Code"),
                 numericInput("xCoord", "X Coordinate (Longitude)", value = NULL, width = "30%"),
                 numericInput("yCoord", "Y Coordinate (Latitude)", value = NULL, width = "30%"),
                 selectInput("crs", "Coordinate Reference System",
                             choices = c(
                                 "WGS 84 (EPSG:4326)" = "EPSG:4326",
                                 "UTM Zone 33N (EPSG:32633)" = "EPSG:32633",
                                 "UTM Zone 32N (EPSG:32632)" = "EPSG:32632",
                                 "Web Mercator (EPSG:3857)" = "EPSG:3857",
                                 "ETRS89 (EPSG:4258)" = "EPSG:4258",
                                 "EUREF89 - NTM Zone 10 (EPSG:5110)" = "EPSG:5110"
                             ), width = "30%"),
                 div(
                     style = "margin-top: 15px",
                     actionButton("addSite", "Add Site", 
                                  class = "btn-success",
                                  icon = icon("plus")),
                     actionButton("removeSites", "Remove Selected", 
                                  class = "btn-danger",
                                  icon = icon("trash"))
                 )
        )
    ),
    
    # Preview table of all sites
    h4("Current Sites:"),
    DTOutput("sitesTable"),
    
    # Map preview
    h4("Map Preview:"),
    leafletOutput("siteMap", height = "400px"),
            
            # Validation messages
            htmlOutput("validationMessages"),
            
            # Action buttons
            div(
                style = "margin-top: 15px",
                actionButton("validateAll", "Next", class = "btn-success"),
                actionButton("clearSites", "Clear All", class = "btn-danger")
            )
        ),
#### Dashboard 2.2 Site Features ----------------------------------------------
        
box(
    title = "2.2 Enter site features",
    width = NULL,
    class = "box-red",
    solidHeader = TRUE,
    collapsible = TRUE,
    collapsed = TRUE,
    # Site table with edit capabilities
    DTOutput("editableSitesTable"),
    
    # Feature selection controls
    div(
        style = "margin-top: 15px",
        selectInput("site_feature", "Geographic Feature",
                    choices = c(
                        "Select Feature" = "",
                        "Lake" = "LAKE",
                        "River" = "RIVER",
                        "Coast" = "COAST",
                        "Other" = "OTHER"
                    )),
        conditionalPanel(
            condition = "input.site_feature == 'OTHER'",
            textInput("custom_feature", "Specify Other Feature")
        ),
        selectInput("site_sub_feature", "Geographic Sub-Feature",
                    choices = c(
                        "Select Sub-Feature" = "",
                        "Pelagic" = "PELAGIC",
                        "Littoral" = "LITTORAL",
                        "Profundal" = "PROFUNDAL",
                        "Other" = "OTHER"
                    )),
        conditionalPanel(
            condition = "input.site_sub_feature == 'OTHER'",
            textInput("custom_sub_feature", "Specify Other Sub-Feature")
        )
    ),
    
    # Action buttons
    div(
        style = "margin-top: 15px",
        actionButton("updateFeatures", "Update Selected Sites", 
                     class = "btn-success",
                     icon = icon("check")),
        actionButton("duplicateSite", "Duplicate Selected", 
                     class = "btn-info",
                     icon = icon("copy")),
        actionButton("updateSiteName", "Edit Site Name", 
                     class = "btn-success",
                     icon = icon("edit"))
    )
),
#### Dashboard 2.3 Export Site ----------------------------------------------
        box(
            title = "2.3 Export site parameters",
            width = NULL,
            class = "box-red",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            downloadButton("export_sites", "Download Sites as .csv"),
            actionButton("go_to_compartments", "Next", 
                         class = "btn-warning",
                         icon = icon("edit"))
        ),
### Dashboard 3 Compartments ----------------------------------------------

#### Dashboard 3.1 Compartments ----------------------------------------------

        box(
            title = "3.1 Compartments",
            width = NULL,
            class = "box-yellow",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            textAreaInput("compartments", "Enter compartments:", rows = 5)
        ),

#### Dashboard 3.2 Sampled Compartments ----------------------------------------

        box(
            title = "3.2 Input sampled compartments",
            width = NULL,
            class = "box-yellow",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            textAreaInput("sampled_compartments", "Enter sampled compartments:", rows = 5)
        ),

#### Dashboard 3.3 Validate Compartments ---------------------------------------
        
        box(
            title = "3.3 Validate sampled compartments",
            width = NULL,
            class = "box-yellow",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            DTOutput("compartment_validation")
        ),

### Dashboard 4 Parameters ----------------------------------------------

#### Dashboard 4.1 Input Parameters ---------------------------------------

        # Parameters panels
        box(
            title = "4.1 Input sampled parameters",
            width = NULL,
            class = "box-green",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            textAreaInput("parameters", "Enter parameter names (one per line):", rows = 10),
            actionButton("validate", "Validate Parameters"),
            uiOutput("progress"),
            style = "primary"
        ),

#### Dashboard 4.2 Validate Parameters ---------------------------------------

        box(
            title = "4.2 Validate sampled parameters",
            width = NULL,
            class = "box-green",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            DTOutput("results"),
            div(style = "margin-top: 15px",
                actionButton("addStressor", "Add as Stressor", class = "btn-primary"),
                actionButton("addQuality", "Add as Quality parameter", class = "btn-info"),
                actionButton("addNorm", "Add as Normalization", class = "btn-success"),
                actionButton("addBackground", "Add as Background", class = "btn-warning")
            )
        ),

#### Dashboard 4.3 Parameters Details ---------------------------------------

        box(
            title = "4.3 Enter parameter details",
            width = NULL,
            class = "box-green",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            DTOutput("selected"),
            actionButton("remove", "Remove selected parameters", class = "mt-3"),
        ),

#### Dashboard 4.4 Export Parameters ---------------------------------------
        
        box(
            title = "4.4 Export sampled parameters",
            width = NULL,
            class = "box-green",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            downloadButton("export_parameters", "Export Parameters")
        ),

### Dashboard 5 Samples ----------------------------------------------

        # Samples panels
        box(
            title = "5.1 Input sample combinations",
            width = NULL,
            class = "box-blue",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            textAreaInput("sample_combinations", "Enter sample combinations:", rows = 5)
        ),
        
        box(
            title = "5.2 Validate sample combinations",
            width = NULL,
            class = "box-blue",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            DTOutput("sample_validation")
        ),
        
        box(
            title = "5.3 Export sample combinations",
            width = NULL,
            class = "box-blue",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            downloadButton("export_samples", "Export Samples")
        ),
### Dashboard 6 Measurements ----------------------------------------------

        # Measurements panels
        box(
            title = "6.1 Export Excel template",
            width = NULL,
            class = "box-purple",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            downloadButton("export_template", "Export Template")
        ),
        
        box(
            title = "6.2 Fill in Excel template",
            width = NULL,
            class = "box-purple",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            helpText("Please fill in the exported Excel template with your data.")
        ),
        
        box(
            title = "6.3 Upload Excel template",
            width = NULL,
            class = "box-purple",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            fileInput("template_upload", "Upload filled template")
        ),
        
        box(
            title = "6.4 Submit to RaDB",
            width = NULL,
            class = "box-purple",
            solidHeader = TRUE,
            collapsible = TRUE, collapsed = TRUE,
            actionButton("submit_radb", "Submit to RaDB", class = "btn-primary")
        )
    )
)


# SERVER() ----------------------------------------------------------------

server <- function(input, output, session) {
    ### Debug Functions ----------------------------------------------------
    notify <- function(message, type = "default", duration = 5) {
        showNotification(
            message,
            type = type,  # can be "default", "message", "warning", "error"
            duration = duration,
            closeButton = TRUE
        )
    }
    
    # Debug notification wrapper
    debug_notify <- function(func_name) {
        notify(
            sprintf("Function called: %s", func_name),
            type = "message",
            duration = 2
        )
    }
### Functions 1. General ------------------------------------------------
    
### Functions 2. Sites -----------------------------------------------------


#### sites_df ----------------------------------------------------------------
    sites <- reactiveVal(data.frame(
        site_id = integer(),
        siteCode = character(),
        x = numeric(),
        y = numeric(),
        crs = character(),
        feature = character(),
        sub_feature = character(),
        stringsAsFactors = FALSE
    ))
    
#### site_counter -------------------------------------------------------------  
    site_counter <- reactiveVal(1)
    
#### site_handler -------------------------------------------------------------  
    observeEvent(input$addSite, {
        debug_notify("addSite")
        req(input$siteCode, input$xCoord, input$yCoord, input$crs)
        
        current_sites <- sites()
        new_site_id <- if(nrow(current_sites) == 0) 1 else max(current_sites$site_id) + 1
        
        new_site <- data.frame(
            site_id = new_site_id,
            siteCode = input$siteCode,
            x = input$xCoord,
            y = input$yCoord,
            crs = input$crs,
            feature = NA_character_,
            sub_feature = NA_character_,
            stringsAsFactors = FALSE
        )
        
        validated_site <- validateCoordinatesWithNotification(new_site)
        
        if (!is.null(validated_site)) {
            sites(rbind(current_sites, validated_site))
            notify("Site added successfully", type = "message")
            
            updateTextInput(session, "siteCode", value = "")
            updateNumericInput(session, "xCoord", value = NULL)
            updateNumericInput(session, "yCoord", value = NULL)
        }
    })
    
#### removeSites -------------------------------------------------------------  
    observeEvent(input$removeSites, {
        debug_notify("removeSites")
        req(input$sitesTable_rows_selected)
        
        current_sites <- sites()
        if (length(input$sitesTable_rows_selected) > 0) {
            sites(current_sites[-input$sitesTable_rows_selected, ])
            notify("Selected sites have been removed", type = "warning")
        }
    })

#### sitesTable -------------------------------------------------------------  
    output$sitesTable <- renderDT({
        selected_cols <- c("siteCode", "x", "y", "crs")
        datatable(
            sites()[, selected_cols, drop = FALSE],
            options = list(
                pageLength = 5,
                dom = 'lftip'
            ),
            selection = 'multiple',
            rownames = FALSE,
            colnames = c(
                "Site Code" = "siteCode",
                "X" = "x",
                "Y" = "y",
                "CRS" = "crs"
            )
        )
    })

#### editableSitesTable -------------------------------------------------------------  
    output$editableSitesTable <- renderDT({
        debug_notify("renderEditableSitesTable")
        datatable(
            sites(),
            options = list(
                pageLength = 5,
                dom = 'lftip'
            ),
            selection = 'multiple',
            rownames = FALSE,
            colnames = c(
                "ID" = "site_id",
                "Site Code" = "siteCode",
                "X" = "x",
                "Y" = "y",
                "CRS" = "crs",
                "Feature" = "feature",
                "Sub-Feature" = "sub_feature"
            ),
            editable = TRUE
        )
    })
    
#### siteMap -------------------------------------------------------------  
    # Initialize map
    output$siteMap <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(lng = 10.7522, lat = 59.9139, zoom = 6)  # Centered on Norway
    })
    
    # Update map when sites change
    observe({
        debug_notify("mapUpdate")
        req(sites())
        if (nrow(sites()) > 0) {
            # Convert all sites to WGS84 for mapping
            sites_sf_list <- lapply(1:nrow(sites()), function(i) {
                site <- sites()[i,]
                epsg_code <- as.numeric(sub(".*:(\\d+)$", "\\1", site$crs))
                point <- st_point(c(site$x, site$y))
                point_sf <- st_sfc(point, crs = epsg_code)
                point_wgs84 <- st_transform(point_sf, 4326)
                site_sf <- st_sf(
                    siteCode = site$siteCode,
                    geometry = point_wgs84
                )
                return(site_sf)
            })
            
            sites_sf <- do.call(rbind, sites_sf_list)
            
            leafletProxy("siteMap") %>%
                clearMarkers() %>%
                addMarkers(
                    data = sites_sf,
                    popup = ~sprintf(
                        "<b>Site:</b> %s<br><b>Original CRS:</b> %s<br><b>Coords:</b> %.6f, %.6f",
                        sites()$siteCode, 
                        sites()$crs,
                        sites()$x,
                        sites()$y
                    )
                )
        }
    })
    
#### updateSiteName -------------------------------------------------------------  
    # Handle site name editing
    observeEvent(input$updateSiteName, {
        debug_notify("updateSiteName")
        req(input$editableSitesTable_rows_selected)
        
        showModal(modalDialog(
            title = "Edit Site Name",
            textInput("newSiteName", "New Site Name"),
            footer = tagList(
                modalButton("Cancel"),
                actionButton("saveSiteName", "Save", class = "btn-success")
            )
        ))
    })
    
#### saveSiteName -------------------------------------------------------------  
    observeEvent(input$saveSiteName, {
        debug_notify("saveSiteName")
        req(input$newSiteName, input$editableSitesTable_rows_selected)
        
        current_sites <- sites()
        current_sites$siteCode[input$editableSitesTable_rows_selected] <- input$newSiteName
        sites(current_sites)
        
        removeModal()
        notify("Site name updated", type = "message")
    })
    
#### updateFeatures -------------------------------------------------------------  
    observeEvent(input$updateFeatures, {
        debug_notify("updateFeatures")
        req(input$editableSitesTable_rows_selected)
        
        feature <- if(input$site_feature == "OTHER") input$custom_feature else input$site_feature
        sub_feature <- if(input$site_sub_feature == "OTHER") input$custom_sub_feature else input$site_sub_feature
        
        current_sites <- sites()
        current_sites$feature[input$editableSitesTable_rows_selected] <- feature
        current_sites$sub_feature[input$editableSitesTable_rows_selected] <- sub_feature
        sites(current_sites)
        
        notify("Features updated for selected sites", type = "message")
    })
    

#### duplicateSite -----------------------------------------------------------
    observeEvent(input$duplicateSite, {
        debug_notify("duplicateSite")
        req(input$editableSitesTable_rows_selected)
        
        current_sites <- sites()
        sites_to_duplicate <- current_sites[input$editableSitesTable_rows_selected, ]
        
        # Update the site counter to be greater than any existing ID
        current_max_id <- max(current_sites$site_id, na.rm = TRUE)
        new_start_id <- current_max_id + 1
        
        # Create sequential IDs for new duplicates
        sites_to_duplicate$site_id <- seq(
            from = new_start_id,
            length.out = nrow(sites_to_duplicate)
        )
        
        # Update the site codes
        sites_to_duplicate$siteCode <- paste0(sites_to_duplicate$siteCode, "_copy")
        
        # Combine original and duplicated sites
        sites(rbind(current_sites, sites_to_duplicate))
        
        # Update the counter for future additions
        site_counter(max(sites()$site_id) + 1)
        
        notify("Sites duplicated successfully", type = "message")
    })

#### Validate coordinates ------------------------------------------------
    validateCoordinatesWithNotification <- function(site_data) {
        debug_notify("validateCoordinatesWithNotification")
        
        # Check only essential fields
        essential_fields <- c("siteCode", "x", "y", "crs")
        if(any(is.na(site_data[essential_fields]))) {
            notify("Error: Missing values in essential site data (code, coordinates, or CRS)", type = "error")
            return(NULL)
        }
        
        # Convert to sf object for spatial validation
        sites_sf <- try({
            epsg_code <- as.numeric(sub(".*:(\\d+)$", "\\1", site_data$crs))
            point <- st_point(c(site_data$x, site_data$y))
            point_sf <- st_sfc(point, crs = epsg_code)
            point_wgs84 <- st_transform(point_sf, 4326)
            
            # Check if coordinates are within reasonable bounds after transformation
            coords <- st_coordinates(point_wgs84)
            if (coords[1] < -180 || coords[1] > 180 || coords[2] < -90 || coords[2] > 90) {
                notify("Warning: Coordinates are outside normal global bounds", type = "warning")
            }
            
            # For Norway, we might want to check if points are roughly in the right area
            # Rough bounds for Norway
            if (coords[1] < 4 || coords[1] > 32 || coords[2] < 58 || coords[2] > 72) {
                notify("Warning: Coordinates are outside Norway's typical bounds", type = "warning")
            }
        })
        
        if(inherits(sites_sf, "try-error")) {
            notify("Error: Invalid coordinates or CRS", type = "error")
            return(NULL)
        }
        
        notify("Coordinates validated successfully", type = "message")
        return(site_data)
    }
    
#### export_sites ------------------------------------------------
    output$export_sites <- downloadHandler(
        filename = function() {
            paste("sites-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            debug_notify("exportSites")
            write.csv(sites(), file, row.names = FALSE)
        }
    )

#### go_to_compartments ------------------------------------------------ 
 
    observeEvent(input$go_to_compartments, {
        debug_notify("goToCompartments")
        
        # Validate that we have at least one site
        if(nrow(sites()) == 0) {
            notify("Please add at least one site before proceeding", type = "error")
            return()
        }
        
        # Collapse current box and open next one
        runjs("
        // Close the sites box
        $('.box-red .box-header').click();
        
        // Open the compartments box after a short delay
        setTimeout(function() {
            $('.box-green .box-header').click();
        }, 500);
    ")
        
        notify("Moving to compartments section", type = "message")
    })
       
### Functions 4. Parameters -----------------------------------------------------
    showDuplicateModal <- function() {
        showModal(modalDialog(
            title = "Parameter Already Added",
            "Selected parameter(s) have already been added to the export table.",
            easyClose = TRUE,
            footer = modalButton("OK")
        ))
    }
    
    results <- eventReactive(input$validate, {
        parameters <- strsplit(input$parameters, "\n")[[1]]
        parameters <- trimws(parameters[parameters != ""])
        
        results <- data.frame(
            input_name = parameters,
            pubchem_name = NA_character_,
            cid = NA_character_,
            DATA_TYPE = "unknown",
            stringsAsFactors = FALSE
        )
        
        withProgress(message = 'Validating parameters', value = 0, {
            n <- length(parameters)
            time_per_lookup <- 2
            
            for(i in seq_along(parameters)) {
                incProgress(1/n, detail = sprintf(
                    "Parameter %d/%d (Est. %d seconds remaining)",
                    i, n, (n-i)*time_per_lookup
                ))
                
                tryCatch({
                    lookup <- get_cid(parameters[i], from = "name", match = "first")
                    if (!is.null(lookup) && length(lookup) > 0) {
                        cid <- as.character(lookup[2])
                        if (!is.na(cid)) {
                            results$cid[i] <- cid
                            results$DATA_TYPE[i] <- "Stressor"
                            
                            pc_info <- pc_prop(cid, properties = "IUPACName")
                            if (!is.null(pc_info)) {
                                results$pubchem_name[i] <- pc_info$IUPACName
                            }
                        }
                    }
                }, error = function(e) {
                })
                Sys.sleep(0.1)
            }
        })
        results
    })
    
    selected_data <- reactiveVal(data.frame(
        input_name = character(),
        pubchem_name = character(),
        cid = character(),
        DATA_TYPE = character(),
        stringsAsFactors = FALSE
    ))
    
    addParameters <- function(type) {
        if (!is.null(input$results_rows_selected)) {
            new_data <- results()[input$results_rows_selected, ]
            current_data <- selected_data()
            
            # Check for duplicates
            duplicates_exist <- any(new_data$cid %in% current_data$cid)
            new_data <- new_data[!new_data$cid %in% current_data$cid,]
            
            if(nrow(new_data) > 0) {
                new_data$DATA_TYPE <- type
                if (nrow(current_data) == 0) {
                    selected_data(new_data)
                } else {
                    selected_data(rbind(current_data, new_data))
                }
            }
            
            if(duplicates_exist) {
                showDuplicateModal()
            }
        }
    }
    
    
    observeEvent(input$addStressor, { addParameters("Stressor") })
    observeEvent(input$addQuality, { addParameters("Quality") })
    observeEvent(input$addNorm, { addParameters("Normalization") })
    observeEvent(input$addBackground, { addParameters("Background") })
    
    observeEvent(input$remove, {
        if (!is.null(input$selected_rows_selected)) {
            current_data <- selected_data()
            selected_data(current_data[-input$selected_rows_selected,])
        }
    })
    
    observe({
        if(is.null(input$results_rows_selected) || length(input$results_rows_selected) == 0) {
            shinyjs::disable("addStressor")
            shinyjs::disable("addQuality")
            shinyjs::disable("addNorm")
            shinyjs::disable("addBackground")
        } else {
            shinyjs::enable("addStressor")
            shinyjs::enable("addQuality")
            shinyjs::enable("addNorm")
            shinyjs::enable("addBackground")
        }
    })
    
    observe({
        if(is.null(input$selected_rows_selected) || length(input$selected_rows_selected) == 0) {
            shinyjs::disable("remove")
        } else {
            shinyjs::enable("remove")
        }
    })
    
    output$results <- renderDT({
        results_df <- if(!is.null(results())) results() else data.frame(
            input_name = character(),
            pubchem_name = character(),
            cid = character(),
            DATA_TYPE = character()
        )
        
        results_df$cid <- ifelse(
            !is.na(results_df$cid),
            sprintf('<a href="https://pubchem.ncbi.nlm.nih.gov/compound/%s" target="_blank">%s</a>', 
                    results_df$cid, results_df$cid),
            NA_character_
        )
        
        datatable(results_df,
                  options = list(pageLength = 25),
                  escape = FALSE,
                  selection = 'multiple',
                  rownames = FALSE) %>%
            formatStyle('DATA_TYPE',
                        backgroundColor = styleEqual(
                            c("unknown", "Stressor", "Quality", "Normalization", "Background"),
                            c("#ffebee", "#e8f5e9", "#e3f2fd", "#fff3e0", "#f3e5f5")
                        ))
    })
    
    output$selected <- renderDT({
        selected_df <- selected_data()
        if (nrow(selected_df) > 0) {
            selected_df$cid <- ifelse(
                !is.na(selected_df$cid),
                sprintf('<a href="https://pubchem.ncbi.nlm.nih.gov/compound/%s" target="_blank">%s</a>', 
                        selected_df$cid, selected_df$cid),
                NA_character_
            )
        }
        
        datatable(selected_df,
                  options = list(pageLength = 25),
                  escape = FALSE,
                  selection = 'multiple',
                  rownames = FALSE) %>%
            formatStyle('DATA_TYPE',
                        backgroundColor = styleEqual(
                            c("unknown", "Stressor", "Quality", "Normalization", "Background"),
                            c("#ffebee", "#e8f5e9", "#e3f2fd", "#fff3e0", "#f3e5f5")
                        ))
    })
}    

shinyApp(ui = ui, server = server)
