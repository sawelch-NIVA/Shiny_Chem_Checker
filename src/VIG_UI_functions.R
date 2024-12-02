jsCodes <- function(){
  
 
  tags$head(
    tags$script(HTML(
      "//Scrolls back to top of the page after choosing next category
      Shiny.addCustomMessageHandler('scrollTo', function(id) {
        var el = document.getElementById(id);
        if(el) {
          el.scrollIntoView({ behavior: 'smooth', block: 'start' });
        }
      });
      
      // This will resize textAreaInput according to its size
      $(document).on('shiny:connected', function() {
        $('textarea').each(function() {
          this.setAttribute('style', 'height:' + (this.scrollHeight) + 'px;overflow-y:hidden;');
          this.style.height = 'auto';
          this.style.height = (this.scrollHeight) + 'px';
        }).on('input', function() {
          this.style.height = 'auto';
          this.style.height = (this.scrollHeight) + 'px';
        });
      });
      
      // Copy text from the htmlOutput
      document.getElementById('copy-icon').addEventListener('click', function() {
        var text = document.getElementById('showDatadsetID').innerText;
        
        // Copy the text to the clipboard
        navigator.clipboard.writeText(text).then(function() {
          // Notify the server that the text was copied
          Shiny.setInputValue('text_copied', true);
        });
      });
      "
    )),
    tags$style(HTML("
    // Hides app while loading
      #loading-content {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-color: #ffffff;
        z-index: 9999;
        display: flex;
        align-items: center;
        justify-content: center;
        flex-direction: column;
      }
      /* Spinner styling */
      .spinner {
        border: 12px solid #f3f3f3; /* Light grey */
        border-top: 12px solid #3498db; /* Blue */
        border-radius: 50%;
        width: 80px;
        height: 80px;
        animation: spin 2s linear infinite;
      }
      /* Spinner animation */
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
    "))
  )
  
}

#### Max character length ####
set_max_length <- function(input_id, max_length, warning_id) {
  shinyjs::runjs(sprintf(
    "
    $('#%s').on('input', function() {
      let inputField = $(this);
      let warning = $('#%s');
      let value = inputField.val();

      if (value.length > %d) {
        inputField.val(value.substring(0, %d)); // Restrict input to max length
        warning.text('Exceeded maximum %d characters!').show();
      } else {
        warning.hide();
      }
    });
    ", 
    input_id, warning_id, max_length, max_length, max_length
  ))
}


#### Info Messages ####


infoMessage <- function(fieldText, infoText, type = "sidebar"){
  
  if (type == "box"){
    
    informationText = paste0("<p style = 'font-weight: 300; color: #ffff;'>", infoText, " </p></details>" )
    showText = paste0("<details><summary>",fieldText," <i style = 'color: #fffff'; class='fa fa-info-circle'></i></summary>")
    
    
  } else {
    
    informationText = paste0("<p style = 'font-weight: 300; color: #3c89d0;'>", infoText, " </p></details>" )
    showText = paste0("<details><summary>",fieldText," <i style = 'color: #3c8dbc'; class='fa fa-info-circle'></i></summary>")
    
  }  
  
  HTML(showText,informationText)
  
}

boxContent <- function(title, btn, info){
  expBtn <- paste0("ExpTbl",title)
  table = paste0("tbl", title)
  
 box <-  box(
   title = infoMessage(fieldText = title, 
                infoText = info,
                type = "box"),
    status = "info",
    width = NULL,
    solidHeader = T,
    collapsible = T,
    
    br(),
    rHandsontableOutput(table),
    
    br(),
    
    actionButton(btn, "Clear table"),
    br(),
    
    actionBttn(
      expBtn,
      "Expand table",
      color = "primary",
      style = "simple",
      icon = icon("expand"),
      size = 'sm'
    )
  )
  return(box)
}


updatedataSet <- function(dt, session) {
  dt <- dt %>%
    replace(is.na(.), "")
  if (nrow(dt) == 0){
    return()
  }

  dt = tail(dt,1)
  updateDateInput(session, "expDate", value = format(as.Date(dt$EXP_DATE, format = "%d.%m.%Y"), "%Y-%m-%d"))
  
  updateSelectizeInput(session, "SAMPLE_MATRIX", selected = dt$SAMPLE_MATRIX)
  
  updateSelectizeInput(session, "FRACTION", selected = dt$FRACTION)
  
  selection = ifelse(dt$OBS_TYPE == "", character(0), dt$OBS_TYPE)
  updateRadioButtons(session, "obsType", selected = selection)
  
  updateSelectizeInput(session, "reliabSys", selected = dt$RELIAB_EVAL_SYS)
  
  updateNumericInput(session, "reliabScore", value = as.integer(dt$RELIAB_SCORE))
  
  updateDateInput(session, "configExpDate", value = format(as.Date(dt$CONF_EXPIRY_DATE, format = "%d.%m.%Y"), "%Y-%m-%d"))

  updateSelectizeInput(session, "dataSource", selected =  dt$DATA_SOURCE)
  
  updateSelectizeInput(session, "refIDType", selected = dt$REF_ID_TYPE)
  
  updateTextAreaInput(session, "refID", value = dt$REF_ID)
  
  updateTextAreaInput(session, "organisation", value = dt$ORGANIZATION)
  
  updateTextAreaInput(session, "enteredBy", value = dt$ENTERED_BY)
  
  updateDateInput(session, "enteredDate", value = format(as.Date(dt$ENTERED_DATE, format = "%d.%m.%Y"), "%Y-%m-%d"))
  
  updateTextAreaInput(session, "genComment", value = dt$GENERAL_COMMENT)
  
  
  #### Species ####
  
  
  updateSelectizeInput(
    session,
    "SAMPLE_SPECIES",
    choices = c("", dt$SPECIES),
    selected = dt$SPECIES
  )
  
  updateSelectizeInput(session, "SAMPLE_SPECIES_GENDER", selected = dt$SPECIES_GENDER)
  
  updateSelectizeInput(session, "SAMPLE_SPECIES_LIFESTAGE", selected = dt$SPECIES_LIFESTAGE)
  
  updateTextAreaInput(session, "CommentSpecies", value = dt$SPECIES_COMMENT)
  
  
  # #### Assay ####
  
  updateSelectizeInput(session, "SAMPLE_TISSUE", selected = dt$TISSUE_NAME)
  
  updateSelectizeInput(session, "RAcell", selected = dt$CELL_NAME)
  
  updateSelectizeInput(session, "assayFormat", selected = dt$ASSAY_FORMAT)
  
  updateTextAreaInput(session, "assayDesc", value = dt$ASSAY_DESCRIPTION)
  
  updateTextAreaInput(session, "assayComment", value =  dt$ASSAY_COMMENTS)
  
  updateSelectizeInput(session, "RAtarget", selected = dt$TARGET)
  
  updateTextAreaInput(session, "targetAcc", value = dt$TARGET_ACC)
  
  updateSelectizeInput(session, "targetAccSource", selected = dt$TARGET_ACC_SOURCE)
  
  # #### Study ####
  
  
  updateSelectizeInput(session, "studyType", selected = dt$STUDY_TYPE)
  
  updateNumericInput(session, "studyDuration", value = as.integer(dt$DURATION))
  
  updateSelectizeInput(session, "studyDurationUnit", selected = dt$DURATION_UNIT)
  
  updateSelectizeInput(session, "expType", selected = dt$EXPOSURE_TYPE)
  
  updateNumericInput(session, "expDuration", value = as.integer(dt$EXPOSURE_DURATION))
  
  updateSelectizeInput(session, "expDurUnit", selected = dt$EXPOSURE_DURATION_UNIT)
  
  updateNumericInput(session, "obsDuration", value = dt$OBS_DURATION)
  #
  updateSelectizeInput(session, "MEASURED_UNIT", selected = dt$OBS_DURATION_UNIT)
  
  updateTextAreaInput(session, 'testComment', value = dt$TEST_COMMENT)
  
  # #### Stressors ####
  
  
  updateSelectizeInput(session, 'DATA_TYPE', selected = dt$DATA_TYPE)
  
  updateSelectizeInput(session, "RAstressor", selected = dt$STRESSOR_TYPE)
  
  # updateTextAreaInput('INCHIKEY_STANDARD', "Type InChiKey standard",
  #                 selected = dt$INCHIKEY_STANDARD)
  
  updateSelectizeInput(session, "chem", selected = dt$textChem)
  
  updateTextAreaInput(session, "STRESSOR_NAME_SUB", value = dt$STRESSOR_NAME_SUB)
  #
  updateSelectizeInput(session, "StressSource", selected = dt$STRESSOR_SOURCE)
  
  # #### Observation ####
  #
  updateSelectizeInput(session, "meastype1", selected = dt$MEAS1_TYPE)
  #
  updateSelectizeInput(session, "measver1", selected = dt$MEAS1_VER)
  #
  updateSelectizeInput(session, "measunit1", selected = dt$MEAS1_UNIT)
  #
  updateNumericInput(session, "measRepl1", value = as.integer(dt$MEAS1_REPL))
  #
  updateSelectizeInput(session, "meastype2", selected = dt$MEAS2_TYPE)
  #
  updateSelectizeInput(session, "measver2", selected = dt$MEAS2_VER)
  #
  updateSelectizeInput(session, "measunit2", selected = dt$MEAS2_UNIT)
  #
  updateNumericInput(session, "measRepl2", value = as.integer(dt$MEAS2_REPL))
  #
  updateNumericInput(session, "dataGroup", value = as.integer(dt$GROUP))
  #
  updateTextAreaInput(session, "meascomment2", value = dt$MEAS2_COMMENT)
}
