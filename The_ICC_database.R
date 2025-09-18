
# Connect to REDCap
rcon <- redcapConnection(url = redcap_url, token = api_token)

# Fetch data
redcap_data <- exportRecords(rcon)

# --- Variable selection ---

subset_data <- redcap_data[, setdiff(names(redcap_data), c("record_id", "redcap_survey_identifier", "the_icc_form_timestamp", "outcome_description","outcome_description_text" , "the_icc_form_complete"))]

# --- Relabel columns for display ---
colnames(subset_data) <- c("ICC value", "Estimation method", "N individuals", "Individual level", "N clusters", "Cluster level", "Coefficient of variation of cluster sizes", "Outcome description", "Primary outcome (Yes/No)", "Outcome type", "Other outcome type", "Study ID", "Study design", "Other study design", "Publication link/doi/PMID")

# Render data table
output$redcap_table <- renderDT({
  datatable(subset_data, options = list(pageLength = 10, scrollX = TRUE))
})





