##### LOCALITY PROFILES MASTER DOC RENDER CODE #####

source(here::here("Master RMarkdown Document & Render Code", "Locality Profiles parameter values.R"))

  # Loop to create the profiles for all the localities in the list

  # There are several stages to the profiles:
  # 1. Looping through each locality in the HSCP doing the following:
  # 1a. Run each section script for that locality
  # 1b. Run the Rmd for the main body of the profiles
  # 1c. Run the Rmd for the summary tables

 # loop_env <- c(ls(), "loop_env")


  # 1. Loop through each locality to create the main body of the profiles and the summary table
#  for (LOCALITY in locality_list) {
    # 1a) Source in all the scripts for a given LOCALITY

    # Demographics ----
    source(here("Demographics", "1. Demographics - Population.R"))
    source(here("Demographics", "2. Demographics - SIMD.R"))

    # Housing ----
    source(here("Households", "Households Code.R"))

    # Services ----
    source(here("Services", "2. Services data manipulation & table.R"))
    source(here("Services", "3. Service HSCP map.R"))

    # General Health ----
    source("General Health/3. General Health Outputs.R")

    # Lifestyle & Risk Factors ----
    source("Lifestyle & Risk Factors/2. Lifestyle & Risk Factors Outputs.R")

    # Unscheduled Care ----
    source("Unscheduled Care/2. Unscheduled Care outputs.R")

    # Appendices ----
    source("Master RMarkdown Document & Render Code/Tables for Appendix.R")

    # Render main profile content ----
    render(
      input = "Master RMarkdown Document & Render Code/Locality_Profiles_Master_Markdown.Rmd",
      output_file = glue("{LOCALITY} - Locality Profile.docx"),
      output_dir = output_dir
    )

    # Render the summary table(s) ----
    render(
      input = "Summary Table/Summary-Table-Markdown.Rmd",
      output_file = glue("{LOCALITY} - Summary Table.docx"),
      output_dir = path(output_dir, "Summary Tables")
    )

    # End of loop housekeeping ----
    # Clean up the environment by restoring it to the 'pre-loop' state.
 #   rm(list = setdiff(ls(), loop_env))
    # Force garbage collection to free up memory
    gc()
 # }
#}
