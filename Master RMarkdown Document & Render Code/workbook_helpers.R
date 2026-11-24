# Extract the existing locality-level data tables after the analysis scripts run.
collect_data_workbook_tables <- function(locality_environment) {
  with(locality_environment, {
    df <- list(
      "Population_Estimates" = pops[pops$hscp_locality == LOCALITY, ],
      "Population_Projections" = pop_proj_dat,
      "SIMD_Locality_2022" = simd_perc_breakdown,
      "SIMD_Domains_2016" = simd2016_dom,
      "SIMD_Domains_2022" = simd2020_dom,
      "Care_Home" = markers_care_home[
        markers_care_home$hscp_locality == LOCALITY,
      ],
      "Emergency_Dep" = markers_emergency_dep[
        markers_emergency_dep$hscp_locality == LOCALITY,
      ],
      "GP" = markers_gp[markers_gp$hscp_locality == LOCALITY, ],
      "Minor_Injuries_Unit" = markers_miu[
        markers_miu$hscp_locality == LOCALITY,
      ],
      "Housing_Data" = house_dat1,
      "Housing_Council_Tax_Band" = house_dat2,
      "Life_Expectancy" = life_exp[life_exp$area_name == LOCALITY, ],
      "Deaths_Aged_15_44" = deaths_15_44[deaths_15_44$area_name == LOCALITY, ],
      "Long_Term_Conditions" = ltc[ltc$hscp_locality == LOCALITY, ],
      "Cancer_Registrations" = cancer_reg[cancer_reg$area_name == LOCALITY, ],
      "Early_Deaths_Cancer" = early_deaths_cancer[
        early_deaths_cancer$area_name == LOCALITY,
      ],
      "Asthma_Hospitalisations" = asthma_hosp[
        asthma_hosp$area_name == LOCALITY,
      ],
      "CHD_Hospitalisations" = chd_hosp[chd_hosp$area_name == LOCALITY, ],
      "COPD_Hospitalisations" = copd_hosp[copd_hosp$area_name == LOCALITY, ],
      "MH_Prescriptions" = adp_presc[adp_presc$area_name == LOCALITY, ],
      "Alcohol_Admissions" = alcohol_hosp[alcohol_hosp$area_name == LOCALITY, ],
      "Alcohol_Deaths" = alcohol_deaths[alcohol_deaths$area_name == LOCALITY, ],
      "Drug_Admissions" = drug_hosp[drug_hosp$area_name == LOCALITY, ],
      "Bowel_Screening" = bowel_screening[
        bowel_screening$area_name == LOCALITY,
      ],
      "Emergency_Admissions" = emergency_adm_areas[
        emergency_adm_areas$location == LOCALITY,
      ],
      "Emergency_Admissions_Age " = emergency_adm_age[
        emergency_adm_age$hscp_locality == LOCALITY,
      ],
      "Unscheduled_Bed_Days" = bed_days_areas[
        bed_days_areas$location == LOCALITY,
      ],
      "Unscheduled_Bed_Days_Age" = bed_days_age[
        bed_days_age$hscp_locality == LOCALITY,
      ],
      "AE_attendances" = ae_att_areas[ae_att_areas$location == LOCALITY, ],
      "AE_attendances_Age" = ae_att_age[ae_att_age$hscp_locality == LOCALITY, ],
      "Readmissions" = readmissions_areas[
        readmissions_areas$location == LOCALITY,
      ],
      "Readmissions_Age" = readmissions_age,
      "Delayed_Discharge" = delayed_disch_areas[
        delayed_disch_areas$location == LOCALITY,
      ],
      "Fall_Admissions" = falls_areas[falls_areas$location == LOCALITY, ], # Need to add Scotland & HB to this
      "Preventable_admission_PPA" = ppa_areas[ppa_areas$location == LOCALITY, ], # Need to add Scotland & HB to this
      "psych_admissions" = psych_hosp[psych_hosp$area_name == LOCALITY, ], # Need to add Scotland & HB to this
      "MH_bed_days" = bed_days_mh_areas[
        bed_days_mh_areas$location == LOCALITY,
      ],
      "bed_days_mh_age" = bed_days_mh_age[
        bed_days_mh_age$hscp_locality == LOCALITY,
      ]
    )

    lapply(df, function(table) {
      table[["locality"]] <- rep(LOCALITY, nrow(table))
      table
    })
  })
}

collect_sdc_workbook_tables <- function(locality_environment) {
  with(locality_environment, {
    smr01_based_loc <- imap(
      list(
        "Preventable_Admissions_PPA" = ppa_areas,
        "Emergency_Admissions" = emergency_adm_areas,
        "Unscheduled_Bed_Days" = bed_days_areas,
        "Readmissions" = readmissions_areas,
        "AE_attendances" = ae_att_areas,
        "Delayed_Discharges" = delayed_disch_areas,
        "Fall_Admissions" = falls_areas,
        "MH_bed_days" = bed_days_mh_areas
      ),
      \(data, name) {
        data |>
          filter(location == LOCALITY) |>
          select(financial_year, n = any_of(c("read_28", "n"))) |>
          mutate(
            locality = LOCALITY,
            name = name,
            .before = everything()
          ) |>
          filter(n < 10L) # Apply SDC filter (counts less than 10)
      }
    )

    smr01_age_loc <- imap(
      list(
        "Emergency_Admissions_Age" = filter(
          emergency_adm_age,
          hscp_locality == LOCALITY
        ),
        "Unscheduled_Bed_Days_Age" = filter(
          bed_days_age,
          hscp_locality == LOCALITY
        ),
        "Readmissions_Age" = readmissions_age,
        "AE_Attendances_Age" = filter(ae_att_age, hscp_locality == LOCALITY),
        "Bed_Days_MH_Age" = filter(bed_days_mh_age, hscp_locality == LOCALITY)
      ),
      \(data, name) {
        data |>
          select(
            financial_year,
            age_group,
            n = any_of(c("adm", "bed_days", "read_28", "attendances"))
          ) |>
          mutate(
            locality = LOCALITY,
            name = name,
            .before = everything()
          ) |>
          filter(n < 10L) # Apply SDC filter (counts less than 10)
      }
    )

    ltc_loc <- imap(
      list(
        "Long_Term_Conditions" = ltc_multimorbidity,
        "LTC_Types" = ltc_types,
        "LTC_Top_5_Prevalence" = top5ltc_loc
      ),
      \(data, name) {
        data |>
          select(
            measure = any_of(c("total_ltc", "key", "Prevalence")),
            any_of("age_group"),
            n = any_of(c("people", "value"))
          ) |>
          mutate(
            locality = LOCALITY,
            name = name,
            .before = everything()
          ) |>
          filter(n < 10L) # Apply SDC filter (counts less than 10)
      }
    )

    list(
      SMR01_based = purrr::list_rbind(smr01_based_loc),
      SMR01_Age_Groups = purrr::list_rbind(smr01_age_loc),
      LTCs_Age_Groups = purrr::list_rbind(ltc_loc)
    )
  })
}

combine_data_workbook_tables <- function(localities_tables) {
  sheet_names <- names(localities_tables[[1]])
  tables <- lapply(sheet_names, function(sheet) {
    Reduce(rbind, lapply(localities_tables, `[[`, sheet))
  })
  names(tables) <- sheet_names
  tables
}

combine_sdc_workbook_tables <- function(localities_tables) {
  sheet_names <- names(localities_tables[[1]])
  tables <- lapply(sheet_names, function(sheet) {
    dplyr::bind_rows(lapply(localities_tables, `[[`, sheet))
  })
  names(tables) <- sheet_names
  tables
}

# Stage a complete workbook beside its destination before replacing an older file.
write_staged_workbook <- function(output_path, write_file) {
  staged_path <- tempfile(
    pattern = ".lp-workbook-",
    tmpdir = fs::path_dir(output_path),
    fileext = ".xlsx"
  )
  on.exit(unlink(staged_path), add = TRUE)

  write_file(staged_path)
  fs::file_chown(staged_path, group_id = "LIST_analytics")

  if (!file.rename(staged_path, output_path)) {
    stop(
      "Could not replace the existing workbook: ",
      output_path,
      call. = FALSE
    )
  }

  output_path
}

write_data_workbook <- function(tables, output_path) {
  write_staged_workbook(output_path, function(staged_path) {
    wb <- openxlsx::createWorkbook()

    for (sheet in names(tables)) {
      output <- tables[[sheet]]
      openxlsx::addWorksheet(wb, sheetName = sheet)
      openxlsx::writeData(wb, sheet = sheet, x = output)
      openxlsx::setColWidths(
        wb,
        sheet = sheet,
        cols = seq_len(ncol(output)),
        widths = "auto"
      )
    }

    openxlsx::addWorksheet(wb, sheetName = "Index")
    openxlsx::writeData(
      wb,
      sheet = "Index",
      x = data.frame(Sheet_name = names(tables))
    )
    openxlsx::saveWorkbook(wb, staged_path, overwrite = TRUE)
  })
}

write_sdc_workbook <- function(tables, output_path) {
  write_staged_workbook(output_path, function(staged_path) {
    save_dataframes_to_excel(
      dataframes = unname(tables),
      sheet_names = names(tables),
      file_path = staged_path
    )
  })
}
