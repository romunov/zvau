devtools::load_all(".")

makeNGSfilter(sheet = "../NGS_R_python_scripts/gatc_trial/DNA_plate_SLO_FR bear.xlsx", 
              template = "../NGS_R_python_scripts/gatc_trial/rawdata_gatc_medvedi.ngsfilter",
              experiment = "UA_GATC",
              num.replicates = 1,
              filename = "../NGS_R_python_scripts/gatc_trial/test.ngsfilter")
