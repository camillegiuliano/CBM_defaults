defineModule(sim, list(
  name = "CBM_defaults",
  description = "Provides CBM-CFS3 defaults for Canada",
  keywords = c("CBM-CFS3", "forest carbon", "Canada parameters"),
  authors = c(
    person("Celine", "Boisvenue", email = "celine.boisvenue@nrcan-rncan.gc.ca", role = c("aut", "cre"))
  ),
  childModules = character(0),
  version = list(CBM_defaults = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "CBM_defaults.Rmd"),
  reqdPkgs = list("RSQLite", "data.table", "sf"),
  parameters = bindrows(
    defineParameter(".useCache", "logical", FALSE, NA, NA, NA)
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "dbPath", objectClass = "character", desc = NA,
                 sourceURL = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.8340.362.db"),
    expectsInput(objectName = "dbPathURL", objectClass = "character",
                 desc = "URL for dbPath"),
    expectsInput(objectName = "dMatrixAssociation", objectClass = "data.frame", desc = NA,
                 sourceURL = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_exn/disturbance_matrix_association.csv"),
    expectsInput( objectName = "dMatrixAssociationURL", objectClass = "character",
      desc = "URL for dMatrixAssociation"),
    expectsInput(objectName = "dMatrixValue", objectClass = "data.frame", desc = NA,
                 sourceURL = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_exn/disturbance_matrix_value.csv"),
    expectsInput( objectName = "dMatrixValueURL", objectClass = "character",
                  desc = "URL for dMatrixValue"),
    expectsInput(
      objectName = "ecoLocator", objectClass = "sf",
      sourceURL = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
      desc = "Canada's ecozones as polygon features"),
    expectsInput(
      objectName = "ecoLocatorURL", objectClass = "character",
      desc = "URL for ecoLocator"),
    expectsInput(
      objectName = "spuLocator", objectClass = "sf|SpatRaster",
      sourceURL = "https://drive.google.com/file/d/1D3O0Uj-s_QEgMW7_X-NhVsEZdJ29FBed",
      desc = "Canada's spatial units as polygon features"),
    expectsInput(
      objectName = "spuLocatorURL", objectClass = "character",
      desc = "URL for spuLocator")
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "species_tr",        objectClass = "data.table", desc = NA),
    createsOutput(objectName = "disturbanceMatrix", objectClass = "data.table", desc = NA),
    createsOutput(objectName = "cTransfers",        objectClass = "data.table", desc = NA),
    createsOutput(objectName = "spinupSQL",         objectClass = "data.table", desc = NA),
    createsOutput(objectName = "forestTypeId",      objectClass = "data.table", desc = NA),
    createsOutput(objectName = "pooldef",           objectClass = "character",  desc = NA),
    createsOutput(objectName = "poolCount",         objectClass = "numeric",    desc = NA),
    createsOutput(
      objectName = "ecoLocator", objectClass = "sf",
      desc = "Canada's ecozones as polygon features with field 'ecoID' containing ecozone IDs"),
    createsOutput(
      objectName = "spuLocator", objectClass = "sf",
      desc = "Canada's spatial units as polygon features with field 'spuID' containing spatial unit IDs")
  )
))

doEvent.CBM_defaults <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

Init <- function(sim) {

  ## Read the CBM-CFS3 Database ----

  # Connect to database
  archiveIndex <- dbConnect(dbDriver("SQLite"), sim$dbPath)
  on.exit(dbDisconnect(archiveIndex))

  # dbListTables(archiveIndex)
  # [1] "admin_boundary"                       "admin_boundary_tr"
  # [3] "afforestation_initial_pool"           "afforestation_pre_type"
  # [5] "afforestation_pre_type_tr"            "biomass_to_carbon_rate"
  # [7] "composite_flux_indicator"             "composite_flux_indicator_category"
  # [9] "composite_flux_indicator_category_tr" "composite_flux_indicator_tr"
  # [11] "composite_flux_indicator_value"       "decay_parameter"
  # [13] "disturbance_matrix"                   "disturbance_matrix_association"
  # [15] "disturbance_matrix_tr"                "disturbance_matrix_value"
  # [17] "disturbance_type"                     "disturbance_type_tr"
  # [19] "dom_pool"                             "eco_boundary"
  # [21] "eco_boundary_tr"                      "flux_indicator"
  # [23] "flux_indicator_sink"                  "flux_indicator_source"
  # [25] "flux_process"                         "forest_type"
  # [27] "forest_type_tr"                       "genus"
  # [29] "genus_tr"                             "growth_multiplier_series"
  # [31] "growth_multiplier_value"              "land_class"
  # [33] "land_class_tr"                        "land_type"
  # [35] "locale"                               "pool"
  # [37] "pool_tr"                              "random_return_interval"
  # [39] "root_parameter"                       "slow_mixing_rate"
  # [41] "spatial_unit"                         "species"
  # [43] "species_tr"                           "spinup_parameter"
  # [45] "stump_parameter"                      "turnover_parameter"
  # [47] "vol_to_bio_factor"                    "vol_to_bio_forest_type"
  # [49] "vol_to_bio_genus"                     "vol_to_bio_species"


#extract species tables
  # this table has species_id, locale_id, name
species_tr <- as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM species_tr"))
#keep english only
sim$species_tr <- species_tr[locale_id <= 1,]

  # This table, matrices6, has all the names associated with
  # disturbance_type_id. disturbance_type_id, along with spatial_unit_id and a
  # sw_hw flag is how libcbm connects (internally) to the proportions. In the
  # libcbm stepping function libcbmr::cbm_exn_step(), disturbance_type_id is in
  # cbm_vars$parameters$disturbance_type column and in the libcbm spinup
  # function libcbmr::cbm_exn_spinup(), the disturbance_type_id is passed in the
  # historical/lastpass values.
  matrices6 <- as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM disturbance_type_tr"))
  # matrices6 has French, Spanish and Russian disturbance translations, here we
  # only keep one copy in English.
  matrices6 <- matrices6[locale_id <= 1,]
  # only keep the colums we need, disturbance_type_id and its associated name.
  matrices6 <- matrices6[,.(disturbance_type_id, name, description)]
  # $disturbanceMatrix links together spatial_unit_id disturbance_type_id
  # disturbance_matrix_id, the disturbance names and descriptions.
  disturbanceMatrix <- sim$dMatrixAssociation[matrices6, on = .(disturbance_type_id = disturbance_type_id), allow.cartesian = TRUE]
  sim$disturbanceMatrix <- disturbanceMatrix

  # here we want to match sim$dMatrixValue with disturbanceMatrix so that sim$CTransfers has disturbance names.
  # CTransfers will have to be subset to the regions' spatial_unit_id.
  disturbanceNames <- unique(disturbanceMatrix[,.(disturbance_type_id, disturbance_matrix_id, name, description, spatial_unit_id)])
  cTransfers <- sim$dMatrixValue[disturbanceNames, on = .(disturbance_matrix_id = disturbance_matrix_id), allow.cartesian=TRUE]
  sim$cTransfers <- cTransfers

  # Get location and mean_annual_temperature for each spatial_unit, along with
  # other spinup parameters. These are needed to create sim$level3DT in
  # CBM_dataPrep_XX, which are in turn used in the CBM_core to create the
  # spinup_parameters in CBM_core.
  spatialUnitIds <- as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM spatial_unit"))
  spinupParameter <-  as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM spinup_parameter"))
  #create $spinupSQL for use in other modules
  sim$spinupSQL <- spatialUnitIds[spinupParameter, on = .(spinup_parameter_id = id)]

  #extract for pooldef
  ##TODO pooldef from the SQL database is OUTDATED. Need to coordinate with
  ##Scott to see if the SQLight can be modified and to identify where in libcbm
  ##scripts poolDef is defined (probably in cbm_exn_get_default_parameters()).
  ##Once fixed/identified, add info and this back in
  ##and remove the hard code below.
  # pooldef <- dbGetQuery(archiveIndex, "SELECT * FROM pool")
  # sim$pooldef <- as.character(pooldef$code)

  sim$pooldef = c(
    "Input","Merch", "Foliage", "Other", "CoarseRoots", "FineRoots",
    "AboveGroundVeryFastSoil", "BelowGroundVeryFastSoil",
    "AboveGroundFastSoil", "BelowGroundFastSoil", "MediumSoil",
    "AboveGroundSlowSoil", "BelowGroundSlowSoil", "StemSnag",
    "BranchSnag", "CO2", "CH4", "CO", "NO2", "Products")
  ##TODO $poolCount is not needed anymore as a spinup_parameter with the
  ##migration to libcbm. Once the last module has transitioned
  ##(CBM_dataPrep_XX), we can remove this from CBM_defaults.
  sim$poolCount <- length(sim$pooldef)

  ##TODO right now (SK examples) forest_type_id is provided by the user via
  ##gcMetaEg.csv. But the user could provide the forest type via the name in the
  ##"name" column below. sw_hw identification is important as it links to the
  ##carbon transfer proportion matrices. Right now, CBM_vol2biomass deals with
  ##forestType on lines 606-616, and CBM_core from line 313-319. We need a more
  ##generalized option for users that uses the table below.
  #find forest_type_id
  forestTypeId <- as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM forest_type_tr"))
  sim$forestTypeId <- forestTypeId[, .(is_sw = any(forest_type_id == 1)), .(name, locale_id, forest_type_id)]
  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

.inputObjects <- function(sim) {

  # CBM-CFS3 Database
  if (!suppliedElsewhere("dbPath", sim)){
    if (suppliedElsewhere("dbPathURL", sim)){

      sim$dbPath <- prepInputs(
        destinationPath = inputPath(sim),
        url = sim$dbPathURL
      )

    }else{

      sim$dbPath <- prepInputs(
        destinationPath = inputPath(sim),
        url         = extractURL("dbPath"),
        targetFile  = "cbm_defaults_v1.2.8340.362.db",
        fun         = NA,
        purge = 7 ##keep this in as it solves the malformed disc error when running in certain scenarios
      )
    }
  }

  # Disturbance Matrix Association
  if (!suppliedElsewhere("dMatrixAssociation", sim)) {
    if (!suppliedElsewhere("dMatrixAssociationURL", sim)) {
      sim$dMatrixAssociationURL <- extractURL("dMatrixAssociation")
    }
    sim$dMatrixAssociation <- prepInputs(url = sim$dMatrixAssociationURL,
                               targetFile = "disturbance_matrix_association.csv",
                               destinationPath = inputPath(sim),
                               fun = fread
                               )
  }

  #Disturbance Matrix Value
  if (!suppliedElsewhere("dMatrixValue", sim)) {
    if (!suppliedElsewhere("dMatrixValueURL", sim)) {
      sim$dMatrixValueURL <- extractURL("dMatrixValue")
    }
    sim$dMatrixValue <- prepInputs(url = sim$dMatrixValueURL,
                                         targetFile = "disturbance_matrix_value.csv",
                                         destinationPath = inputPath(sim),
                                         fun = fread
    )
  }

  # Canada ecozones
  if (!suppliedElsewhere("ecoLocator", sim)){

    if (suppliedElsewhere("ecoLocatorURL", sim) &
        !identical(sim$ecoLocatorURL, extractURL("ecoLocator"))){

      sim$ecoLocator <- prepInputs(
        destinationPath = inputPath(sim),
        url = sim$ecoLocatorURL
      )

    }else{

      ## 2024-12-04 NOTE:
      ## Multiple users had issues downloading and extracting this file via prepInputs.
      ## Downloading the ZIP directly and saving it in the inputs directory works OK.
      sim$ecoLocator <- tryCatch(

        prepInputs(
          destinationPath = inputPath(sim),
          url         = extractURL("ecoLocator"),
          filename1   = "ecozone_shp.zip",
          targetFile  = "ecozones.shp",
          alsoExtract = "similar",
          fun         = sf::st_read(targetFile, quiet = TRUE)
        ),

        error = function(e) stop(
          "Canada ecozones Shapefile failed be downloaded and extracted:\n", e$message, "\n\n",
          "If this error persists, download the ZIP file directly and save it to the inputs directory.",
          "\nDownload URL: ", extractURL("ecoLocator"),
          "\nInputs directory: ", normalizePath(inputPath(sim), winslash = "/"),
          call. = FALSE))

      # Make ecoID field
      sim$ecoLocator <- cbind(ecoID = sim$ecoLocator$ECOZONE, sim$ecoLocator)

      sf::st_agr(sim$ecoLocator) <- "constant"
    }
  }

  # Canada spatial units
  if (!suppliedElsewhere("spuLocator", sim)){

    if (suppliedElsewhere("spuLocatorURL", sim) &
        !identical(sim$spuLocatorURL, extractURL("spuLocator"))){

      sim$spuLocator <- prepInputs(
        destinationPath = inputPath(sim),
        url = sim$spuLocatorURL
      )

    }else{

      sim$spuLocator <- prepInputs(
        destinationPath = inputPath(sim),
        url         = extractURL("spuLocator"),
        filename1   = "spUnit_Locator.zip",
        targetFile  = "spUnit_Locator.shp",
        alsoExtract = "similar",
        fun         = sf::st_read(targetFile, quiet = TRUE)
      )

      # Make spuID field
      sim$spuLocator <- cbind(spuID = sim$spuLocator$spu_id, sim$spuLocator)

      sf::st_agr(sim$spuLocator) <- "constant"
    }
  }

  # Return sim
  return(invisible(sim))

}
