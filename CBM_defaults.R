defineModule(sim, list(
  name = "CBM_defaults",
  description = "Reads in all the default values for CBM-CFS3 for Canada",
  keywords = c("CBM-CFS3", "forest carbon","Canada parameters"),
  authors = c(
    person("Celine", "Boisvenue", email = "celine.boisvenue@nrcan-rncan.gc.ca", role = c("aut", "cre"))
  ),
  childModules = character(0),
  version = list(CBM_defaults = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "CBM_defaults.Rmd"),
  reqdPkgs = list("RSQLite", "data.table", "CBMutils"
                  ##TODO: get this message currently when adding CBMUtils: CBMutils not on CRAN; checking CRAN archives ...
  ),

  parameters = bindrows( ##TODO: these are all default SpaDES parameters, not sure if all are needed here
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?") ##TODO: keep if caching
  ),

  inputObjects = bindrows( ##TODO: find inputs (so far just dbPath), should archiveIndex be an input? prob not
    expectsInput(objectName = "dbPath", objectClass = "character", desc = NA, sourceURL = NA),

  ),
  outputObjects = bindrows(
    createsOutput(objectName = "disturbanceMatrix", objectClass = "dataset", desc = NA),
    createsOutput(objectName = "spinupSQL", objectClass = "dataset", desc = NA),
    createsOutput(objectName = "forestTypeId", objectClass = "dataset", desc = NA),
    createsOutput(objectName = "pooldef", objectClass = "character", desc = NA),
    createsOutput(objectName = "poolCount", objectClass = "numeric", desc = NA)##TODO: add missing outputs
  )
))

##TODO this is copied from current CBM_defaults, unsure if that needs to change? it's still only 1 event
doEvent.CBM_defaults <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "CBM_defaults", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "CBM_defaults", "save")
    },
    # plot = {
    #
    # },
    # save = {
    #
    # },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
      "' in module '", current(sim)[1, "moduleName", with = FALSE], "'",
      sep = ""
    ))
  )
  return(invisible(sim))
}
### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  #get database
  archiveIndex <- dbConnect(dbDriver("SQLite"), sim$dbPath)

  #extract disturbance tables
  matrices2 <- as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM disturbance_matrix_association"))
  matrices3 <- as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM disturbance_matrix_tr"))
  matrices3 <- matrices3[locale_id <= 1,]
  matrices6 <- as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM disturbance_type_tr"))
  matrices6 <- matrices6[locale_id <= 1,]
  disturbance1 <- matrices2[matrices3, on = .(disturbance_matrix_id = disturbance_matrix_id), allow.cartesian = TRUE]
  sim$disturbanceMatrix <- disturbance1[matrices6, on = .(disturbance_type_id = disturbance_type_id, locale_id = locale_id), allow.cartesian = TRUE]
##TODO this has 2/3 columns also needed for disturbance_type_ref_en_ca (spatial_unit_id and disturbance_type_id).
  #This version has spanish and russian disturbance translations removed. I kept french in as I assumed it would likely be needed for quebec data

   sim$matrices5 <- as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM disturbance_type"))
   ##TODO is id = to disturbance_type_id?

  #extract spinup and spatial unit ID tables
  spatialUnitIds <- as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM spatial_unit"))
  spinupParameter <-  as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM spinup_parameter"))
  #create spinupSQL
  sim$spinupSQL <- spatialUnitIds[spinupParameter, on = .(spinup_parameter_id = id)]

  #extract for pooldef
  ##TODO pooldef from the SQL database is OUTDATED.once fixed, add this back in and remove the hard code below.
  #pooldef <- dbGetQuery(archiveIndex, "SELECT * FROM pool")
  #sim$pooldef <- as.character(pooldef$code)
  sim$pooldef = c(
    "Input","Merch", "Foliage", "Other", "CoarseRoots", "FineRoots",
    "AboveGroundVeryFastSoil", "BelowGroundVeryFastSoil",
    "AboveGroundFastSoil", "BelowGroundFastSoil", "MediumSoil",
    "AboveGroundSlowSoil", "BelowGroundSlowSoil", "StemSnag",
    "BranchSnag", "CO2", "CH4", "CO", "NO2", "Products")
  sim$poolCount <- length(sim$pooldef)

  #find forest_type_id
  forestTypeId <- as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM forest_type_tr"))
  sim$forestTypeId <- forestTypeId[, .(is_sw = any(forest_type_id == 1)), .(name, locale_id, forest_type_id)]

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

.inputObjects <- function(sim) {

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.") ##TODO: figure out what this does/means, and what needs to be changed for this module

  # ! ----- EDIT BELOW ----- ! #

  if (!suppliedElsewhere(sim$dbPath)) {
    ##TODO: cant get prepInputs to properly download this file without errors, this is the workaround I got to work. Downloads the database properly.
   sim$dbPath <- prepInputs(url = "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.8340.362.db",
                        targetFile = "cbm_defaults_v1.2.8340.362.db",
                        alsoExtract = NA,
                        destinationPath = "inputs",
                        fun = NA,
                        purge = 7
                        )
    ## download file here: https://github.com/cat-cfs/libcbm_py/tree/main/libcbm/resources/cbm_defaults_db
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
