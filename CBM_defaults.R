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
  reqdPkgs = list("RSQLite", "data.table"
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

  inputObjects = bindrows( ##TODO: find inputs (so far just dbPath)
    expectsInput(objectName = "dbPath", objectClass = "character", desc = NA, sourceURL = NA),

  ),
  outputObjects = bindrows( ##TODO: find outputs, cbmdata, disturbance data, species data
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "disturbanceMatrix", objectClass = "dataset", desc = NA),
    createsOutput(objectName = "spinupSQL", objectClass = "dataset", desc = NA),
    createsOutput(objectName = "forestTypeId", objectClass = "dataset", desc = NA),
    createsOutput(objectName = "pooldef", objectClass = "character", desc = NA) ##TODO: add missing outputs
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

  browser()

  #extract disturbance tables
  matrices2 <- as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM disturbance_matrix_association"))
  matrices3 <- as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM disturbance_matrix_tr"))
  matrices6 <- dbGetQuery(archiveIndex, "SELECT * FROM disturbance_type_tr")
  disturbance1 <- matrices2[matrices3, on = .(disturbance_matrix_id = disturbance_matrix_id), allow.cartesian = TRUE]
  disturbanceMatrix <- disturbance1[matrices6, on = .(disturbance_type_id = disturbance_type_id, locale_id = locale_id), allow.cartesian = TRUE]
##TODO this has 2/3 columns also needed for disturbance_type_ref_en_ca (spatial_unit_id and disturbance_type_id).
  # however this is a huge file because it has disturbance names in english, french, spanish, and russian.

   sim$matrices5 <- as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM disturbance_type"))
   ##TODO is id = to disturbance_type_id?

  #extract spinup and spatial unit ID tables
  spatialUnitIds <- as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM spatial_unit"))
  spinupParameter <-  as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM spinup_parameter"))
  #create spinupSQL
  sim$spinupSQL <- spatialUnitIds[spinupParameter, on = .(spinup_parameter_id = id)]

  #extract for pooldef
  pooldef <- dbGetQuery(archiveIndex, "SELECT * FROM pool")
  sim$pooldef <- as.character(pooldef$code)
  sim$poolCount <- length(sim$pooldef)

  #find forest_type_id
  sim$forestTypeId <- as.data.table(dbGetQuery(archiveIndex, "SELECT * FROM forest_type_tr"))

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

.inputObjects <- function(sim) {

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.") ##TODO: figure out what this does/means, and what needs to be changed for this module

  # ! ----- EDIT BELOW ----- ! #

  if (!suppliedElsewhere(sim$dbPath)) {
    sim$dbPath <- "C:/Camille/GitHub/spadesCBM/defaultDB/cbm_defaults_v1.2.8340.362.db"
    ##TODO: this eventually needs to not lead to a locally stored file
    ## download file here: https://github.com/cat-cfs/libcbm_py/tree/main/libcbm/resources/cbm_defaults_db
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
