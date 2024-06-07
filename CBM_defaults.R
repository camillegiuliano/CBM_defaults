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
  reqdPkgs = list("RSQLite", "data.table",
                  "PredictiveEcology/CBMutils"),
  parameters = rbind(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules,",
                          "where stochasticity and time are not relevant."))
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "sqlDir", objectClass = "character", desc = NA, sourceURL = NA),
    expectsInput(objectName = "dbPath", objectClass = "character", desc = NA, sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "cbmData", objectClass = "dataset",
                  desc = NA), ## TODO
    createsOutput(objectName = "decayRates", objectClass = "matrix",
                  desc = "decay rates per spatial unit?"),
    createsOutput(objectName = "PoolCount", objectClass = "numeric",
                  desc = "Length of pooldef"),
    createsOutput(objectName = "pooldef", objectClass = "character",
                  desc = "Vector of names (characters) for each of the carbon pools, with `Input` being the first one"),
    createsOutput(objectName = "processes", objectClass = "list",
                  desc = "decay mixing turnover and disturbances")
  )
))

## event types
#   - type `init` is required for initialiazation

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

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  sim$pooldef <- CBMutils::.pooldef
  sim$PoolCount <- length(sim$pooldef)

  # step 1 read the cbm_defaults parameter data
  spatialUnitIds <- as.matrix(getTable("spatialUnitIds.sql", sim$dbPath, sim$sqlDir))
  disturbanceMatrix <- as.matrix(getTable("disturbanceMatrix.sql", sim$dbPath, sim$sqlDir))
  # this is the S4 object that has ALL the parameters
  sim$cbmData <- new("dataset",
    turnoverRates = as.matrix(getTable("turnoverRates.sql", sim$dbPath, sim$sqlDir)),
    rootParameters = as.matrix(getTable("rootParameters.sql", sim$dbPath, sim$sqlDir)),
    decayParameters = as.matrix(getTable("decayParameters.sql", sim$dbPath, sim$sqlDir)),
    spinupParameters = as.matrix(getTable("spinupParameters.sql", sim$dbPath, sim$sqlDir)),
    climate = as.matrix(getTable("climate.sql", sim$dbPath, sim$sqlDir)),
    spatialUnitIds = spatialUnitIds,
    slowAGtoBGTransferRate = as.matrix(0.006),
    biomassToCarbonRate = as.matrix(0.5),
    stumpParameters = as.matrix(getTable("stumpParameters.sql", sim$dbPath, sim$sqlDir)),
    overmatureDeclineParameters = as.matrix(getTable("overmaturedecline.sql", sim$dbPath, sim$sqlDir)),
    disturbanceMatrix = disturbanceMatrix,
    disturbanceMatrixAssociation = as.matrix(getTable("disturbanceMatrixAssociation.sql", sim$dbPath, sim$sqlDir)),
    disturbanceMatrixValues = as.matrix(getTable("disturbanceMatrixValues.sql", sim$dbPath, sim$sqlDir)),
    landclasses = as.matrix(getTable("landclasses.sql", sim$dbPath, sim$sqlDir)),
    pools = as.matrix(getTable("pools.sql", sim$dbPath, sim$sqlDir)),
    domPools = as.matrix(getTable("domPools.sql", sim$dbPath, sim$sqlDir))
  )

  # step 2 create constant matrices from parameter data
  # these are the decay rates for all the 48 spatial units
  sim$decayRates <- spatialUnitDecayRates(sim$cbmData@climate, sim$cbmData@decayParameters, sim$cbmData@domPools)

  sim$processes <- list(
    domDecayMatrices = matrixHash(computeDomDecayMatrices(sim$decayRates, sim$cbmData@decayParameters, sim$PoolCount)),
    slowDecayMatrices = matrixHash(computeSlowDecayMatrices(sim$decayRates, sim$cbmData@decayParameters, sim$PoolCount)),
    slowMixingMatrix = matrixHash(computeSlowMixingMatrix(sim$cbmData@slowAGtoBGTransferRate, sim$PoolCount)),
    domTurnover = matrixHash(computeDomTurnoverMatrices(sim$cbmData@turnoverRates, sim$PoolCount)),
    bioTurnover = matrixHash(computeBioTurnoverMatrices(sim$cbmData@turnoverRates, sim$PoolCount)),
    disturbanceMatrices = matrixHash(loadDisturbanceMatrixIds(sim$cbmData@disturbanceMatrixValues, sim$cbmData@pools))
  )
## assertion to check if annual process proportional matrices all have a sumed value of 1 per row.
  propTransfer <- NULL
  for (i in setdiff(names(sim$processes), "disturbanceMatrices")) {
    makeDT <- matrixDT(matricesIn = sim$processes[[i]], indicesIn = names(sim$processes[[i]]))
    propCheck <- checkProp(makeDT)
    if (sum(propCheck$noLoss) != length(propCheck$noLoss))
      stop("Transfer matrices have proportions different then 1: carbon is disappearing or appearing")
  }

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  # Plot("object")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (dPath != dataPath(sim)) {
    checkPath(file.path(dPath, "cbm_defaults"), create = TRUE)
    file.copy(file.path(dataPath(sim), "cbm_defaults"), dPath, recursive = TRUE, overwrite = TRUE)
  }

  # ! ----- EDIT BELOW ----- ! #
  if (!suppliedElsewhere(sim$sqlDir)) {
    sim$sqlDir <- file.path(dPath, "cbm_defaults")
  }

  if (!suppliedElsewhere(sim$dbPath)) {
    sim$dbPath <- file.path(sim$sqlDir, "cbm_defaults.db")
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
