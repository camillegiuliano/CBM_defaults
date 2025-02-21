
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Module runs with defaults", {

  ## Run simInit and spades ----

  # Set project path
  projectPath <- file.path(spadesTestPaths$temp$projects, "1-defaults")
  dir.create(projectPath)
  withr::local_dir(projectPath)

  # Set up project
  simInitInput <- SpaDES.project::setupProject(

    modules = "CBM_defaults",
    paths   = list(
      projectPath = projectPath,
      modulePath  = spadesTestPaths$temp$modules,
      inputPath   = spadesTestPaths$temp$inputs,
      packagePath = spadesTestPaths$temp$packages,
      cachePath   = file.path(projectPath, "cache"),
      outputPath  = file.path(projectPath, "outputs")
    )
  )

  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)

  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)

  expect_s4_class(simTest, "simList")


  ## Check output 'species_tr' ----

  expect_true(!is.null(simTest$species_tr))


  ## Check output 'disturbanceMatrix' ----

  expect_true(!is.null(simTest$disturbanceMatrix))


  ## Check output 'spinupSQL' ----

  expect_true(!is.null(simTest$spinupSQL))


  ## Check output 'forestTypeId' ----

  expect_true(!is.null(simTest$forestTypeId))


  ## Check output 'pooldef' ----

  expect_true(!is.null(simTest$pooldef))


  ## Check output 'poolCount' ----

  expect_true(!is.null(simTest$poolCount))


  ## Check output 'ecoLocator' ----

  expect_true(!is.null(simTest$ecoLocator))
  expect_true(inherits(simTest$ecoLocator, "sf"))


  ## Check output 'spuLocator' ----

  expect_true(!is.null(simTest$spuLocator))
  expect_true(inherits(simTest$spuLocator, "sf"))


})


