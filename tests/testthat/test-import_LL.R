test_that("import works", {
  path <- 
    system.file(
      "extdata", 
      package = "LightLogR"
      )
  filename <- "205_actlumus_Log_1020_20230904101707532.txt.zip"
  tz <- "UTC"
  pattern <- "^(\\d{3})"
  data <- import$ActLumus(filename, path, tz = tz, auto.id = pattern, silent = TRUE)
  import_cols <- c("Id",
                   "file.name",
                   "Datetime",
                   "MS",
                   "EVENT",
                   "TEMPERATURE",
                   "EXT.TEMPERATURE",
                   "ORIENTATION",
                   "PIM",
                   "PIMn",
                   "TAT",
                   "TATn",
                   "ZCM",
                   "ZCMn",
                   "LIGHT",
                   "AMB.LIGHT",
                   "RED.LIGHT",
                   "GREEN.LIGHT",
                   "BLUE.LIGHT",
                   "IR.LIGHT",
                   "UVA.LIGHT",
                   "UVB.LIGHT",
                   "STATE",
                   "CAP_SENS_1",
                   "CAP_SENS_2",
                   "F1",
                   "F2",
                   "F3",
                   "F4",
                   "F5",
                   "F6",
                   "F7",
                   "F8",
                   "MEDI",
                   "CLEAR")
  #check if the data has the correct dimensions
  expect_equal(dim(data), c(61016, 35))
  #check if the data has the correct column names
  expect_equal(names(data), import_cols)
  #check if the function extracted the correct name from the filepath and whether there is only one
  expect_equal(unique(data$Id) %>% as.character(), "205")
})

test_that("VEET import handles ragged mixed-modality files", {
  filename <- tempfile(fileext = ".csv")
  writeLines(
    c(
      paste(c(1719835200, "PHO", 100, 1, 10:18, 0, 100), collapse = ","),
      paste(c(1719835230, "TOF", seq_len(256)), collapse = ","),
      paste(c(1719835260, "PHO", 101, 2, 20:28, 1, 200), collapse = ",")
    ),
    filename
  )
  
  data <- import$VEET(
    filename,
    modality = "PHO",
    auto.plot = FALSE,
    silent = TRUE
  )
  
  expect_equal(nrow(data), 2)
  expect_equal(data$modality, c("PHO", "PHO"))
  expect_equal(data$Clear, c(100, 200))
  expect_equal(data$s910, c(18, 28))
})
