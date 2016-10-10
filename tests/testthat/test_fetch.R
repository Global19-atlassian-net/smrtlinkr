library(smrtlinkr)
context("webRetrieval")

# Just some simple tests to verify behavior, typically I just validate that the
# query returned a dataframe via one mechanism or another
test_that("typesQuery", {
	df = fetchDataSetTypes()
	# We only have 9 allowable errors
	expect_equal(nrow(df), 9)

	df2 = fetchDataSetsByType("references")
	expect_gte(length(grep("coli", df2$name)), 2)
})

test_that("runsQuery", {
  d2 = fetchRunDesigns()
  expect_gte(nrow(d2), 1)

})

test_that("jobsQuery", {

  df = fetchJobTypes()
  # We just verify that returned at least a few rows
  expect_gt(nrow(df), 10)

  df = fetchJobsByType("pbsmrtpipe")
  # We just verify that returned at least a few rows
  expect_gt(nrow(df), 10)

  df = fetchJobDataStore("pbsmrtpipe", 5270L)
  expect_equal(nrow(df), 4)

  df = fetchJobReports("pbsmrtpipe", 5270L)
  expect_equal(df, nrow(2))

})

test_that("dataQuery", {

  df = fetchSubreadSetInfo(541L)
  expect_equal(df$name, "2016-08-29_A6_15Kecoli1.5x_Seabiscuit_analogs_120fmol_oldtube_BA018272")

  df  = fetchReferenceSetInfo(68L)
  expect_equal(df$name, "ecoliK12_pbi_March2013")

})




