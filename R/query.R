DEFAULT_SERVER = "smrtlink-internal"
DEFAULT_PORT = "8081"
DEFAULT_SERVER_SUFFIX = ".nanofluidics.com:"
allowable_job_types = c('convert-rs-movie',
                        'mock-pbsmrtpipe',
                        'pbsmrtpipe',
                        'simple',
                        'merge-datasets',
                        'import-datastore',
                        'convert-fasta-reference',
                        'import-dataset')
verifyJobType <- function(jobType) {
  if (!(jobType %in% allowable_job_types)) {
    stop(paste("Argument", jobType, "was not one of the allowable arguments (must be one of", paste(allowable_job_types, collapse = ", "), ")", sep = " ", collapse = ""))
  }
}

verifyAndReturnIntegerID <- function(x) {
  if (length(x) != 1) {
    stop("Length of ID != 1")
  }
  if (class(x) == "numeric") {
    warning("ID was a numeric type, converting to integer by rounding.")
    return(as.integer(2))
  } else if (class(x) == "integer") {
    return(x)
  } else {
    stop("ID wasn't a numeric or integer type.")
  }
}


# Internal convience function to fetch JSON from a usual URL
fetchFromServer <- function(server, port, tail) {
  url = paste("http://", server, DEFAULT_SERVER_SUFFIX, port, tail, sep = "")
  jsonlite::fromJSON(url)
}

#' List All Run Designs
#'
#' See: http://smrtflow.readthedocs.io/en/latest/SMRT_Link_Service_API/run_design_endpoints/list_all_run_designs.html
#'
#' @param server A server address, to which ".nanofluidics.com will be appended
#' @param  port A port to query the server on (usually 8081 or 9091)
#' @export
fetchRunDesigns <- function(server = DEFAULT_SERVER, port = DEFAULT_PORT) {
  fetchFromServer(server, port, "/smrt-link/runs")
}

#' List All Dataset Types
#'
#' See: http://smrtflow.readthedocs.io/en/latest/SMRT_Secondary_Service_API/dataset_service_endpoints/list_all_dataset_types.html
#'
#' @param server A server address, to which ".nanofluidics.com will be appended
#' @param  port A port to query the server on (usually 8081 or 9091)
#' @export
fetchDataSetTypes <-  function(server = DEFAULT_SERVER, port = DEFAULT_PORT) {
  fetchFromServer(server, port, "/secondary-analysis/dataset-types")
}

#' List All Datasets by Type
#'
#' See: http://smrtflow.readthedocs.io/en/latest/SMRT_Secondary_Service_API/dataset_service_endpoints/list_all_datasets_by_type.html
#'
#' @param datasetType What type of dataset to fetch? (should be one of references, ccsreads, contigs, subreads, barcodes, ccsalignments, hdfsubreads, alignments gmapreferences, references
#' @param server A server address, to which ".nanofluidics.com will be appended
#' @param  port A port to query the server on (usually 8081 or 9091)
#' @export
fetchDataSetsByType <-  function(datasetType, server = DEFAULT_SERVER, port = DEFAULT_PORT) {
  allowable = c("references",
                "ccsreads",
                "contigs",
                "subreads",
                "barcodes",
                "ccsalignments",
                "hdfsubreads",
                "alignments",
                "gmapreferences")
  if (!(datasetType %in% allowable)) {
    stop(paste("Argument", datasetType, "was not one of the allowable arguments (must be one of", paste(allowable, collapse = ", "), ")", sep = " ", collapse = ""))
  }
  fetchFromServer(server, port, paste("/secondary-analysis/datasets/", datasetType, sep = ""))
}

#' List All Job Types
#'
#' See: http://smrtflow.readthedocs.io/en/latest/SMRT_Secondary_Service_API/jobs_service_endpoints/list_all_job_types.html
#'
#' @param server A server address, to which ".nanofluidics.com will be appended
#' @param  port A port to query the server on (usually 8081 or 9091)
#' @export
fetchJobTypes <-  function(server = DEFAULT_SERVER, port = DEFAULT_PORT) {
  fetchFromServer(server, port, "/secondary-analysis/job-manager/job-types")
}

#' List All Job Types
#'
#' See: http://smrtflow.readthedocs.io/en/latest/SMRT_Secondary_Service_API/jobs_service_endpoints/list_all_jobs_by_type.html
#' @param jobType JobType to fetch jobs for, must be one of 'convert-rs-movie', 'mock-pbsmrtpipe', 'pbsmrtpipe', 'simple', 'merge-datasets', 'import-datastore', 'convert-fasta-reference', 'import-dataset
#' @param server A server address, to which ".nanofluidics.com will be appended
#' @param  port A port to query the server on (usually 8081 or 9091)
#' @export
fetchJobsByType <-  function(jobType, server = DEFAULT_SERVER, port = DEFAULT_PORT) {
  verifyJobType(jobType)
  fetchFromServer(server, port, paste("/secondary-analysis/job-manager/jobs/", jobType, sep = ""))
}

#' Fetch job datastore by Job Type and ID
#'
#' See: http://smrtflow.readthedocs.io/en/latest/SMRT_Secondary_Service_API/jobs_service_endpoints/fetch_job_datastore.html
#'
#' @param jobTypeID name of the job type
#' @param id The id of the job
#' @param server A server address, to which ".nanofluidics.com will be appended
#' @param port A port to query the server on (usually 8081 or 9091)
#' @example fetchJobDataStore("pbsmrtpipe", 5270L)
#' @export
fetchJobDataStore <-  function(jobType, id, server = DEFAULT_SERVER, port = DEFAULT_PORT) {
  verifyJobType(jobType)
  id = verifyAndReturnIntegerID(id)
  fetchFromServer(server, port,
                  paste("/secondary-analysis/job-manager/jobs/", jobType, "/",
                        id, "/datastore", sep = ""))
}

#' Fetch job report by Job Type and ID
#'
#' See: http://smrtflow.readthedocs.io/en/latest/SMRT_Secondary_Service_API/jobs_service_endpoints/fetch_job_reports.html
#'
#' @param jobTypeID name of the job type
#' @param id The id of the job
#' @param server A server address, to which ".nanofluidics.com will be appended
#' @param  port A port to query the server on (usually 8081 or 9091)
#' @export
fetchJobReports <-  function(jobType, id, server = DEFAULT_SERVER, port = DEFAULT_PORT) {
  verifyJobType(jobType)
  id = verifyAndReturnIntegerID(id)
  fetchFromServer(server, port,
                  paste("/secondary-analysis/job-manager/jobs/", jobType, "/",
                        id, "/reports", sep = ""))
}





#' Get the subreads and reference paths for a resequencing job that was
#' executed on SMRTLink
#'
#' @param tbl A table returned by fetchJobsByType
#' @return  A new data frame with the job name, subreadset path and reference path
#' @export
fetchSubreadsAndReferencesForResequencingJobs <- function(tbl, server = DEFAULT_SERVER, port = DEFAULT_PORT) {
  if (class(tbl) != "data.frame") stop("Expected tbl to be a dataframe returned by fetchJobReports")
  # Verify column names
  expected_names = c("name", "updatedAt", "path", "state", "uuid", "jobTypeId", "id", "comment", "createdAt", "jsonSettings", "createdBy")
  if (any(colnames(tbl) != expected_names)) stop("Expected tbl to be a dataframe returned by fetchJobReports")

  # Now let's get the resequencing jobs
  reseqRows = grep("resequencing", tbl$comment)
  if (length(reseqRows) != nrow(tbl)) warning("Not every job in the table was a resequencing job. Only returning values for resequencing rows.")
  nd = tbl[reseqRows, ]
  jsons = lapply(nd$jsonSettings, jsonlite::fromJSON)
  getSubreadSetPath <- function(x) {
      id = x$entryPoints$datasetId[x$entryPoints$entryId == "eid_subread"]
      fetchSubreadSetInfo(id)$path
  }
  getReferencePath <- function(x) {
    id = x$entryPoints$datasetId[x$entryPoints$entryId == "eid_ref_dataset"]
    fetchReferenceSetInfo(id)$path
  }
  subpaths = sapply(jsons, getSubreadSetPath)
  refpaths = sapply(jsons, getReferencePath)
  names = sapply(jsons, function(x) x$name)
  data.frame(name = names, subread = subpaths, refpaths = refpaths)
}


#' Gets the data for a subreadset id given a server/port
#'
#' @export
fetchSubreadSetInfo <- function(id, server = DEFAULT_SERVER, port = DEFAULT_PORT) {
  id = verifyAndReturnIntegerID(id)
  fetchFromServer(server, port, paste("/secondary-analysis/datasets/subreads/", id, sep = ""))
}

#' Gets the data for a referenceset id given a server/port
#'
#' @export
fetchReferenceSetInfo <- function(id, server = DEFAULT_SERVER, port = DEFAULT_PORT) {
  id = verifyAndReturnIntegerID(id)
  fetchFromServer(server, port, paste("/secondary-analysis/datasets/references/", id, sep = ""))
}

#' Create resequencing condition job
#'
#' Create a resequencing condition job on the SMRTLink server, using the numeric ID
#' of a given reference and subreadset.
#'
#' @param name What is the name of the job
#' @param refID What is the reference ID
#' @param subID What is the subreadset ID
#' @param server Which server to use
#' @param port Which port to use.
#' @return A
#' @export
postReseqJob <- function(name, refID, subID, server = DEFAULT_SERVER, port = DEFAULT_PORT) {
  # Read in a template file
  jsonFile = system.file("jsonTemplate", "reseq.json", package = "smrtlinkr")
  template = readChar(jsonFile, file.info(jsonFile)$size)
  # Replace some key variables to make some new stuff
  template = sub("%%Name", paste('\"', name, '\"', sep = ""), template)
  template = sub("%%RefID", refID, template)
  template = sub("%%SubreadID", subID, template)

  # Now post to the server
  url = paste("http://", server, DEFAULT_SERVER_SUFFIX, port,
              "/secondary-analysis/job-manager/jobs/pbsmrtpipe/", sep = "")
  httr::POST(url, body = template, encode = "json",
             httr::add_headers("Content-Type" = "application/json"))
}
