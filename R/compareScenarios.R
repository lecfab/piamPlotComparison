#' Render CompareScenarios
#'
#' Renders the *.Rmd-files associated to CompareScenarios. In the Rmds,
#' scenario- and historical .mif-files are loaded. Then plots are created from
#' this data. The result may be rendered to PDF or HTML. Alternatively one can
#' choose Rmd as output format and obtain a copy of the *.Rmd-files.
#'
#' @param projectLibrary \code{NULL} or \code{character(n)}. Default: \code{NULL}.
#'   An R reporting library containing Rmd files to be included.
#'   You can further filter the sections to be rendered using the \code{sections} YAML parameter.
#'   The Rmd files must be placed in a folder \code{inst/compareScenarios}.
#'   Files must have the format \code{cs_NN_XXXX.Rmd} to be considered.
#'   If the folder contains a file \code{preprocessing.Rmd}, it will be executed
#'   before rendering the sections.
#' @param mifScen \code{character(n)}, optionally named. Paths to scenario mifs.
#'   If the vector has names, those are used to refer to the scenarios in the
#'   output file.
#' @param mifHist \code{character(1)}. Path to historical mif or \code{NULL}.
#' @param outputFile \code{character(1)}. File name (without extension) of the
#'   output document to be created.
#' @param outputDir \code{character(1)}. The directory where the output document
#'   and intermediary files are created.
#' @param outputFormat \code{character(1)}, not case-sensitive. \code{"html"},
#'   \code{"pdf"}, or \code{"rmd"}.
#' @param envir \code{environment}. The environment in which the code chunks are
#'   to be evaluated. See the argument of the same name in
#'   \code{\link[rmarkdown:render]{rmarkdown::render()}}.
#'   Set this to \code{globalenv()} and \code{sections} to \code{NULL} to load
#'   an preprocess data in your global environment during development.
#' @param quiet \code{logical(1)}. Suppress printing during rendering?
#' @param mc.cores \code{integer(1)} or \code{NULL}. Default: \code{NULL}.
#'   Number of sections to render in parallel (forked worker processes, via
#'   \code{\link[parallel:mclapply]{parallel::mclapply()}}). Only used for
#'   \code{outputFormat = "pdf"}; other formats always render sequentially.
#'   Each section is rendered as an independent, self-contained document that
#'   re-reads and re-preprocesses \code{mifScen}/\code{mifHist}, so the
#'   speed-up is smaller than \code{mc.cores} would suggest for cheap mifs,
#'   but substantial once section-specific plotting dominates runtime (as is
#'   typical for large multi-scenario comparisons). The resulting per-section
#'   PDFs are combined into a single PDF using the \code{qpdf} or
#'   \code{pdfunite} command line tool (one of them must be on the \code{PATH}).
#'   Note the combined PDF has one table of contents and page numbering per
#'   section rather than a single continuous one.
#'   Set to \code{1} to disable parallel rendering and reproduce the exact
#'   original, sequential single-process behavior. Defaults to the number of
#'   CPUs allocated to the job (\code{SLURM_CPUS_PER_TASK}) or, absent that,
#'   \code{parallel::detectCores()}; always \code{1} on Windows.
#' @param ... YAML parameters, see below.
#' @importFrom piamutils getSystemFile
#' @return The value returned by \code{\link[rmarkdown:render]{rmarkdown::render()}}.
#' @section YAML Parameters:
#' \describe{
#'   \item{\code{cfgScen}}{
#'     \code{character(n) or NULL}.
#'     Paths to config.Rdata files containing the \code{cfg} object for each
#'     scenario. The paths must be provided in the same order as \code{mifScen}.
#'     If provided, some information gathered from these files is
#'     shown at the beginning of the output document.}
#'   \item{\code{docTitle}}{
#'    \code{character(n) or NULL}.
#'    Title of the document. Defaults to 'Compare Scenarios'}
#'   \item{\code{yearsScen}}{
#'     \code{numeric(n)}.
#'     Default: \code{c(seq(2005, 2060, 5), seq(2070, 2100, 10))}.
#'     Years to show for scenario data.}
#'   \item{\code{yearsHist}}{
#'     \code{numeric(n)}.
#'     Default: \code{c(seq(1960, 2020, 1), seq(2025, 2100, 5))}.
#'     Years to show for historical data.}
#'   \item{\code{yearsBarPlot}}{
#'     \code{numeric(n)}.
#'     Default: \code{c(2010, 2030, 2050, 2100)}.
#'     Years to show in bar plots of scenario data.}
#'   \item{\code{yearRef}}{
#'     \code{numeric(1)}.
#'     Default: \code{2020}.
#'     A reference year used to show relative values in Kaya decomposition.}
#'   \item{\code{reg}}{
#'     \code{NULL} or \code{character(n)}.
#'     Default: \code{NULL}.
#'     Regions to show. \code{NULL} means all.}
#'   \item{\code{modelsHistExclude}}{
#'     \code{character(n) or NULL}.
#'     Default: \code{c()}.
#'     Models in historical data to exclude.}
#'   \item{\code{sections}}{
#'     \code{character(n) or numeric(n) or NULL}.
#'     Default: \code{"all"}.
#'     Names or numbers of sections to include.
#'     Use \code{"all"} to include all available sections.
#'     Use \code{NULL} to not include any section
#'     (useful in combination with parameter \code{envir}).}
#'   \item{\code{userSectionPath}}{
#'     \code{NULL} or \code{character(n)}.
#'     Default: \code{NULL}.
#'     Path to a *.Rmd-file that may be included as additional section.}
#'   \item{\code{mainReg}}{
#'     \code{character(1)}.
#'     Default: \code{"World"}.
#'     A region for which larger plots are shown.}
#'   \item{\code{figWidth, figHeight}}{
#'     \code{numeric(1)}.
#'     Default: \code{15} and \code{10}, respectively.
#'     Size of plots in inches.}
#'   \item{\code{warning}}{
#'     \code{logical(1)}.
#'     Default: \code{TRUE}.
#'     Show warnings in output?}
#' }
#' @author Christof Schoetz, Falk Benke
#' @examples
#' \dontrun{
#' # Simple use. Creates PDF:
#' compareScenarios(
#'   projectLibrary = "mylib",
#'   mifScen = c("path/to/Base.mif", "path/to/NDC.mif"),
#'   mifHist = "path/to/historical.mif",
#'   outputFile = "CompareScenariosExample")
#' # More complex use. Creates folder with Rmds:
#' compareScenarios(
#'   projectLibrary = "mylib",
#'   mifScen = c(ScenarioName1 = "path/to/scen1.mif", ScenarioName2 = "path/to/scen2.mif"),
#'   mifHist = "path/to/historical.mif",
#'   cfgScen = c("path/to/scen1/config.RData", "path/to/scen2/config.RData"),
#'   outputDir = "path/to/output",
#'   outputFormat = "Rmd",
#'   outputFile = format(Sys.time(), "compScen_%Y%m%d-%H%M%S"),
#'   warning = FALSE,
#'   sections = c(0, 2, 3, 99),
#'   userSectionPath = "path/to/myPlots.Rmd")
#' # Use in development. Load data into global environment:
#' compareScenarios(
#'   projectLibrary = "mylib",
#'   mifScen = c("path/to/scen1.mif", "path/to/scen2.mif"),
#'   mifHist = "path/to/historical.mif",
#'   outputFile = format(Sys.time(), "cs2_load_%Y%m%d-%H%M%S"),
#'   sections = NULL,
#'   envir = globalenv())
#' }
#' @export
compareScenarios <- function(
    projectLibrary = NULL,
    mifScen,
    mifHist = NULL,
    outputDir = getwd(),
    outputFile = "CompareScenarios",
    outputFormat = "PDF",
    envir = new.env(),
    quiet = FALSE,
    mc.cores = NULL,
    ...
) {
  # Set yaml parameters and convert relative to absolute paths.
  yamlParams <- c(
    list(
      mifScen = normalizePath(mifScen, mustWork = TRUE),
      mifScenNames = names(mifScen)),
    list(...))
  if (!is.null(mifHist)) {
    yamlParams$mifHist <- normalizePath(mifHist, mustWork = TRUE)
  }
  if (!is.null(yamlParams[["cfgScen"]])) {
    yamlParams$cfgScen <- normalizePath(yamlParams$cfgScen, mustWork = TRUE)
  }
  if (!is.null(yamlParams[["userSectionPath"]])) {
    yamlParams$userSectionPath <- normalizePath(yamlParams$userSectionPath, mustWork = TRUE)
  }

  outputFormat <- tolower(outputFormat)[[1]]
  if (outputFormat == "pdf") {
    outputFormat <- "pdf_document"
  } else if (outputFormat == "html") {
    outputFormat <- "html_document"
  } else if (outputFormat == "rmd") {
    return(.compareScenarios2Rmd(projectLibrary, yamlParams, outputDir, outputFile))
  }

  # copy the template directory from the package to the outputDir because
  # rmarkdown writes to the folder containing the template.
  templateInOutputDir <- file.path(outputDir, "compareScenarios", "cs_main.Rmd")
  file.copy(piamutils::getSystemFile("compareScenarios", package = "piamPlotComparison"),
            outputDir, recursive = TRUE, copy.mode = FALSE)

  if (!is.null(projectLibrary)) {
    file.copy(piamutils::getSystemFile("compareScenarios", package = projectLibrary),
              outputDir, recursive = TRUE, copy.mode = FALSE)
  }

  # avoid potential problems with write permissions on the copied files
  Sys.chmod(list.files(outputDir, full.names = TRUE))

  if (is.null(mc.cores)) mc.cores <- .defaultMcCores()
  mc.cores <- max(1L, as.integer(mc.cores))

  if (mc.cores > 1 && outputFormat != "pdf_document") {
    if (!quiet) {
      message("Parallel rendering (mc.cores > 1) is only supported for PDF output. Rendering sequentially.")
    }
    mc.cores <- 1L
  }

  if (mc.cores > 1) {
    .renderScenariosParallel(
      templateInOutputDir = templateInOutputDir,
      outputDir = outputDir,
      outputFile = outputFile,
      outputFormat = outputFormat,
      yamlParams = yamlParams,
      quiet = quiet,
      mc.cores = mc.cores)
  } else {
    rmarkdown::render(
      templateInOutputDir,
      intermediates_dir = outputDir,
      output_dir = outputDir,
      output_file = outputFile,
      output_format = outputFormat,
      params = yamlParams,
      envir = envir,
      quiet = quiet
    )
  }

  unlink(file.path(outputDir, "compareScenarios"), recursive = TRUE)
}

# Default number of cores to use for parallel section rendering.
.defaultMcCores <- function() {
  if (!identical(.Platform$OS.type, "unix")) return(1L)
  slurmCores <- suppressWarnings(as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "")))
  if (!is.na(slurmCores) && slurmCores > 0) return(slurmCores)
  cores <- suppressWarnings(parallel::detectCores())
  if (is.na(cores) || cores < 1) 1L else cores
}

# Resolves the ordered vector of section *.Rmd file names (as in cs_main.Rmd's
# 'sectionPaths' chunk) for a given 'sections' YAML parameter.
.resolveSectionFiles <- function(csDir, sections) {
  files <- sort(list.files(csDir, pattern = "^cs_[0-9]+.*\\.Rmd$"))
  nums <- as.numeric(sub("^cs_([0-9]+).*$", "\\1", files))

  if (is.null(sections)) sections <- "all"

  if (length(sections) == 1 && identical(sections, "all")) {
    return(files)
  }
  if (is.numeric(sections)) {
    return(files[match(sections, nums)][!is.na(match(sections, nums))])
  }
  if (length(sections) > 0) {
    return(paste0("cs_", sections, ".Rmd"))
  }
  character(0)
}

# Renders each section (plus the user section, if any) as an independent,
# self-contained PDF in parallel forked worker processes, then combines the
# resulting PDFs into a single PDF.
.renderScenariosParallel <- function(templateInOutputDir, outputDir, outputFile,
                                     outputFormat, yamlParams, quiet, mc.cores) {
  csDir <- dirname(templateInOutputDir)
  sectionFiles <- .resolveSectionFiles(csDir, yamlParams[["sections"]])

  # jobs: one entry per section file, plus one more for the user section, if any.
  jobs <- as.list(sectionFiles)
  if (!is.null(yamlParams[["userSectionPath"]])) {
    jobs <- c(jobs, list(NA_character_))
  }
  if (length(jobs) == 0) {
    stop("No sections to render (the 'sections' parameter resolved to an empty selection).")
  }

  renderJob <- function(i) {
    jobParams <- yamlParams
    if (is.na(jobs[[i]])) {
      jobParams$sections <- character(0)
    } else {
      jobParams$sections <- as.numeric(sub("^cs_([0-9]+).*$", "\\1", jobs[[i]]))
      jobParams$userSectionPath <- NULL
    }
    partFile <- sprintf("%s_part%03d", outputFile, i)
    intermediatesDir <- tempfile("cs2_intermediates_")
    dir.create(intermediatesDir)
    on.exit(unlink(intermediatesDir, recursive = TRUE), add = TRUE)
    rmarkdown::render(
      templateInOutputDir,
      intermediates_dir = intermediatesDir,
      output_dir = outputDir,
      output_file = partFile,
      output_format = outputFormat,
      params = jobParams,
      envir = new.env(),
      quiet = quiet)
  }

  results <- parallel::mclapply(
    seq_along(jobs), renderJob,
    mc.cores = min(mc.cores, length(jobs)), mc.preschedule = FALSE)

  failed <- vapply(results, function(x) inherits(x, "try-error"), logical(1))
  if (any(failed)) {
    msgs <- vapply(results[failed], function(x) attr(x, "condition")$message %||% as.character(x), character(1))
    stop("Parallel rendering failed for ", sum(failed), " section(s):\n", paste(msgs, collapse = "\n"))
  }

  partFiles <- vapply(results, identity, character(1))
  finalOutput <- file.path(outputDir, paste0(outputFile, ".pdf"))
  .mergePdfs(partFiles, finalOutput)
  unlink(partFiles)
  invisible(finalOutput)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# Combines several PDFs into one, using qpdf if available, else pdfunite.
.mergePdfs <- function(files, output) {
  if (nzchar(Sys.which("qpdf"))) {
    status <- system2("qpdf", c("--empty", "--pages", files, "--", output))
  } else if (nzchar(Sys.which("pdfunite"))) {
    status <- system2("pdfunite", c(files, output))
  } else {
    stop(
      "Parallel rendering (mc.cores > 1) requires the 'qpdf' or 'pdfunite' ",
      "command line tool to combine the section PDFs, but neither was found on the PATH.")
  }
  if (!identical(status, 0L)) {
    stop("Combining section PDFs into '", output, "' failed (exit status ", status, ").")
  }
  invisible(output)
}

# Copies the CompareScenarios-Rmds to the specified location and modifies
# their YAML header according to \code{yamlParams}.
.compareScenarios2Rmd <- function(projectLibrary, yamlParams, outputDir, outputFile) {
  pathMain <- piamutils::getSystemFile("compareScenarios/cs_main.Rmd", package = "piamPlotComparison")
  linesMain <- readLines(pathMain)
  delimiters <- grep("^(---|\\.\\.\\.)\\s*$", linesMain)
  headerMain <- linesMain[(delimiters[1]):(delimiters[2])]
  yml <- yaml::yaml.load(
    headerMain,
    handlers = list(r = function(x) ymlthis::yml_params_code(!!rlang::parse_expr(x))))
  baseYaml <- ymlthis::as_yml(yml)
  newYamlParams <- baseYaml$params
  newYamlParams[names(yamlParams)] <- yamlParams

  if (!is.null(names(yamlParams$mifScen))) {
    newYamlParams$mifScenNames <- names(yamlParams$mifScen)
  }

  newYaml <- ymlthis::yml_replace(
    baseYaml,
    params = newYamlParams,
    date = format(Sys.Date()))

  pathDir <- file.path(outputDir, paste0(outputFile, "_Rmd"))

  if (!dir.exists(pathDir)) dir.create(pathDir)

  dirFiles <- dir(piamutils::getSystemFile("compareScenarios", package = "piamPlotComparison"), full.names = TRUE)

  if (!is.null(projectLibrary)) {
    dirFiles <- c(dirFiles, dir(piamutils::getSystemFile("compareScenarios", package = projectLibrary), full.names = TRUE))
  }

  rmdDirFiles <- grep(
    dirFiles,
    pattern = "cs_main\\.Rmd$",
    invert = TRUE, value = TRUE)

  file.copy(rmdDirFiles, pathDir, copy.mode = FALSE)

  # avoid potential problems with write permissions on the copied files
  Sys.chmod(list.files(pathDir, full.names = TRUE))

  ymlthis::use_rmarkdown(
    newYaml,
    path = file.path(pathDir, "cs_main.Rmd"),
    template = piamutils::getSystemFile(
      "compareScenarios/cs_main.Rmd",
      package = "piamPlotComparison"),
    include_yaml = FALSE)
}
