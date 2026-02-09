#' @title Build lis files
#' @description Build .lis files from DayCent .bin outputs (eq, base, exp), read, combine, save, cleanup
#'
#' @param site character. Site name (e.g., "wooster")
#' @param run character. Experiment/run label used in schedule/run call (e.g., "cc_nt")
#' @param dc_L100 character. Path to DDlist100 executable
#' @param lis_vars_path character. Path to outvars list file (e.g., "few_outvars.txt")
#' @param out_dir character. Output folder (default "outputs")
#' @param keep_combined logical. Write combined tables to out_dir (default TRUE)
#'
#' @return list with: all (df), base_exp (df), exp (df), logs (list)
build_lis_from_bin <- function(site,
                               run,
                               dc_L100,
                               lis_vars = c("som1c(2)", "som2c(2)", "som3c",
                                            "somsc", "agcprd", "cgrain"),
                               out_dir = "outputs",
                               keep_combined = TRUE) {


  if (!file.exists(dc_L100)) stop("dc_L100 (DDlist100) not found: ", dc_L100)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  TEMPfile = "TEMPVARS.txt"
  writeLines(text = lis_vars, con = TEMPfile)

  # names used by DDlist100
  eqRun   <- paste0(site, "_eq")
  baseRun <- paste0(site, "_base")
  siteRun <- paste0(site, "_", run)

  lis_file_eq   <- paste0(eqRun, ".lis")
  lis_file_base <- paste0(baseRun, ".lis")
  lis_file_exp  <- paste0(siteRun, ".lis")

  # remove any old temporary lis files
  unlink(c(lis_file_eq, lis_file_base, lis_file_exp))

  # build args + run DDlist100
  lis_args_eq   <- paste("eq",   eqRun,   TEMPfile)
  lis_args_base <- paste("base", baseRun, TEMPfile)
  lis_args_exp  <- paste(run,    siteRun, TEMPfile)

  run_list100 <- function(args) {
    system2(command = dc_L100, args = args, wait = TRUE, stdout = TRUE, stderr = TRUE)
  }

  logs <- list(
    eq   = run_list100(lis_args_eq),
    base = run_list100(lis_args_base),
    exp  = run_list100(lis_args_exp)
  )

  # sanity: ensure files exist
  missing <- c(lis_file_eq, lis_file_base, lis_file_exp)[!file.exists(c(lis_file_eq, lis_file_base, lis_file_exp))]
  if (length(missing) > 0) {
    stop("DDlist100 did not produce expected .lis file(s): ", paste(missing, collapse = ", "),
         "\nCheck run names (eq/base/run), working directory, and bin outputs.")
  }

  # read variable names (1 per line)
  var_names <- read.table(TEMPfile, stringsAsFactors = FALSE)[[1]]
  col_names <- c("time", var_names)

  read_lis <- function(path) {
    df <- read.table(file = path, skip = 3)
    names(df) <- col_names
    df
  }

  lis_df_eq   <- read_lis(lis_file_eq)
  lis_df_base <- read_lis(lis_file_base)
  lis_df_exp  <- read_lis(lis_file_exp)

  eqYear = max(lis_df_eq$time)
  baseYear = min(lis_df_base$time)
  expYear = min(lis_df_exp$time)

  # adjust eq time
  eqDeltaYear = baseYear-eqYear-1
  lis_df_eq$time = lis_df_eq$time + eqDeltaYear
  # combine + label run periods + adjust eq time + compute dSOC (gm2 per year)
  lis_df <- dplyr::bind_rows(lis_df_eq, lis_df_base, lis_df_exp) %>%
    dplyr::mutate(
      run_period = dplyr::case_when(
        time < baseYear ~ "eq",
        time < expYear  ~ "base",
        TRUE            ~ "experiment"
      ))

  # dSOC uses somsc if present
  if ("somsc" %in% names(lis_df)) {
    lis_df <- lis_df %>%
      dplyr::arrange(time) %>%
      dplyr::mutate(dSOC_gm2 = c(0, diff(somsc) / diff(time)))
  } else {
    lis_df$dSOC_gm2 <- NA_real_
    warning("Column 'somsc' not found in lis vars; dSOC_gm2 set to NA.")
  }

  lis_df_base_exp <- lis_df %>% dplyr::filter(run_period != "eq")
  lis_df_exp_only <- lis_df %>% dplyr::filter(run_period == "experiment")

  # write combined tables + move exp lis for reference
  if (keep_combined) {
    # nice file stem
    stem <- paste0(site, "_", run)

    utils::write.table(lis_df,
                       file = file.path(out_dir, run, paste0(stem, "_eqBaseExp.lis")),
                       row.names = FALSE)
    utils::write.table(lis_df_base_exp,
                       file = file.path(out_dir, run, paste0(stem, "_baseExp.lis")),
                       row.names = FALSE)

    file.copy(lis_file_exp, file.path(out_dir, run ,lis_file_exp), overwrite = TRUE)
  }

  # cleanup temporary lis files
  unlink(c(lis_file_eq, lis_file_base, lis_file_exp, TEMPfile))

  message("Built lis outputs for: ", siteRun, " (saved tables in ", out_dir, "; removed temporary .lis and .txt files).")

  list(
    all = lis_df,
    base_exp = lis_df_base_exp,
    exp = lis_df_exp_only,
    logs = logs
  )
}
