prepare <- function(experiment_folder, om)
{
    file.copy(file.path(om$path, "densities.csv"), experiment_folder, overwrite=TRUE)
    file.copy(file.path(om$path, paste0("scenario_", om$version, ".xsd")), experiment_folder, overwrite=TRUE)
}

create_commands <- function(scenarios, experiment_folder, om)
{
    commands = list()
    output_format = sub("\\.gz$", "", om$output_format)
    compress_output = endsWith(om$output_format, ".gz")

    for(row in seq_len(nrow(scenarios)))
    {
        index = scenarios$index[row]
        outputfile = scenarios$outputFile[row]
        exe <- file.path(om$path, if (.Platform$OS.type == "windows") "openMalaria.exe" else "openMalaria")
        args = c(
            shQuote(exe),
            "-s", shQuote(file.path("xml", paste0(index, ".xml"))),
            "--output-format", output_format,
            if (compress_output) "--compress-output",
            "--output", shQuote(outputfile)
        )
        command = paste(args, collapse = " ")
        commands = append(commands, command)
    }
    writeLines(as.character(commands), file.path(experiment_folder, "commands.txt"))
    
    commands
}

run <- function(scenarios, experiment_folder, om, slurm = NULL, overwrite = FALSE)
{
    if (!om$output_format %in% c("bin", "bin.gz", "txt", "txt.gz")) stop("Unknown output_format: ", om$output_format)

    xml_files = file.path(experiment_folder, "xml", paste0(scenarios$index, ".xml"))
    if (!all(file.exists(xml_files))) stop("Scenario XML files are missing. Run write_scenarios() first.")

    folders = file.path(experiment_folder, c("out", "log"))
    existing = folders[file.exists(folders)]
    if (length(existing) && !overwrite) {
        stop("Output folders already exist: ", paste(existing, collapse = ", "), ". Use overwrite = TRUE to replace them.")
    }
    if (overwrite && unlink(folders, recursive=TRUE)) stop("Could not remove output folders")
    if (!all(vapply(folders, dir.create, logical(1), recursive=TRUE))) stop("Could not create output folders")

    scenarios$outputFile = file.path("out", paste0(scenarios$index, ".", om$output_format))
    fwrite(scenarios, file.path(experiment_folder, "scenarios.csv"))
    
    message("Creating commands...")
    commands = create_commands(scenarios, experiment_folder, om)
    
    message("Running scenarios...")
    if (is.null(slurm)) run_local(commands, experiment_folder, om) else run_slurm(commands, experiment_folder, om, slurm)
}

extract <- function(scenarios, experiment_folder, overwrite = FALSE)
{
    message("Extracting results...")
    output_file = file.path(experiment_folder, "output.csv")
    if (file.exists(output_file) && !overwrite) stop("Output file already exists: ", output_file, ". Use overwrite = TRUE to replace it.")
    
    if (!"outputFile" %in% names(scenarios)) {
        scenarios = fread(file.path(experiment_folder, "scenarios.csv"))
    }
    
    start.time <- Sys.time()
    df = to_df(scenarios, experiment_folder)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    message("Extract time: ", time.taken)
    
    if(nrow(df) == 0) {
        message("Error: extraction failed, output dataframe is empty")
        message("       output.csv not saved")
    }
    else {
        start.time <- Sys.time()
        fwrite(df, output_file)
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        message("Write time: ", time.taken)
    }
    
    invisible(df)
}

run_slurm <- function(commands, experiment_folder, om, slurm)
{
    prepare(experiment_folder, om)
    n <- ceiling(length(commands) / slurm$batch_size)
    
    script <- readLines("job.sh")
    replacements <- c(
        N = n,
        account = slurm$account,
        partition = slurm$partition,
        jobname = slurm$job_name,
        qos = slurm$qos,
        time = slurm$time,
        MEM_PER_CPU = slurm$mem_per_cpu,
        CPUS_PER_TASK = slurm$cpus_per_task,
        BATCH_SIZE = slurm$batch_size
    )
    for (name in names(replacements)) {
        script <- gsub(paste0("@", name, "@"), replacements[[name]], script, fixed = TRUE)
    }
    writeLines(script, file.path(experiment_folder, "start_array_job.sh"))
      
    message("Submitted ", n, " jobs")

    oldwd <- setwd(experiment_folder); on.exit(setwd(oldwd), add = TRUE)
    if (system2("sbatch", c("--wait", "start_array_job.sh")) != 0) stop("Slurm job failed")
}

run_local <- function(commands, experiment_folder, om)
{
  prepare(experiment_folder, om)
  
  n <- length(commands)
  message("Running ", n, " scenarios locally")
  
  results <- lapply(seq_along(commands), function(i) {
    cmd <- commands[[i]]
    oldwd <- setwd(experiment_folder); on.exit(setwd(oldwd), add = TRUE)
    
    logfile <- file.path("log", paste0("job_", i, ".log"))
    
    shell <- if (.Platform$OS.type == "windows") "cmd" else "bash"
    flag  <- if (.Platform$OS.type == "windows") "/c"  else "-c"
    
    # Must quote entire command string for redirection to work
    cmd_full <- paste(cmd, ">", shQuote(logfile), "2>&1")
    exitcode <- system2(shell, paste(flag, shQuote(cmd_full)), stdout = NULL, stderr = NULL, wait = TRUE)
    
    if (exitcode != 0) {
      msg = paste0("Error in task ", i, ": ", cmd, "\n  See log: ", logfile)
      msg
    } else NULL
  })

  cat(paste(unlist(results), collapse = "\n"))
}
