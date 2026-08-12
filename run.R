prepare <- function(experiment_folder, om)
{
    file.copy(file.path(om$path, "densities.csv"), experiment_folder)
    file.copy(file.path(om$path, paste0("scenario_", om$version, ".xsd")), experiment_folder)
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

run <- function(scenarios, experiment_folder, om, slurm = NULL)
{
    message("Cleaning Tree...")
    unlink(experiment_folder, recursive=TRUE)
    dir.create(file.path(experiment_folder, "xml"), recursive=TRUE)
    dir.create(file.path(experiment_folder, "out"))
    dir.create(file.path(experiment_folder, "log"))
    
    message("Creating scenarios...")
    scenarios = write_scenarios(scenarios, experiment_folder, om)
    fwrite(scenarios, file.path(experiment_folder, "scenarios.csv"))
    
    message("Creating commands...")
    commands = create_commands(scenarios, experiment_folder, om)
    
    message("Running scenarios...")
    if (is.null(slurm)) run_local(commands, experiment_folder, om) else run_slurm(commands, experiment_folder, om, slurm)
    
    invisible(scenarios)
}

extract <- function(scenarios, experiment_folder)
{
    message("Extracting results...")
    unlink(file.path(experiment_folder, "output.csv"))
    
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
        fwrite(df, file.path(experiment_folder, "output.csv"))
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
