prepare <- function(output, om)
{
    file.copy(paste0(om$path, "/densities.csv"), paste0(output, "/"))
    file.copy(paste0(om$path, "/scenario_", om$version, ".xsd"), paste0(output, "/"))
}

create_commands <- function(scenarios, output)
{
    commands = list()
    for(scenario in scenarios)
    {
        index = scenario$index
        outputfile = paste0("txt/", scenario$index, ".txt")
        command = paste0(om$path, "/", "openMalaria -s xml/", index, ".xml --output ", outputfile)
        commands = append(commands, command)
    }
    writeLines(as.character(commands), paste0(output, "/commands.txt"))
    
    commands
}

run_scicore <- function(commands, output, om, sciCORE)
{
    prepare(output, om)
  
    n <- ceiling(length(commands) / sciCORE$batch_size)
    
    script = readLines("job.sh")
    script = gsub(pattern = "@N@", replace = n, x = script)
    script = gsub(pattern = "@account@", replace = sciCORE$account, x = script)
    script = gsub(pattern = "@jobname@", replace = sciCORE$jobName, x = script)
    script = gsub(pattern = "@qos@", replace = sciCORE$qos, x = script)
    script = gsub(pattern = "@time@", replace = sciCORE$time, x = script)
    script = gsub(pattern = "@CPUS_PER_TASK@", replace = sciCORE$cpus_per_task, x = script)
    script = gsub(pattern = "@BATCH_SIZE@", replace = sciCORE$batch_size, x = script)
    writeLines(script, con=paste0(output, "/start_array_job.sh"))
      
    message("Submitted ", n, " jobs")
    system(paste0("cd ", output, " && sbatch --wait start_array_job.sh"))
}

run_local <- function(commands, output, om)
{
  prepare(output, om)
  
  n <- length(commands)
  n_cores <- parallel::detectCores()
  
  message("Running ", n, " scenarios on ", n_cores, " cores")
  
  cluster <- makeCluster(n_cores)
  registerDoParallel(cluster)
  
  results <- foreach(i = seq_along(commands), .combine = 'c') %dopar% {
    cmd <- commands[[i]]
    oldwd <- setwd(output); on.exit(setwd(oldwd), add = TRUE)
    
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
  }
  
  stopCluster(cluster)
  cat(paste(results, collapse = "\n"))
}
