run_scicore <- function(scenarios, experiment, om, sciCORE)
{
    commands = list()
    for(scenario in scenarios)
    {
        index = scenario$index
        outputfile = paste0("txt/", scenario$index, ".txt")
        command = paste0(om$path, "/", "openMalaria -s xml/", index, ".xml --output ", outputfile)
        commands = append(commands, command)
    }
    writeLines(as.character(commands), paste0(experiment, "/commands.txt"))
    
    n <- ceiling(length(scenarios) / sciCORE$batch_size)
    
    script = readLines("job.sh")
    script = gsub(pattern = "@N@", replace = n, x = script)
    script = gsub(pattern = "@account@", replace = sciCORE$account, x = script)
    script = gsub(pattern = "@jobname@", replace = sciCORE$jobName, x = script)
    script = gsub(pattern = "@qos@", replace = sciCORE$qos, x = script)
    script = gsub(pattern = "@time@", replace = sciCORE$time, x = script)
    script = gsub(pattern = "@CPUS_PER_TASK@", replace = sciCORE$cpus_per_task, x = script)
    script = gsub(pattern = "@BATCH_SIZE@", replace = sciCORE$batch_size, x = script)
    writeLines(script, con=paste0(experiment, "/start_array_job.sh"))
      
    message("Submitted ", n, " jobs")
    system(paste0("cd ", experiment, " && sbatch --wait start_array_job.sh"))
}

run_local <- function(scenarios, experiment, om)
{
    n = length(scenarios)
    n_cores <- as.numeric(system("nproc", intern = TRUE))
    message("Running ", n, " scenarios on ", n_cores, " cores")
    
    registerDoParallel(n_cores)
    cluster = makeCluster(n_cores, type="FORK")  
    registerDoParallel(cluster)  
    
    foreach(i=1:n, .combine = 'c') %dopar% {
        scenario = scenarios[[i]]
        index = scenario$index
        
        outputfile = paste0("txt/", scenario$index, ".txt")
        command = paste0(om$path, "/", "openMalaria -s xml/", index, ".xml --output ", outputfile)
        full_command = paste0("cd ", experiment, " && ", command)
        system(full_command)#, ignore.stdout = TRUE, ignore.stderr = TRUE)
        NULL
    }
    
    stopCluster(cluster)
}

run_scenarios <- function(scenarios, experiment, om, sciCORE)
{
    file.copy(paste0(om$path, "/densities.csv"), paste0(experiment, "/"))
    file.copy(paste0(om$path, "/scenario_", om$version, ".xsd"), paste0(experiment, "/"))
    
    if(sciCORE$use == TRUE) run_scicore(scenarios, experiment, om, sciCORE)
    else run_local(scenarios, experiment, om)
}
