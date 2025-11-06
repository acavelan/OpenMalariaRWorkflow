# Clear global environment
rm(list = ls())

# load local files
source("pacman.R")
source("run.R")
source("extract.R")

# Load all required packages, installing them if required
pacman::p_load(char = c("foreach", "doParallel", "dplyr", "data.table"))

# sciCORE Slurm parameters:
sciCORE = list(
    use = TRUE,
    account = "chitnis",
    jobName = "OpenMalaria",
    qos = "30min",
    time = "00:30:00",
    cpus_per_task=16, # number of CPUs per job
    batch_size=16 # number of OM instances per job
    # number of job in array = N / batch_size
    # if batch_size = cpus_per_task then one OM instance = one CPU = faster
    # if batch_size > cpus_per_task then multiple OM instances per cpus = slower but less Slurm jobs
    # just leave it 16 / 16, 32 / 32, 64 / 64
    # if more than 500k jobs then 64 / 128 or 64 / 256 
)

# OpenMalaria
om = list(
    version = 48,
    # path = "/scicore/home/chitnis/GROUP/openMalaria-48"
    path = "/home/acavelan/git/fork/openMalaria-48.0"
)

# Scaffold xmls to use
scaffolds = list(
    "scaffolds/default.xml"
)

# run scenarios, extract the data, or both
do = list(
    run = TRUE, 
    extract = TRUE,
    example = TRUE
)

output = 'output' # name of the output folder

# Fixed parameters for all xmls
pop_size = 10000 # number of humans
start_year = 2000 # start of the monitoring period
end_year = 2020 # end of the monitoring period
burn_in = start_year - 50 # additional burn in time

# Varying parameters (combinatorial output)
seeds = 10
eirs = c(5, 10, 15, 20, 40, 60, 80, 100, 150, 200)
accesses = c(0.04, 0.20)

# Override for a quick test
pop_size = 2000
seeds = 3
eirs = c(5, 20)
accesses = c(0.04)

# Return a list of scenarios
create_scenarios <- function()
{
    index = 1
    scenarios = list()
    for(scaffold in scaffolds)
    {
        xml = readLines(scaffold)
        xml = gsub(pattern = "@version@", replace = om$version, x = xml)
        xml = gsub(pattern = "@pop_size@", replace = pop_size, x = xml)
        xml = gsub(pattern = "@burn_in@", replace = burn_in, x = xml)
        xml = gsub(pattern = "@start_year@", replace = start_year, x = xml)
        xml = gsub(pattern = "@end_year@", replace = end_year, x = xml)
        
        for(eir in eirs)
        {
          for(access in accesses)
          {
            for(seed in 1:seeds)
            {
              scenario = xml
              scenario = gsub(pattern = "@seed@", replace = seed, x = scenario)
              scenario = gsub(pattern = "@eir@", replace = eir, x = scenario)
              scenario = gsub(pattern = "@access@", replace = access, x = scenario)
              
              # use the right net snippet
              scenario = gsub(pattern = "@INTERVENTIONS@", replace = "", x = scenario)
              
              # write xml
              writeLines(scenario, con=paste0(output, "/xml/", index, ".xml"))
              
              # add the scenario to the list, only the 'index' field is mandatory, see example at the end
              scenario_metadata = list(scaffoldName = scaffold, access = access, eir = eir, seed = seed, index = index)
              scenarios = append(scenarios, list(scenario_metadata))
              
              index = index + 1
            }
          }
        }
    }
    
    return(scenarios)
}

if (do$run == TRUE)
{
    message("Cleaning Tree...")
    unlink(output, recursive=TRUE)
    dir.create(output)
    dir.create(paste0(output, "/xml"))
    dir.create(paste0(output, "/txt"))
    dir.create(paste0(output, "/fig"))
    dir.create(paste0(output, "/log"))
    
    message("Creating scenarios...")
    scenarios = create_scenarios()
    fwrite(rbindlist(scenarios), paste0(output, "/scenarios.csv"))
    
    message("Creating commands...")
    commands = create_commands(scenarios, output)
    
    message("Running scenarios...")
    # run_scicore(commands, output, om, sciCORE)
    run_local(commands, output, om)
}

if (do$extract == TRUE)
{
    message("Extracting results...")
    unlink(paste0(output, "/output.csv"))
    scenarios = fread(paste0(output, "/scenarios.csv"))
    
    start.time <- Sys.time()
    df = to_df(scenarios, output)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    message("Extract time: ", time.taken)
    
    if(nrow(df) == 0) {
        message("Error: extraction failed, output dataframe is empty")
        message("       output.csv not saved")
    }
    else {
        start.time <- Sys.time()
        fwrite(df, paste0(output, "/output.csv"))
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        message("Write time: ", time.taken)
    }
}

if (do$example == TRUE)
{
    scenarios = fread(paste0(output, "/scenarios.csv"))
    d = fread(paste0(output, "/output.csv"))
    
    # remove NA values
    d = d[complete.cases(d), ]
    
    # remove first survey
    d = d[!d$survey == 1,]
    
    # sum up surveys
    d = d %>% 
        group_by(index, measure, survey) %>% 
        summarise(value = sum(value), .groups = 'drop')
    
    # merge with the scenarios to have more metadata
    d = merge(d, scenarios, by = 'index')
    
    # summarised scenario_1
    scenario_1 = d[d$index == 1, ]
}

