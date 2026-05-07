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
    path = "/scicore/home/scicore/cavelan/git/fork/openMalaria-48.0"
)

# Scaffold xmls to use
scaffolds = list(
    "scaffolds/limits_eir_gambiae_arabiensis.xml"
)

# run scenarios, extract the data, or both
do = list(
    run = TRUE, 
    extract = TRUE,
    example = TRUE
)

output = 'output' # name of the output folder

# Fixed parameters for all xmls
start_year = 2000 # start of the monitoring period
end_year = 2001 # end of the monitoring period
burn_in = start_year - 50 # additional burn in time

# Scenario grid for the first limits-of-the-model figure:
# x-axis = EIR 0.1..5.0 by 0.1
# y-axis = shared gambiae/arabiensis a1 0.5..10.0 by 0.5
pop_size = 10000
seeds = 1
eirs = seq(0.1, 5.0, by = 0.1)
a1s = seq(0.5, 10.0, by = 0.5)

# Override for testing. Comment out this block for the full figure grid.
# pop_size = 200
# eirs = c(seq(0.1, 4.1, by = 1.0), 5.0)
# a1s = c(seq(0.5, 9.5, by = 1.0), 10.0)

nv0_solver = "false" # use legacy method if false

format_value <- function(x)
{
    trimws(format(x, trim = TRUE, scientific = FALSE))
}

replace_tokens <- function(template, replacements)
{
    rendered = template
    for (name in names(replacements))
    {
        rendered = gsub(
            pattern = paste0("@", name, "@"),
            replace = format_value(replacements[[name]]),
            x = rendered,
            fixed = TRUE
        )
    }

    rendered
}

# Return a list of scenarios
create_scenarios <- function()
{
    index = 1
    scenarios = list()
    for(scaffold in scaffolds)
    {
        xml = readLines(scaffold)
        xml = replace_tokens(xml, list(
            version = om$version,
            pop_size = pop_size,
            burn_in = burn_in,
            start_year = start_year,
            end_year = end_year,
            nv0_solver = nv0_solver
        ))
        
        for(eir in eirs)
        {
          for(a1 in a1s)
          {
            for(seed in seq_len(seeds))
            {
              scenario = replace_tokens(xml, list(
                seed = seed,
                eir = eir,
                a1 = a1
              ))

              # write xml
              writeLines(scenario, con=paste0(output, "/xml/", index, ".xml"))

              # add the scenario to the list, only the 'index' field is mandatory, see example at the end
              scenario_metadata = list(
                scaffoldName = scaffold,
                eir = eir,
                a1 = a1,
                seed = seed,
                index = index
              )
              scenarios = append(scenarios, list(scenario_metadata))

              index = index + 1
            }
          }
        }
    }
    
    return(scenarios)
}

plot_eir_a1_heatmap <- function(output)
{
    scenarios = fread(paste0(output, "/scenarios.csv"))
    d = fread(paste0(output, "/output.csv"))
    
    d = d[measure %in% c(35, 36) & ageGroup == 0]
    d = d[complete.cases(d), ]
    d = d[d$survey != 1, ]
    
    if (nrow(d) == 0) {
        stop("No EIR output is available to plot")
    }
    
    d = dcast(d, index + survey ~ measure, value.var = "value")
    
    if (!all(c("35", "36") %in% names(d))) {
        stop("Measures 35 and 36 were not both found in output.csv")
    }
    
    setnames(d, c("35", "36"), c("input_eir", "simulated_eir"))
    
    surveys_per_year = 72
    d[, year_index := ((survey - 2) %/% surveys_per_year) + 1]
    
    annual_fit = d[, .(
        normalized_l2_1y = {
            denominator = sum(input_eir)
            if (denominator == 0) {
                NA_real_
            } else {
                sqrt(sum(((simulated_eir / denominator) - (input_eir / denominator))^2))
            }
        },
        n_surveys = .N
    ), by = .(index, year_index)]
    
    fit = annual_fit[, .(
        mean_1y_l2 = mean(normalized_l2_1y, na.rm = TRUE),
        n_years = .N,
        n_surveys = sum(n_surveys)
    ), by = index]
    fit[!is.finite(mean_1y_l2), mean_1y_l2 := NA_real_]
    
    heatmap = merge(scenarios, fit, by = "index", all.x = TRUE)
    fwrite(heatmap, paste0(output, "/fig/heatmap_eir_a1.csv"))
    
    max_value = 1
    
    breaks = seq(0, max_value, length.out = 100)
    heatmap[, plot_value := ifelse(is.na(mean_1y_l2), -0.01, pmin(mean_1y_l2, max_value))]
    
    p = lattice::levelplot(
        plot_value ~ eir * a1,
        data = heatmap,
        at = c(-0.01, breaks),
        col.regions = c(
            "grey85",
            grDevices::hcl.colors(length(breaks) - 1, palette = "YlOrRd", rev = TRUE)
        ),
        xlab = "Annual EIR",
        ylab = "Shared a1 Fourier Coefficient",
        main = "Mean 1-Year Normalized L2 Error Relative to Input EIR",
        sub = "Grey tiles indicate scenarios that failed before producing EIR output",
        colorkey = list(
            space = "right",
            at = pretty(c(0, max_value)),
            labels = list(
                at = pretty(c(0, max_value)),
                labels = pretty(c(0, max_value))
            )
        )
    )
    
    png(
        filename = paste0(output, "/fig/heatmap_eir_a1.png"),
        width = 2400,
        height = 1500,
        res = 300
    )
    print(p)
    dev.off()
    
    heatmap
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
    run_scicore(commands, output, om, sciCORE)
    #run_local(commands, output, om)
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
    heatmap = plot_eir_a1_heatmap(output)
}
