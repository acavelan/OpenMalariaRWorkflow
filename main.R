# Clear global environment
rm(list = ls())

packages <- c("data.table", "xml2")

missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) install.packages(missing_packages, repos = "https://cloud.r-project.org")

invisible(lapply(packages, library, character.only = TRUE))

source("scenarios.R")
source("extract.R")
source("run.R")

slurm = list(
    account = "chitnis",
    partition = "scicore",
    job_name = "OpenMalaria",
    qos = "30min",
    time = "00:30:00",
    mem_per_cpu = "1G",
    cpus_per_task = 16,
    batch_size = 16
    # number of job in array = N / batch_size
    # if batch_size = cpus_per_task then one OM instance = one CPU = faster
    # if batch_size > cpus_per_task then multiple OM instances per cpus = slower but less Slurm jobs
    # just leave it 16 / 16, 32 / 32, 64 / 64
    # if more than 53.60 ms00k jobs then 64 / 128 or 64 / 256
)

# OpenMalaria
om = list(
    version = 49,
    path = normalizePath("../fork/openMalaria-49.0"),
    output_format = "bin.gz" # "bin", "bin.gz", "txt", or "txt.gz"
)

experiment_folder = "experiment"

scenarios <- s(
    "scaffolds/default.xml",
    "demography/@popSize", 1000,
    "monitoring/@startDate", "1950-01-01", # 50 years burnin (start date - 50 years)
    "monitoring/surveys/surveyTime", "2000-01-01", # start date
    "monitoring/surveys/surveyTime/@repeatStep", "5d", # survey every 5d (1y is also common)
    "monitoring/surveys/surveyTime/@repeatEnd", "2020-01-01", # end date
    "entomology/@scaledAnnualEIR", vary(eir = c(5, 20)),
    "healthSystem/DecisionTree5Day/pSeekOfficialCareUncomplicated1/@value", vary(access = 0.04),
    "healthSystem/DecisionTree5Day/pSeekOfficialCareUncomplicated2/@value", vary(access = 0.04),
    "model/computationParameters/@iseed", vary(seed = 1:3)
)

# comment out to avoid rerunning the scenarios and extracting the data
#scenarios <- run(scenarios, experiment_folder, om) #, slurm)
#df <- extract(scenarios, experiment_folder)

# Load saved results if starting from an existing experiment folder
scenarios <- fread(file.path(experiment_folder, "scenarios.csv"))
df <- fread(file.path(experiment_folder, "output.csv"))

d = df[complete.cases(df), ] # remove NA values
d = d[!d$survey == 1,] # remove first survey
d = d[, .(value = sum(value)), by = .(index, measure, survey)] # aggregate age-groups

# merge with the scenarios to have more metadata
d = merge(d, scenarios, by = 'index')
setorder(d, eir, seed, survey, measure)

nPatent = d[measure == 3]
nUncomp = d[measure == 14]
nHost = d[measure == 0]
inputEIR = d[measure == 35]
simulatedEIR = d[measure == 36]

prevalence = copy(nPatent)
prevalence[, value := nPatent$value / nHost$value]

incidence = copy(nUncomp)
incidence[, value := nUncomp$value / nHost$value]

plot_lines = function(d, ylab) {
    if (!is.list(d) || is.data.table(d)) d = list(d)
    d = lapply(d, function(x) x[, .(value = mean(value)), by = .(eir, survey)])
    names(d)[names(d) == ""] = ylab
    labels = unique(d[[1]]$eir)
    ylim = range(unlist(lapply(d, function(x) x$value)), na.rm = TRUE)
    plot(NA, xlim = range(d[[1]]$survey), ylim = ylim, xlab = "Survey", ylab = ylab)
    for (j in seq_along(d)) {
        for (i in seq_along(labels)) {
            x = d[[j]][eir == labels[i]]
            lines(x$survey, x$value, col = i, lty = j, lwd = 2)
        }
    }
    legend("topright", legend = paste(rep(names(d), each = length(labels)), "EIR", labels),
           col = rep(seq_along(labels), length(d)), lty = rep(seq_along(d), each = length(labels)),
           bty = "n", cex = 2)
}

old_par = par(no.readonly = TRUE)
par(mfrow = c(3, 1), cex.lab = 2, cex.axis = 2, mar = c(4, 4.5, 1, 1))

plot_lines(prevalence, "nPatent / nHost") # plot prevalence
plot_lines(incidence, "nUncomp / nHost") # plot incidence
plot_lines(list(input = inputEIR, simulated = simulatedEIR), "EIR") # plot EIR

par(old_par)
