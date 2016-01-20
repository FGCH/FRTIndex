#=
Run FRT Stan Model from Julia
=#

ENV["CMDSTAN_HOME"] = "/Users/christophergandrud/cmdstan"
ENV["JULIA_SVG_BROWSER"] = "Google's Chrome.app"

using DataFrames
using Mamba
using Stan

# Set wd
cd("$(homedir())/git_repositories/FRTIndex")

# Load data set
MoltenReady = readtable("misc/stan_molten_ready.csv")

# Load stan model
f = open("source/FRT.stan")
const model_code = readall(f)
close(f)

# Set up Stan model
monitor = ["delta", "alpha", "beta", "gamma"]
stan_model = Stanmodel(name = "frt_model",
                       model = model_code,
                       monitors = monitor,
                       adapt = 50,
                       update = 50,
                       );
stan_model |> display

# Find variables
C = maximum(MoltenReady[:countrynum])
T = maximum(MoltenReady[:yearnum])
K = maximum(MoltenReady[:variable])
N = nrow(MoltenReady)

# Enter data
const frt_data = [
  Dict(
    "C" => C,
    "T" => T,
    "K" => K,
    "N" => N,
    "cc" => MoltenReady[:countrynum],
    "tt" => MoltenReady[:yearnum],
    "kk" => MoltenReady[:variable],
    "y" => MoltenReady[:value]
  )
]

# Run model
save_dir = string("frt_out_", Dates.today())

fit = stan(stan_model, frt_data, ProjDir = save_dir, CmdStanDir = CMDSTAN_HOME)
