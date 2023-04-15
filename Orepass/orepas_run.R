source('fns.r')

# set sensitivity variables (make sure they are not overwritten by build code)
modelname = 'orepass'
scenario_desc='initial suite'
n_boggers <- 15
n_trucks <- 15
stope_access_const = 1
runduration =  24 * 3600
loglevel = 0
reps = 2



## Run the model reps times
sens_step=0
run_id <<- makevalidname(as.character(Sys.time()))
for(bogger in 5:6) {
  for (n_trucks in 5:6) {
    for (stope_access_const in 1:2) {
      sens_step=sens_step+1
      run_model(
        modelname = modelname,
        scenario_desc = scenario_desc,
        runduration = runduration ,
        reps = reps
      )
    }
  }
}


## collect results
seq_path <- paste0(scen_dir,'/',run_id)


all_run_vars <- combine_csv(path=seq_path,target='vars')
summary_stats <- scenario_summary(path=seq_path,target= 'attributes')




