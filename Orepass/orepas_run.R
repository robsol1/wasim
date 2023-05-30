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
verbose=FALSE


## Run the model reps times
rm(summary_result)
sens_step=0
run_id <<- makevalidname(as.character(Sys.time()))
for(n_boggers in 5:6) {
  #for (n_trucks in 5:6) {
   # for (stope_access_const in 1:2) {
      sens_step=sens_step+1
      run_model(
        modelname = modelname,
        scenario_desc = scenario_desc,
        runduration = runduration ,
        reps = reps,
        verbose=verbose
      )
   # }
  #}
}

## collect results
seq_path <- paste0(scen_dir,'/',run_id)
summary_result[is.na(summary_result)] = 0
write.csv(summary_result,paste0(seq_path,'/_sumresults.csv'))

all_run_vars <- combine_csv(path=seq_path,target='vars')
write.csv(all_run_vars,paste0(seq_path,'/_all_run_vars.csv'))




