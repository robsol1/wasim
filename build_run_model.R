
versioncode="Base_model"
thisseed <- as.integer(runif(1,1,10000))



## stockpiles

drawpoint_stocks_max = 9999999
stope_stocks_max =1000
hoist_stocks_max = 9999999

#unit sizes
cave_notional_size=1000
bogger_bucket_size =7
truck_load_size=70
hoist_nominal_size =1000

#unit rates and reliability
# Bogger

bogger_get_delay <- 'function() max(1, rnorm(1, 50, 5))'
bogger_haul_loaded_delay <- 'function() max(1, rnorm(1, 90, 9))'
bogger_dump_delay <- 'function() max(1, rnorm(1, 20, 2))'
bogger_haul_empty_delay='function() max(1, rnorm(1, 75, 7.5))'
bogger_mttr_str = 'function() max(1, rnorm(1, 3600, 360))'
bogger_mtbf_str = 'function() max(1, rexp(1, rate = 1/(24*3600)))'

# Truck

truck_get_delay <- 'function() max(1, rnorm(1, 180, 18))'
truck_haul_loaded_delay <- 'function() max(1, rnorm(1, 1200, 120))'
truck_dump_delay <- 'function() max(1, rnorm(1, 40, 4))'
truck_haul_empty_delay='function() max(1, rnorm(1, 1100, 110))'
truck_mttr_str = 'function() max(1, rnorm(1, 3600, 360))'
truck_mtbf_str = 'function() max(1, rexp(1, rate = 1/(24*3600)))'


pilenames=c('drawpoint','stope_stock','hoist_stock')
maxstocks=c(drawpoint_stocks_max,stope_stocks_max,hoist_stocks_max) 
initstocks=c(drawpoint_stocks_max,stope_stocks_max/2,0)
access_limit=c(3,stope_access_const,1) # number of activities that can operate on pile at any one time



mod_df <- init_model(model='model',level = loglevel,seed=thisseed)

# build bogger trajectory

mod_df <- build_stockpiles(mod_df,pilenames=pilenames,maxstocks=maxstocks,initstocks=initstocks,access_limit)
mod_df <- add_trajectory_to_model(
  modelname = 'newmodel',
  item='bogger',
  modeldf = mod_df,
  n_items = n_boggers,
  item_unit_capacity = bogger_bucket_size,
  item_mttr = bogger_mttr_str,
  item_mtbf = bogger_mtbf_str
)
mod_df <- add_lhd (
  modelname = 'model',
  mod_df = mod_df,
  item = 'bogger',
  activity = 'lhd',
  # Get Vars
  get_resources=n_boggers,
  get_delay=bogger_get_delay,
  get_stockpile='drawpoint',
  get_secondary_unit_name='cave',
  get_secondary_unit_capacity=cave_notional_size,
  # haul loaded Vars
  haul_loaded_resources=n_boggers,
  haul_loaded_delay=bogger_haul_loaded_delay,
  # put vars
  put_resources=n_boggers,
  put_delay=bogger_dump_delay,
  put_stockpile='stope_stock',
  put_secondary_unit_name='truck',
  put_secondary_unit_capacity=truck_load_size,
  
  # haul empty
  haul_empty_resources = n_boggers,
  haul_empty_delay = bogger_haul_empty_delay,
  trj_step = -1
)

mod_df <- create_close_trj('model',mod_df,'bogger')

## Build truck trajectory

mod_df <- add_trajectory_to_model(
  modelname = 'newmodel',
  item='truck',
  modeldf = mod_df,
  n_items = n_trucks,
  item_unit_capacity = truck_load_size,
  item_mttr =  truck_mttr_str,
  item_mtbf = truck_mtbf_str
)
mod_df <- add_lhd (
  modelname = 'model',
  mod_df = mod_df,
  item = 'truck',
  activity = 'truck_haul',
  # Get Vars
  get_resources=1, #only one loader at stockpile
  get_delay=truck_get_delay,
  get_stockpile='stope_stock',
  get_secondary_unit_name='bogger',
  get_secondary_unit_capacity=bogger_bucket_size,
  # haul loaded Vars
  haul_loaded_resources=n_trucks,
  haul_loaded_delay=truck_haul_loaded_delay,
  # put vars
  put_resources=1,# single tip point
  put_delay=truck_dump_delay,
  put_stockpile='hoist_stock',
  put_secondary_unit_name='hoist',
  put_secondary_unit_capacity=hoist_nominal_size,
  # haul empty
  haul_empty_resources = n_trucks,# all trucks can be hauling at the same time
  haul_empty_delay = truck_haul_empty_delay,
  trj_step = -1
)

mod_df <- create_close_trj('model',mod_df,'truck')



code <- join_code(mod_df)

save_text_to_file(code, '_nmod_df.r')
#file.show('_nmod_df.r')

# run the model
source('_nmod_df.r')


if(loglevel>0) {
  sink(file = './code.log.txt')
}
env <- env %>% run(runduration)
if(loglevel>0) {
  sink()
}
#get basic data 


