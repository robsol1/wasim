source("fns.R")

modelname="orepasswithsharedlhd"
loglevel=1

truck_unit_capacity=70
n_trucks=1
truck_mttr=1
truck_mtbf=1
truck_loading_delay=5

LHD_unit_capacity=20
n_LHD=1
LHD_mttr=1
LHD_mtbf=1

## stockpiles

drawpoint_stocks_max = 9999999
stope_stocks_max =1000
hoist_stocks_max = 9999999
stope_access_const = 1
pilenames=c('drawpoint','stope_stock','hoist_stock')
maxstocks=c(drawpoint_stocks_max,stope_stocks_max,hoist_stocks_max) 
initstocks=c(drawpoint_stocks_max,stope_stocks_max/2,0)
access_limit=c(3,stope_access_const,1) # number of activities that can operate on pile at any one time
mod_df <- init_model(model=modelname,level = loglevel)

mod_df <- build_stockpiles(mod_df,pilenames=pilenames,maxstocks=maxstocks,initstocks=initstocks,access_limit)
mod_df <- add_trajectory_to_model(modelname=modelname,
                                  modeldf = mod_df,
                                  item = 'truck',
                                  actvity ='add_truck_trj',
                                  n_items = n_trucks,
                                  item_unit_capacity = truck_unit_capacity,
                                  item_mttr=truck_mttr,
                                  item_mtbf=truck_mtbf)

mod_df <- add_target_seek_load_by_item(modelname=modelname,
                                  mod_df = mod_df,
                                  item = 'truck',
                                  activity = 'loading',
                                  trj_step = -1,
                                  number_of_resources = '1',
                                  item_activity_delay = 'function() max(1, rnorm(1, 5, .2))',
                                  stockpile='stope_stock',
                                  secondary_unit_name='LHD',
                                  next_trj_step = -1)
mod_df <- create_close_trj(modelname,mod_df,'truck')






mod_df <- add_trajectory_to_model(modelname=modelname,
                                  modeldf = mod_df,
                                  item = 'LHD',
                                  actvity ='add_LHD_trj',
                                  n_items = n_LHD,
                                  item_unit_capacity = LHD_unit_capacity,
                                  item_mttr=LHD_mttr,
                                  item_mtbf=LHD_mtbf)

mod_df <- add_loader_loads_item(
  modelname=modelname,
  mod_df= mod_df,
  item='LHD',
  activity = 'loading',
  trj_step = -1,
  number_of_resources = n_LHD,
  item_activity_delay = 'function() max(1, rnorm(1, 5, .2))',
  stockpile='stope_stock',
  secondary_unit_name='truck',
  next_trj_step = -1
)



mod_df <- create_close_trj(modelname,mod_df,'LHD')
code <- join_code(mod_df)

save_text_to_file(code,"temp2.R")
source("temp2.R")

env <- env %>% run(100)
arrivals <- get_mon_arrivals(env)
resources <- get_mon_resources(env)
attributes <- get_mon_attributes(env)
plot(resources, metric = "utilization")
plot(arrivals, metric = "flow_time")
pltdata <- attributes %>% 
  filter(key=="truck_id")

pltdata %>% ggplot(aes(x=time,y=value)) +
  geom_step()+
  facet_wrap(facets=vars(name))


get_palette <- scales::brewer_pal(type = "qual", palette = 1)
plot(item_trj, fill = get_palette,height=3000)

