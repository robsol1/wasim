source("fns.R")

modelname="orepasswithsharedlhd"
loglevel=1

#unit sizes
cave_notional_size=1000
LHD_unit_capacity =21
truck_load_size=63
hoist_nominal_size =1000

truck_unit_capacity=70
n_trucks=1
truck_mttr=1
truck_mtbf=10000
truck_loading_delay='function() trimrandom(70)'
truck_travel_loaded_delay='function() trimrandom(600)'
truck_dump_delay='function() trimrandom(20)'
truck_travel_empty_delay='function() trimrandom(500)'

n_LHD=1
LHD_mttr=1
LHD_mtbf=100000
LHD_loading_delay='function() trimrandom(20)'
LHD_travel_loaded_delay='function() trimrandom(70)'
LHD_dump_delay='function() trimrandom(20)'
LHD_travel_empty_delay='function() trimrandom(70)'
## stockpiles

drawpoint_stocks_max = 9999999
stope_stocks_max =1000
hoist_stocks_max = 9999999
stope_access_const = 1
pilenames=c('drawpoint','stope_stock','hoist_stock')
maxstocks=c(drawpoint_stocks_max,stope_stocks_max,hoist_stocks_max) 
initstocks=c(drawpoint_stocks_max,stope_stocks_max/2,0)
access_limit=c(3,stope_access_const,1) # number of activities that can operate on pile at any one time


###
### build Code
mod_df <- init_model(model=modelname,level = loglevel)

## Initialise model

mod_df <-
  build_stockpiles(
    modeldf = mod_df,
    pilenames = pilenames,
    maxstocks = maxstocks,
    initstocks = initstocks,
    access_limit=access_limit
  )


## add truck trajectory to model
item='truck'
mod_df <- add_trajectory_to_model(modelname=modelname,
                                  modeldf = mod_df,
                                  item = item,
                                  actvity ='add_truck_trj',
                                  n_items = n_trucks,
                                  item_unit_capacity = truck_unit_capacity,
                                  item_mttr=truck_mttr,
                                  item_mtbf=truck_mtbf)

mod_df <- add_target_seek_load_by_item(modelname=modelname,
                                  mod_df = mod_df,
                                  item = item,
                                  activity = 'loading',
                                  trj_step = -1,
                                  number_of_resources = 1,
                                  item_activity_delay = truck_loading_delay,
                                  stockpile='stope_stock',
                                  secondary_unit_name='LHD',
                                  next_trj_step = -1)

mod_df <- add_activity_delay(modelname=modelname,
                             mod_df=mod_df,
                             item=item,
                             activity = 'travel_loaded',
                             trj_step = -1,
                             number_of_resources = n_trucks,
                             item_activity_delay = truck_travel_loaded_delay,
                             next_trj_step=-1)

mod_df <-  add_delay_with_single_stock_movement(modelname=modelname,
                                                 mod_df=mod_df,
                                                 item=item,
                                                 activity = 'dump_to_stock',
                                                 action='put',
                                                 trj_step = -1,
                                                 number_of_resources = 1,
                                                 item_activity_delay = truck_dump_delay,
                                                 stockpile='hoist_stock',
                                                 secondary_unit_name='hoist',
                                                 secondary_unit_capacity=hoist_nominal_size,
                                                 next_trj_step = -1)
mod_df <- add_activity_delay(modelname=modelname,
                             mod_df=mod_df,
                             item=item,
                             activity = 'travel_empty',
                             trj_step = -1,
                             number_of_resources = n_trucks,
                             item_activity_delay = truck_travel_empty_delay,
                             next_trj_step=-1)


mod_df <- create_close_trj(modelname=modelname,
                           modeldf = mod_df,
                           item)



item <- "LHD"


mod_df <- add_trajectory_to_model(modelname=modelname,
                                  modeldf = mod_df,
                                  item = item,
                                  actvity ='add_LHD_trj',
                                  n_items = n_LHD,
                                  item_unit_capacity = LHD_unit_capacity,
                                  item_mttr=LHD_mttr,
                                  item_mtbf=LHD_mtbf)
mod_df <-  add_delay_with_single_stock_movement(modelname=modelname,
                                                mod_df=mod_df,
                                                item=item,
                                                activity = 'load_from_drawpoint',
                                                action='put',
                                                trj_step = -1,
                                                number_of_resources = 1,
                                                item_activity_delay = LHD_loading_delay,
                                                stockpile='hoist_stock',
                                                secondary_unit_name='hoist',
                                                secondary_unit_capacity=hoist_nominal_size,
                                                next_trj_step = -1)
mod_df <- add_activity_delay(modelname=modelname,
                             mod_df=mod_df,
                             item=item,
                             activity = 'travel_loaded',
                             trj_step = -1,
                             number_of_resources = n_LHD,
                             item_activity_delay = LHD_travel_loaded_delay,
                             next_trj_step=-1)
mod_df <-  add_delay_with_single_stock_movement(modelname=modelname,
                                                mod_df=mod_df,
                                                item=item,
                                                activity = 'dump_2_Stopestock',
                                                action='put',
                                                trj_step = -1,
                                                number_of_resources = 1,
                                                item_activity_delay = LHD_dump_delay,
                                                stockpile='stope_stock',
                                                secondary_unit_name='truck',
                                                secondary_unit_capacity=truck_load_size,
                                                next_trj_step = -1)
mod_df <- add_loader_loads_item(modelname=modelname,
                                  mod_df= mod_df,
                                  item=item,
                                  activity = 'loading',
                                  trj_step = -1,
                                  number_of_resources = n_LHD,
                                  item_activity_delay = truck_loading_delay,
                                  stockpile='stope_stock',
                                  secondary_unit_name='truck',
                                  next_trj_step = -1
                                )
mod_df <- add_activity_delay(modelname=modelname,
                             mod_df=mod_df,
                             item=item,
                             activity = 'travel_empty',
                             trj_step = -1,
                             number_of_resources = n_LHD,
                             item_activity_delay = LHD_travel_empty_delay,
                             next_trj_step=-1)


mod_df <- create_close_trj(modelname=modelname,
                           modeldf = mod_df,
                           item)

code <- join_code(mod_df)
path <- paste0(modelname,"/",modelname,"_code.R")
save_text_to_file(code,path)


logfile <- paste0(modelname,"/",modelname,"_log.log")
con <- file(logfile)
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")
thisseed=2
source(path)
env <- env %>% run(10000)
sink(type = "message")
sink() 
close(con )
# log <- read.delim(logfile,header = F,sep = ":")
# names(log) <- c("time","item_Id","item_type","activity","message")
log <- read_log(logfile)
attributes <- get_attributes(env)



arrivals <- get_mon_arrivals(env)
resources <- get_mon_resources(env)

#plot(resources, metric = "utilization")
#plot(arrivals, metric = "flow_time")
pltdata <- attributes %>% 
  filter(key=="truck_id")

pltdata %>% ggplot(aes(x=time,y=value)) +
  geom_step()+
  facet_wrap(facets=vars(name))


get_palette <- scales::brewer_pal(type = "qual", palette = 1)
plot(truck_trj, fill = get_palette,height=3000)
plot(LHD_trj, fill = get_palette,height=3000)
