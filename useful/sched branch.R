source('fns.R')

data.env <- new.env()
assign('global.df',data.frame(x=c(1:200),y=c(601:800)),data.env)
arr=data.env$global.df


get_global_array <- function(arr,row,col){
  print(arr[row,col])
}
scheduler <- function(schedule){
  if(schedule=='sched_loader'){
    gatt <- get_global(env, 'globalatt1')
    set_global('globalatt1', gatt)
    trj=3
  } else {
    trj=2
  }
  return(trj)
}

env <- simmer()
HD_trj <-trajectory() %>% 
  branch( option = function() scheduler('sched_loader'),
          continue=c(TRUE,TRUE,TRUE),
          trajectory('sched_trajectory_1') %>% 
            log_('LHD:sched_branch:Entering branch trajectory 1',level = 1) %>% 
            set_attribute('LHD_next_block',4),
          trajectory('sched_trajectory_2') %>% 
            log_('LHD:sched_branch:Entering branch trajectory 2',level = 1) %>% 
            set_attribute('LHD_next_block',5),
          trajectory('sched_rajectory_3') %>% 
            log_('LHD:sched_branch:Entering branch trajectory 3',level = 1) %>% 
            set_attribute('LHD_next_block',6)
  ) 


bogger_trj <- trajectory('bogger_trj') %>%
  log_('item enter') %>% 
  set_global('globalatt1', 2) %>% 
  branch( option = function() scheduler('sched_loader'),
    continue = c(TRUE, TRUE,TRUE),
    trajectory('sched_trajectory_1') %>% 
      log_('LHD:sched_branch:Entering branch trajectory 1',level = 1),
    trajectory('sched_trajectory_2') %>% 
      log_('LHD:sched_branch:Entering branch trajectory 2',level = 1),
    trajectory('sched_rajectory_3') %>% 
      log_('LHD:sched_branch:Entering branch trajectory 3',level = 1) 
  ) %>% 
  log_('item exit')

  
  
trj <-
  branch( option = function() 3,
          continue=c(TRUE,TRUE,TRUE),
          trajectory('sched_trajectory_1') %>% 
            log_('LHD:sched_branch:Entering branch trajectory 1',level = 1),
          trajectory('sched_trajectory_2') %>% 
            log_('LHD:sched_branch:Entering branch trajectory 2',level = 1),
          trajectory('sched_rajectory_3') %>% 
            log_('LHD:sched_branch:Entering branch trajectory 3',level = 1) 
  ) %>% 
  log_('item exit')

 



env <- env %>%
  add_generator('bogger', trajectory = bogger_trj, at((1:6)), mon = 2) %>% 
  add_global('globalatt1',0)

env <- env %>% run(1000)
arrivals <- get_mon_arrivals(env)

