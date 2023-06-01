env <- simmer('env' , log_level = 0)


trj1 <- trajectory() %>% 
  log_("item enter block") %>% 
  trap('into') %>%
  log_("item arrived wait") %>%
  set_global("truck_waiting_loader",1) %>% 
  wait() %>%
  set_attribute('start_load', function() simmer::now(env)) %>%
  log_("left wait") %>% 
  set_global("truck_waiting_loader",0) %>% 
  trap('outof') %>%
  wait() %>%
  log_("exit") %>% 
  set_attribute('end_load', function() simmer::now(env)) %>% 
  set_attribute("truck_delay",function() simmer::now(env)-get_attribute(env,'start_load'))


trj2 <- trajectory() %>% 
  log_("loader enter block",tag = "loop") %>% 
  branch( option = function() ifelse(get_global(env, 'truck_waiting_loader') >0,1,2),
          continue=c(TRUE,TRUE),
          trajectory('trigger_truck_load') %>%
            send('into') %>% 
            timeout(0.5) %>% 
            send('outof'),
          trajectory("keep_looking") %>% 
            timeout(4)) %>% 
  log_("keep looking") %>% 
  simmer::rollback("loop")
  

env <- env %>%
  add_generator('truck', trajectory = trj1, at((10)), mon = 2)
env <- env %>%
  add_generator('bogger', trajectory = trj2, at((1)), mon = 2)

env <- env %>%
  add_global('truck_waiting_loader',0)
env <- env %>% run(100)

get_mon_attributes(env)

library(simmer.plot)

get_palette <- scales::brewer_pal(type = "qual", palette = 1)
plot(bogger_trj, fill = get_palette)
