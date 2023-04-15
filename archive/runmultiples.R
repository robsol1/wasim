source('fns.r')





run_series <-
  function(n_boggers,
           n_trucks,
           stope_access_const,
           runduration = 604800,
           reps = 10,
           loglevel = 1,
           scenario,
           scenar_desc) {
    seed <- (c(1:reps))
    for (i in 1:reps) {

      source('build_run_model.r')
      seed[i]=thisseed
      if (i == 1) {
        resource <- get_mon_resources(env)
        arrive <- get_mon_arrivals(env, per_resource = T)
        atts <- get_mon_attributes(env)
      } else {
        resource <-
          rbind(resource,
                get_mon_resources(env) %>% mutate(replication = i))
        arrive <-
          rbind(arrive,
                get_mon_arrivals(env, per_resource = T) %>% mutate(replication = i))
        atts <-
          rbind(atts, get_mon_attributes(env) %>% mutate(replication = i))
      }
      print(paste0("run ", i))
    }
    
    log <-
      read.delim(
        'code.log.txt',
        sep = ":",
        col.names = c('time', 'item', 'item_type', 'activity', 'record')
      ) %>%
      mutate(time = as.numeric(time),
             hrs = time / 3600)
    atts <- atts %>%
      mutate(equip_type = gsub('[[:digit:]]+', '', name))
    
    stocks <- atts %>%
      filter(key %like% 'stocks_val')
    status <- left_join(
      atts %>% filter(key %like% '_status') %>%
        arrange(replication, name, time) %>%
        group_by(replication, name) %>%
        mutate(
          end_event_time = lead(time),
          event_duration = end_event_time - time
        ),
      read.csv('statuscodes.csv')
    )
    
    
    tum_check <- status %>%
      group_by(replication, name) %>%
      summarise(tot_duration = sum(event_duration, na.rm = (TRUE)))
    
    tum_item_report <- left_join(
      status %>%
        group_by(replication, name, ID) %>%
        summarise(
          events = n(),
          tot_duration = sum(event_duration, na.rm = (TRUE))
        ) %>%
        mutate(mttr = tot_duration / events),
      status %>%
        group_by(replication, name) %>%
        summarise(elapsed_time = sum(event_duration, na.rm = TRUE))
    ) %>%
      mutate(pct = tot_duration * 100 / elapsed_time)
    
    tum_activity_report <- status %>%
      group_by(replication, name, key, ID) %>%
      summarise(events = n(),
                tot_duration = sum(event_duration, na.rm = (TRUE))) %>%
      mutate(mttr = tot_duration / events)
    
    
    tum_equipment_report <- left_join(
      status %>%
        group_by(replication, equip_type, ID) %>%
        summarise(
          events = n(),
          tot_duration = sum(event_duration, na.rm = (TRUE))
        ) %>%
        mutate(mttr = tot_duration / events),
      status %>%
        group_by(replication, equip_type) %>%
        summarise(elapsed_time = sum(event_duration, na.rm = TRUE))
    ) %>%
      mutate(pct = tot_duration * 100 / elapsed_time)
    
    resource_utilisation <- resource %>%
      arrange(replication, resource, time) %>%
      group_by(replication, resource) %>%
      mutate(
        event_end = lead(time),
        event_duration = event_end - time,
        resource_capacity = capacity * event_duration,
        utilisation = event_duration * server
      ) %>%
      group_by(replication, resource) %>%
      summarise(
        resource_capacity = sum(resource_capacity, na.rm = TRUE),
        utilisation = sum(utilisation, na.rm = TRUE)
      ) %>%
      mutate(utilisation_pct = utilisation / resource_capacity)
    
    
    
    throughput <- atts %>%
      filter(key %like% 'cap_prod' & key %like% 'dump') %>%
      group_by(replication, equip_type, name) %>%
      summarise(throughput = max(value)) %>%
      group_by(replication, equip_type) %>%
      summarise(throughput = sum(throughput)) %>%
      pivot_wider(id_cols = replication,
                  names_from = equip_type,
                  values_from = throughput) %>%
      mutate(min = min(bogger, truck)) %>%
      arrange(min)
    
    
    
    last_event <- atts %>%
      group_by(replication, name) %>%
      summarise(last_event = max(time)) %>%
      mutate(pct_run_time = last_event / runduration) %>%
      arrange(last_event)
    
    
    medRun <- as.integer((reps - 1) / 2)
    ret <- list(
      Trucks = n_trucks,
      boggers = n_boggers,
      runduration = runduration,
      stope_access_const = stope_access_const,
      reps = reps,
      tonnes = throughput$min[medRun],
      seed=seed[medRun],
      res_ute = resource_utilisation %>%
        filter(replication == medRun &
                 resource == 'stope_stock_access'),
      tum = tum_equipment_report %>%
        filter(replication == medRun) %>%
        arrange(equip_type, ID)
    )
  }

n_boggers <- 15
n_trucks <- 15
stope_access_const = 1
runduration = 7* 24 * 3600
loglevel = 0
reps = 10
#scenario = 0
scenario_desc='Initial Titration'

for (n_trucks in 11:20) {
  for (n_boggers in (n_trucks-3):(n_trucks+3)) {
    for (stope_access_const in 2:2) {
      scenario=max(all_results$scenario)+1
      print(paste0("trucks :",n_trucks))
      print(paste0("boggers:", n_boggers))
      print(paste0("stope access :", stope_access_const))
      print(paste0("scenario :",scenario))
      res = run_series(
          n_boggers = n_boggers,
          n_trucks = n_trucks,
          stope_access_const = stope_access_const,
          runduration = runduration,
          loglevel = 0,
          reps = reps,
          scenario=scenario,
          scenario_desc=scenario_desc
        )
      if (scenario == 1){
        print("initial TUM")
        tum <- data.frame(res$tum) %>%
          mutate(scenario = scenario)
        access_resource <- data.frame(res$res_ute) %>%
          mutate(scenario = scenario)
        all_results <-data.frame(
            trucks = res$Trucks,
            boggers = res$boggers,
            runtime = res$runduration,
            access_constraint = res$stope_access_const,
            tonnes = res$tonnes
          ) %>%
          mutate(scenario = scenario)
      }  else {
        print("next TUM")
        tum <- rbind(tum, data.frame(res$tum) %>%
                       mutate(scenario = scenario))
        print("next access")
        access_resource <-
          rbind(access_resource,
                data.frame(res$res_ute) %>%
                  mutate(scenario = scenario))
        print("next access")
        all_results <- rbind(
          all_results,
          data.frame(
            trucks = res$Trucks,
            boggers = res$boggers,
            runtime = res$runduration,
            access_constraint = res$stope_access_const,
            tonnes = res$tonnes,
            seed=res$seed
          ) %>%
            mutate(scenario = scenario)
        )
      }
      
    }
  }
}

ggplot(data=all_results,aes(x=trucks,y=tonnes,colour=as.factor(boggers)))+
  geom_point()+
  facet_wrap(facets = vars(access_constraint))+
  lims(y=c(0,250000))


scenres  = run_series(
    n_boggers = 15,
    n_trucks = 15,
    stope_access_const = 1,
    runduration = 1* 24 * 3600,
    loglevel = 0,
    reps = 5
  )
i=1
access_resource <- data.frame(scenres$res_ute) %>% 
  mutate(scenario=i)


all_results <-
  data.frame(
    trucks = res$Trucks,
    boggers = res$boggers,
    runtime = res$runduration,
    access_constraint = res$stope_access_const,
    tonnes = res$reps,
    res$tonnes
  )

