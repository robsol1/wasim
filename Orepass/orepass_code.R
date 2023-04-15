env <- simmer('env' , log_level = 0)

drawpoint_max_stock <- 9999999
drawpoint_access_limit <- 3

stope_stock_max_stock <- 1000
stope_stock_access_limit <- 2

hoist_stock_max_stock <- 9999999
hoist_stock_access_limit <- 1

bogger_unit_capacity <- 7
bogger_mttr <- function() max(1, rnorm(1, 3600, 360))
bogger_mtbf <- function() max(1, rexp(1, rate = 1/(24*3600)))

## Vars for bogger_get_stocks
bogger_get_stocks_delay <- function() max(1, rnorm(1, 50, 5))
cave_capacity <- 1000
bogger_get_stocks_block_id <- 2



bogger_haul_loaded_block_id <- 3
bogger_haul_loaded_delay <- function() max(1, rnorm(1, 90, 9))
## Vars for bogger_dump_stocks
bogger_dump_stocks_delay <- function() max(1, rnorm(1, 20, 2))
truck_capacity <- 70
bogger_dump_stocks_block_id <- 4



bogger_haul_empty_block_id <- 5
bogger_haul_empty_delay <- function() max(1, rnorm(1, 75, 7.5))
last_block_in_bogger_trj=5
truck_unit_capacity <- 70
truck_mttr <- function() max(1, rnorm(1, 3600, 360))
truck_mtbf <- function() max(1, rexp(1, rate = 1/(24*3600)))

## Vars for truck_get_stocks
truck_get_stocks_delay <- function() max(1, rnorm(1, 180, 18))
bogger_capacity <- 7
truck_get_stocks_block_id <- 2



truck_haul_loaded_block_id <- 3
truck_haul_loaded_delay <- function() max(1, rnorm(1, 1200, 120))
## Vars for truck_dump_stocks
truck_dump_stocks_delay <- function() max(1, rnorm(1, 40, 4))
hoist_capacity <- 1000
truck_dump_stocks_block_id <- 4



truck_haul_empty_block_id <- 5
truck_haul_empty_delay <- function() max(1, rnorm(1, 1100, 110))
last_block_in_truck_trj=5 

 bogger_trj <- trajectory('bogger_trj') %>%
	 log_('bogger:setup_trj:init trajectory',level = 1) %>% 
	set_global('bogger_count', 1, mod = '+') %>%
	set_attribute('bogger_id', function() get_global(env, 'bogger_count')) %>% 
	set_attribute('bogger_ute_time', 0) %>%
	set_attribute('bogger_next_bd', bogger_mtbf) %>%
	set_attribute('bogger_next_block',2) %>% 
 	log_('bogger:setup_trj:end Init and start content',level = 1,tag = 'bogger_rollback_to_start')



## branch_if_not_get_stocks start
bogger_trj <- bogger_trj %>%
  branch(
    option = function()ifelse(get_attribute(env, 'bogger_next_block') == bogger_get_stocks_block_id,1,2),
    continue = c(TRUE, TRUE),
    trajectory('bogger_get_stocks_stay_in_block') %>% 
      ## seizeresourceandpaperwork start
      set_attribute('bogger_get_stocks_success', 0) %>%
      set_attribute('local_bogger_get_stocks_status', s_wait_res) %>%
      seize('bogger_get_stocks_resource', 1) %>% 
      ## specific code for get_put
      set_attribute('local_bogger_get_stocks_status', s_wait_stock_access) %>%
      seize('drawpoint_access') %>% 
      set_attribute('local_bogger_get_stocks_status', s_wait_upstream_stock) %>%
      branch( option = function() ifelse(get_global(env, 'drawpoint_stocks_val') < bogger_unit_capacity,1,2),
        continue=c(TRUE,TRUE),
        trajectory('bogger_get_stocks_constrained_by_stocks') %>% 
          release('drawpoint_access') %>% 
          release('bogger_get_stocks_resource', 1) %>%
          log_('bogger:get_stocks:entering trap waiting stock movement on drawpoint',level = 1) %>% 

          trap('stock_added_to_drawpoint_signal') %>%
          wait() %>%
          log_('bogger:get_stocks:continuing from constrained stocks',level = 1),
        trajectory('have_stocks_so_do_work') %>% 
          log_('bogger:get_stocks:adjusting stocks prior to delay',level = 1) %>% 

           set_global('drawpoint_stocks_val' , function() get_global(env, 'drawpoint_stocks_val')-bogger_unit_capacity) %>%
      
      ## delayandpaperwork start
      set_attribute('local_bogger_get_stocks_status', s_working) %>%
      set_attribute('bogger_get_stocks_delay_att', bogger_get_stocks_delay) %>%
      timeout_from_attribute('bogger_get_stocks_delay_att') %>%
      set_attribute('bogger_get_stocks_success', 1) %>%
      set_attribute('bogger_ute_time', function() get_attribute(env, 'bogger_get_stocks_delay_att'), mod = '+') %>%
      set_attribute('bogger_get_stocks_cap_prod', bogger_unit_capacity, mod = '+') %>%
      release('bogger_get_stocks_resource', 1) %>%
      branch(option = function() ifelse(get_attribute(env, 'bogger_next_block') < last_block_in_bogger_trj,1,2),
      continue = c(TRUE, TRUE),
      trajectory('bogger_get_stocks_set_up_for_next_block') %>%
        set_attribute('bogger_next_block', 1, mod = '+'),
      trajectory('bogger_get_stocks_set_up_for_start') %>%
        set_attribute('bogger_next_block', 2)
      ) %>% 
        # specific code for get_put
        release('drawpoint_access') %>% 
              ### This is generic BD type code ..
              log_('bogger:get_stocks :test for bogger BD', level = 1) %>%
              log_(function() {paste0('bogger:get_stocks :ute time = ',get_attribute(env, 'bogger_ute_time'))}, level = 1) %>%
              log_(function() {paste0('bogger:get_stocks :Next BD  = ',get_attribute(env, 'bogger_next_bd'))}, level = 1) %>%
              branch(
                #breakdown
                option = function() ifelse(get_attribute(env, 'bogger_ute_time') >= get_attribute(env, 'bogger_next_bd'),1,2),
                continue = c(TRUE, TRUE),
                trajectory('broken_down') %>%
                  set_attribute('local_bogger_get_stocks_status', s_breakdown) %>% 
                  log_('bogger : get_stocks : status= s,breakdown',level=1) %>% 
                  set_attribute('bd_delay', bogger_mttr) %>%
                  timeout_from_attribute('bd_delay') %>%
                  log_('bogger:get_stocks :continuing! after breakdown', level = 1) %>% 
                  set_attribute('bogger_next_bd', bogger_mtbf, mod='+') %>% 
                  log_(function() {paste0('bogger:get_stocks :update next bd   ',get_attribute(env, 'bogger_next_bd'))}, level = 1),
                trajectory('working') %>%
                  log_('bogger:get_stocks :not broken down', level = 1)
              ) %>% 
        log_(function() paste0('bogger:get_stocks:drawpoint level = ',get_global(env,'drawpoint_stocks_val')),level = 1) %>% 

        log_(function() paste0('bogger:get_stocks: secondary unit capacity = ',cave_capacity),level=1) %>%  
        branch( option = function() ifelse( drawpoint_max_stock - get_global(env, 'drawpoint_stocks_val') < cave_capacity,1,2),
          continue = c(TRUE, TRUE),
          trajectory('bogger_get_stocks_can_free_up_secondary') %>%
          log_('bogger:get_stocks:already ok for potentially blocked resource',level = 1),
          trajectory('bogger_get_stocks_secondary_eq_is_ok') %>%
            log_('bogger:get_stocks:sending signal to free up cave for drawpoint',level = 1) %>% 

            send('stock_removed_from_drawpoint_signal')
            
        )
      ),
    #skipedblock start 
    trajectory('bogger_get_stocks_skip_this_block') %>%
      log_('bogger:get_stocks:Block id is not next block so skip block',level=1)
  )
  #skipedblock end

## branch_if_not_haul_loaded start
bogger_trj <- bogger_trj %>%
  branch(
    option = function()ifelse(get_attribute(env, 'bogger_next_block') == bogger_haul_loaded_block_id,1,2),
    continue = c(TRUE, TRUE),
    trajectory('bogger_haul_loaded_stay_in_block') %>% 
      ## seizeresourceandpaperwork start
      set_attribute('bogger_haul_loaded_success', 0) %>%
      set_attribute('local_bogger_haul_loaded_status', s_wait_res) %>%
      seize('bogger_haul_loaded_resource', 1) %>% 
      ## delayandpaperwork start
      set_attribute('local_bogger_haul_loaded_status', s_working) %>%
      set_attribute('bogger_haul_loaded_delay_att', bogger_haul_loaded_delay) %>%
      timeout_from_attribute('bogger_haul_loaded_delay_att') %>%
      set_attribute('bogger_haul_loaded_success', 1) %>%
      set_attribute('bogger_ute_time', function() get_attribute(env, 'bogger_haul_loaded_delay_att'), mod = '+') %>%
      set_attribute('bogger_haul_loaded_cap_prod', bogger_unit_capacity, mod = '+') %>%
      release('bogger_haul_loaded_resource', 1) %>%
      branch(option = function() ifelse(get_attribute(env, 'bogger_next_block') < last_block_in_bogger_trj,1,2),
      continue = c(TRUE, TRUE),
      trajectory('bogger_haul_loaded_set_up_for_next_block') %>%
        set_attribute('bogger_next_block', 1, mod = '+'),
      trajectory('bogger_haul_loaded_set_up_for_start') %>%
        set_attribute('bogger_next_block', 2)
      ) %>% 
              ### This is generic BD type code ..
              log_('bogger:haul_loaded :test for bogger BD', level = 1) %>%
              log_(function() {paste0('bogger:haul_loaded :ute time = ',get_attribute(env, 'bogger_ute_time'))}, level = 1) %>%
              log_(function() {paste0('bogger:haul_loaded :Next BD  = ',get_attribute(env, 'bogger_next_bd'))}, level = 1) %>%
              branch(
                #breakdown
                option = function() ifelse(get_attribute(env, 'bogger_ute_time') >= get_attribute(env, 'bogger_next_bd'),1,2),
                continue = c(TRUE, TRUE),
                trajectory('broken_down') %>%
                  set_attribute('local_bogger_haul_loaded_status', s_breakdown) %>% 
                  log_('bogger : haul_loaded : status= s,breakdown',level=1) %>% 
                  set_attribute('bd_delay', bogger_mttr) %>%
                  timeout_from_attribute('bd_delay') %>%
                  log_('bogger:haul_loaded :continuing! after breakdown', level = 1) %>% 
                  set_attribute('bogger_next_bd', bogger_mtbf, mod='+') %>% 
                  log_(function() {paste0('bogger:haul_loaded :update next bd   ',get_attribute(env, 'bogger_next_bd'))}, level = 1),
                trajectory('working') %>%
                  log_('bogger:haul_loaded :not broken down', level = 1)
              ),
    #skipedblock start 
    trajectory('bogger_haul_loaded_skip_this_block') %>%
      log_('bogger:haul_loaded:Block id is not next block so skip block',level=1)
  )
  #skipedblock end


## branch_if_not_dump_stocks start
bogger_trj <- bogger_trj %>%
  branch(
    option = function()ifelse(get_attribute(env, 'bogger_next_block') == bogger_dump_stocks_block_id,1,2),
    continue = c(TRUE, TRUE),
    trajectory('bogger_dump_stocks_stay_in_block') %>% 
      ## seizeresourceandpaperwork start
      set_attribute('bogger_dump_stocks_success', 0) %>%
      set_attribute('local_bogger_dump_stocks_status', s_wait_res) %>%
      seize('bogger_dump_stocks_resource', 1) %>% 
      ## specific code for get_put
      set_attribute('local_bogger_dump_stocks_status', s_wait_stock_access) %>%
      seize('stope_stock_access') %>% 
      set_attribute('local_bogger_dump_stocks_status', s_wait_downstream_stock) %>%
      branch( option = function() ifelse( stope_stock_max_stock - get_global(env, 'stope_stock_stocks_val') < bogger_unit_capacity,1,2),
        continue=c(TRUE,TRUE),
        trajectory('bogger_dump_stocks_constrained_by_stocks') %>% 
          release('stope_stock_access') %>% 
          release('bogger_dump_stocks_resource', 1) %>%
          log_('bogger:dump_stocks:entering trap waiting stock movement on stope_stock',level = 1) %>% 

          trap('stock_removed_from_stope_stock_signal') %>%
          wait() %>%
          log_('bogger:dump_stocks:continuing from constrained stocks',level = 1),
        trajectory('have_stocks_so_do_work') %>% 
          log_('bogger:dump_stocks:adjusting stocks prior to delay',level = 1) %>% 

           set_global('stope_stock_stocks_val', bogger_unit_capacity,mod='+') %>%
      
      ## delayandpaperwork start
      set_attribute('local_bogger_dump_stocks_status', s_working) %>%
      set_attribute('bogger_dump_stocks_delay_att', bogger_dump_stocks_delay) %>%
      timeout_from_attribute('bogger_dump_stocks_delay_att') %>%
      set_attribute('bogger_dump_stocks_success', 1) %>%
      set_attribute('bogger_ute_time', function() get_attribute(env, 'bogger_dump_stocks_delay_att'), mod = '+') %>%
      set_attribute('bogger_dump_stocks_cap_prod', bogger_unit_capacity, mod = '+') %>%
      release('bogger_dump_stocks_resource', 1) %>%
      branch(option = function() ifelse(get_attribute(env, 'bogger_next_block') < last_block_in_bogger_trj,1,2),
      continue = c(TRUE, TRUE),
      trajectory('bogger_dump_stocks_set_up_for_next_block') %>%
        set_attribute('bogger_next_block', 1, mod = '+'),
      trajectory('bogger_dump_stocks_set_up_for_start') %>%
        set_attribute('bogger_next_block', 2)
      ) %>% 
        # specific code for get_put
        release('stope_stock_access') %>% 
              ### This is generic BD type code ..
              log_('bogger:dump_stocks :test for bogger BD', level = 1) %>%
              log_(function() {paste0('bogger:dump_stocks :ute time = ',get_attribute(env, 'bogger_ute_time'))}, level = 1) %>%
              log_(function() {paste0('bogger:dump_stocks :Next BD  = ',get_attribute(env, 'bogger_next_bd'))}, level = 1) %>%
              branch(
                #breakdown
                option = function() ifelse(get_attribute(env, 'bogger_ute_time') >= get_attribute(env, 'bogger_next_bd'),1,2),
                continue = c(TRUE, TRUE),
                trajectory('broken_down') %>%
                  set_attribute('local_bogger_dump_stocks_status', s_breakdown) %>% 
                  log_('bogger : dump_stocks : status= s,breakdown',level=1) %>% 
                  set_attribute('bd_delay', bogger_mttr) %>%
                  timeout_from_attribute('bd_delay') %>%
                  log_('bogger:dump_stocks :continuing! after breakdown', level = 1) %>% 
                  set_attribute('bogger_next_bd', bogger_mtbf, mod='+') %>% 
                  log_(function() {paste0('bogger:dump_stocks :update next bd   ',get_attribute(env, 'bogger_next_bd'))}, level = 1),
                trajectory('working') %>%
                  log_('bogger:dump_stocks :not broken down', level = 1)
              ) %>% 
        log_(function() paste0('bogger:dump_stocks:stope_stock level = ',get_global(env,'stope_stock_stocks_val')),level = 1) %>% 

        log_(function() paste0('bogger:dump_stocks: secondary unit capacity = ',truck_capacity),level=1) %>%  
        branch( option = function() ifelse(get_global(env, 'stope_stock_stocks_val') < truck_capacity,1,2),
          continue = c(TRUE, TRUE),
          trajectory('bogger_dump_stocks_can_free_up_secondary') %>%
          log_('bogger:dump_stocks:already ok for potentially starved resource',level = 1),
          trajectory('bogger_dump_stocks_secondary_eq_is_ok') %>%
            log_('bogger:dump_stocks:sending signal to free up truck for stope_stock',level = 1) %>% 

            send('stock_added_to_stope_stock_signal')
            
        )
      ),
    #skipedblock start 
    trajectory('bogger_dump_stocks_skip_this_block') %>%
      log_('bogger:dump_stocks:Block id is not next block so skip block',level=1)
  )
  #skipedblock end

## branch_if_not_haul_empty start
bogger_trj <- bogger_trj %>%
  branch(
    option = function()ifelse(get_attribute(env, 'bogger_next_block') == bogger_haul_empty_block_id,1,2),
    continue = c(TRUE, TRUE),
    trajectory('bogger_haul_empty_stay_in_block') %>% 
      ## seizeresourceandpaperwork start
      set_attribute('bogger_haul_empty_success', 0) %>%
      set_attribute('local_bogger_haul_empty_status', s_wait_res) %>%
      seize('bogger_haul_empty_resource', 1) %>% 
      ## delayandpaperwork start
      set_attribute('local_bogger_haul_empty_status', s_working) %>%
      set_attribute('bogger_haul_empty_delay_att', bogger_haul_empty_delay) %>%
      timeout_from_attribute('bogger_haul_empty_delay_att') %>%
      set_attribute('bogger_haul_empty_success', 1) %>%
      set_attribute('bogger_ute_time', function() get_attribute(env, 'bogger_haul_empty_delay_att'), mod = '+') %>%
      set_attribute('bogger_haul_empty_cap_prod', bogger_unit_capacity, mod = '+') %>%
      release('bogger_haul_empty_resource', 1) %>%
      branch(option = function() ifelse(get_attribute(env, 'bogger_next_block') < last_block_in_bogger_trj,1,2),
      continue = c(TRUE, TRUE),
      trajectory('bogger_haul_empty_set_up_for_next_block') %>%
        set_attribute('bogger_next_block', 1, mod = '+'),
      trajectory('bogger_haul_empty_set_up_for_start') %>%
        set_attribute('bogger_next_block', 2)
      ) %>% 
              ### This is generic BD type code ..
              log_('bogger:haul_empty :test for bogger BD', level = 1) %>%
              log_(function() {paste0('bogger:haul_empty :ute time = ',get_attribute(env, 'bogger_ute_time'))}, level = 1) %>%
              log_(function() {paste0('bogger:haul_empty :Next BD  = ',get_attribute(env, 'bogger_next_bd'))}, level = 1) %>%
              branch(
                #breakdown
                option = function() ifelse(get_attribute(env, 'bogger_ute_time') >= get_attribute(env, 'bogger_next_bd'),1,2),
                continue = c(TRUE, TRUE),
                trajectory('broken_down') %>%
                  set_attribute('local_bogger_haul_empty_status', s_breakdown) %>% 
                  log_('bogger : haul_empty : status= s,breakdown',level=1) %>% 
                  set_attribute('bd_delay', bogger_mttr) %>%
                  timeout_from_attribute('bd_delay') %>%
                  log_('bogger:haul_empty :continuing! after breakdown', level = 1) %>% 
                  set_attribute('bogger_next_bd', bogger_mtbf, mod='+') %>% 
                  log_(function() {paste0('bogger:haul_empty :update next bd   ',get_attribute(env, 'bogger_next_bd'))}, level = 1),
                trajectory('working') %>%
                  log_('bogger:haul_empty :not broken down', level = 1)
              ),
    #skipedblock start 
    trajectory('bogger_haul_empty_skip_this_block') %>%
      log_('bogger:haul_empty:Block id is not next block so skip block',level=1)
  )
  #skipedblock end
bogger_trj <- bogger_trj %>% 
  simmer::rollback(target = 'bogger_rollback_to_start')
truck_trj <- trajectory('truck_trj') %>%
	 log_('truck:setup_trj:init trajectory',level = 1) %>% 
	set_global('truck_count', 1, mod = '+') %>%
	set_attribute('truck_id', function() get_global(env, 'truck_count')) %>% 
	set_attribute('truck_ute_time', 0) %>%
	set_attribute('truck_next_bd', truck_mtbf) %>%
	set_attribute('truck_next_block',2) %>% 
 	log_('truck:setup_trj:end Init and start content',level = 1,tag = 'truck_rollback_to_start')



## branch_if_not_get_stocks start
truck_trj <- truck_trj %>%
  branch(
    option = function()ifelse(get_attribute(env, 'truck_next_block') == truck_get_stocks_block_id,1,2),
    continue = c(TRUE, TRUE),
    trajectory('truck_get_stocks_stay_in_block') %>% 
      ## seizeresourceandpaperwork start
      set_attribute('truck_get_stocks_success', 0) %>%
      set_attribute('local_truck_get_stocks_status', s_wait_res) %>%
      seize('truck_get_stocks_resource', 1) %>% 
      ## specific code for get_put
      set_attribute('local_truck_get_stocks_status', s_wait_stock_access) %>%
      seize('stope_stock_access') %>% 
      set_attribute('local_truck_get_stocks_status', s_wait_upstream_stock) %>%
      branch( option = function() ifelse(get_global(env, 'stope_stock_stocks_val') < truck_unit_capacity,1,2),
        continue=c(TRUE,TRUE),
        trajectory('truck_get_stocks_constrained_by_stocks') %>% 
          release('stope_stock_access') %>% 
          release('truck_get_stocks_resource', 1) %>%
          log_('truck:get_stocks:entering trap waiting stock movement on stope_stock',level = 1) %>% 

          trap('stock_added_to_stope_stock_signal') %>%
          wait() %>%
          log_('truck:get_stocks:continuing from constrained stocks',level = 1),
        trajectory('have_stocks_so_do_work') %>% 
          log_('truck:get_stocks:adjusting stocks prior to delay',level = 1) %>% 

           set_global('stope_stock_stocks_val' , function() get_global(env, 'stope_stock_stocks_val')-truck_unit_capacity) %>%
      
      ## delayandpaperwork start
      set_attribute('local_truck_get_stocks_status', s_working) %>%
      set_attribute('truck_get_stocks_delay_att', truck_get_stocks_delay) %>%
      timeout_from_attribute('truck_get_stocks_delay_att') %>%
      set_attribute('truck_get_stocks_success', 1) %>%
      set_attribute('truck_ute_time', function() get_attribute(env, 'truck_get_stocks_delay_att'), mod = '+') %>%
      set_attribute('truck_get_stocks_cap_prod', truck_unit_capacity, mod = '+') %>%
      release('truck_get_stocks_resource', 1) %>%
      branch(option = function() ifelse(get_attribute(env, 'truck_next_block') < last_block_in_truck_trj,1,2),
      continue = c(TRUE, TRUE),
      trajectory('truck_get_stocks_set_up_for_next_block') %>%
        set_attribute('truck_next_block', 1, mod = '+'),
      trajectory('truck_get_stocks_set_up_for_start') %>%
        set_attribute('truck_next_block', 2)
      ) %>% 
        # specific code for get_put
        release('stope_stock_access') %>% 
              ### This is generic BD type code ..
              log_('truck:get_stocks :test for truck BD', level = 1) %>%
              log_(function() {paste0('truck:get_stocks :ute time = ',get_attribute(env, 'truck_ute_time'))}, level = 1) %>%
              log_(function() {paste0('truck:get_stocks :Next BD  = ',get_attribute(env, 'truck_next_bd'))}, level = 1) %>%
              branch(
                #breakdown
                option = function() ifelse(get_attribute(env, 'truck_ute_time') >= get_attribute(env, 'truck_next_bd'),1,2),
                continue = c(TRUE, TRUE),
                trajectory('broken_down') %>%
                  set_attribute('local_truck_get_stocks_status', s_breakdown) %>% 
                  log_('truck : get_stocks : status= s,breakdown',level=1) %>% 
                  set_attribute('bd_delay', truck_mttr) %>%
                  timeout_from_attribute('bd_delay') %>%
                  log_('truck:get_stocks :continuing! after breakdown', level = 1) %>% 
                  set_attribute('truck_next_bd', truck_mtbf, mod='+') %>% 
                  log_(function() {paste0('truck:get_stocks :update next bd   ',get_attribute(env, 'truck_next_bd'))}, level = 1),
                trajectory('working') %>%
                  log_('truck:get_stocks :not broken down', level = 1)
              ) %>% 
        log_(function() paste0('truck:get_stocks:stope_stock level = ',get_global(env,'stope_stock_stocks_val')),level = 1) %>% 

        log_(function() paste0('truck:get_stocks: secondary unit capacity = ',bogger_capacity),level=1) %>%  
        branch( option = function() ifelse( stope_stock_max_stock - get_global(env, 'stope_stock_stocks_val') < bogger_capacity,1,2),
          continue = c(TRUE, TRUE),
          trajectory('truck_get_stocks_can_free_up_secondary') %>%
          log_('truck:get_stocks:already ok for potentially blocked resource',level = 1),
          trajectory('truck_get_stocks_secondary_eq_is_ok') %>%
            log_('truck:get_stocks:sending signal to free up bogger for stope_stock',level = 1) %>% 

            send('stock_removed_from_stope_stock_signal')
            
        )
      ),
    #skipedblock start 
    trajectory('truck_get_stocks_skip_this_block') %>%
      log_('truck:get_stocks:Block id is not next block so skip block',level=1)
  )
  #skipedblock end

## branch_if_not_haul_loaded start
truck_trj <- truck_trj %>%
  branch(
    option = function()ifelse(get_attribute(env, 'truck_next_block') == truck_haul_loaded_block_id,1,2),
    continue = c(TRUE, TRUE),
    trajectory('truck_haul_loaded_stay_in_block') %>% 
      ## seizeresourceandpaperwork start
      set_attribute('truck_haul_loaded_success', 0) %>%
      set_attribute('local_truck_haul_loaded_status', s_wait_res) %>%
      seize('truck_haul_loaded_resource', 1) %>% 
      ## delayandpaperwork start
      set_attribute('local_truck_haul_loaded_status', s_working) %>%
      set_attribute('truck_haul_loaded_delay_att', truck_haul_loaded_delay) %>%
      timeout_from_attribute('truck_haul_loaded_delay_att') %>%
      set_attribute('truck_haul_loaded_success', 1) %>%
      set_attribute('truck_ute_time', function() get_attribute(env, 'truck_haul_loaded_delay_att'), mod = '+') %>%
      set_attribute('truck_haul_loaded_cap_prod', truck_unit_capacity, mod = '+') %>%
      release('truck_haul_loaded_resource', 1) %>%
      branch(option = function() ifelse(get_attribute(env, 'truck_next_block') < last_block_in_truck_trj,1,2),
      continue = c(TRUE, TRUE),
      trajectory('truck_haul_loaded_set_up_for_next_block') %>%
        set_attribute('truck_next_block', 1, mod = '+'),
      trajectory('truck_haul_loaded_set_up_for_start') %>%
        set_attribute('truck_next_block', 2)
      ) %>% 
              ### This is generic BD type code ..
              log_('truck:haul_loaded :test for truck BD', level = 1) %>%
              log_(function() {paste0('truck:haul_loaded :ute time = ',get_attribute(env, 'truck_ute_time'))}, level = 1) %>%
              log_(function() {paste0('truck:haul_loaded :Next BD  = ',get_attribute(env, 'truck_next_bd'))}, level = 1) %>%
              branch(
                #breakdown
                option = function() ifelse(get_attribute(env, 'truck_ute_time') >= get_attribute(env, 'truck_next_bd'),1,2),
                continue = c(TRUE, TRUE),
                trajectory('broken_down') %>%
                  set_attribute('local_truck_haul_loaded_status', s_breakdown) %>% 
                  log_('truck : haul_loaded : status= s,breakdown',level=1) %>% 
                  set_attribute('bd_delay', truck_mttr) %>%
                  timeout_from_attribute('bd_delay') %>%
                  log_('truck:haul_loaded :continuing! after breakdown', level = 1) %>% 
                  set_attribute('truck_next_bd', truck_mtbf, mod='+') %>% 
                  log_(function() {paste0('truck:haul_loaded :update next bd   ',get_attribute(env, 'truck_next_bd'))}, level = 1),
                trajectory('working') %>%
                  log_('truck:haul_loaded :not broken down', level = 1)
              ),
    #skipedblock start 
    trajectory('truck_haul_loaded_skip_this_block') %>%
      log_('truck:haul_loaded:Block id is not next block so skip block',level=1)
  )
  #skipedblock end


## branch_if_not_dump_stocks start
truck_trj <- truck_trj %>%
  branch(
    option = function()ifelse(get_attribute(env, 'truck_next_block') == truck_dump_stocks_block_id,1,2),
    continue = c(TRUE, TRUE),
    trajectory('truck_dump_stocks_stay_in_block') %>% 
      ## seizeresourceandpaperwork start
      set_attribute('truck_dump_stocks_success', 0) %>%
      set_attribute('local_truck_dump_stocks_status', s_wait_res) %>%
      seize('truck_dump_stocks_resource', 1) %>% 
      ## specific code for get_put
      set_attribute('local_truck_dump_stocks_status', s_wait_stock_access) %>%
      seize('hoist_stock_access') %>% 
      set_attribute('local_truck_dump_stocks_status', s_wait_downstream_stock) %>%
      branch( option = function() ifelse( hoist_stock_max_stock - get_global(env, 'hoist_stock_stocks_val') < truck_unit_capacity,1,2),
        continue=c(TRUE,TRUE),
        trajectory('truck_dump_stocks_constrained_by_stocks') %>% 
          release('hoist_stock_access') %>% 
          release('truck_dump_stocks_resource', 1) %>%
          log_('truck:dump_stocks:entering trap waiting stock movement on hoist_stock',level = 1) %>% 

          trap('stock_removed_from_hoist_stock_signal') %>%
          wait() %>%
          log_('truck:dump_stocks:continuing from constrained stocks',level = 1),
        trajectory('have_stocks_so_do_work') %>% 
          log_('truck:dump_stocks:adjusting stocks prior to delay',level = 1) %>% 

           set_global('hoist_stock_stocks_val', truck_unit_capacity,mod='+') %>%
      
      ## delayandpaperwork start
      set_attribute('local_truck_dump_stocks_status', s_working) %>%
      set_attribute('truck_dump_stocks_delay_att', truck_dump_stocks_delay) %>%
      timeout_from_attribute('truck_dump_stocks_delay_att') %>%
      set_attribute('truck_dump_stocks_success', 1) %>%
      set_attribute('truck_ute_time', function() get_attribute(env, 'truck_dump_stocks_delay_att'), mod = '+') %>%
      set_attribute('truck_dump_stocks_cap_prod', truck_unit_capacity, mod = '+') %>%
      release('truck_dump_stocks_resource', 1) %>%
      branch(option = function() ifelse(get_attribute(env, 'truck_next_block') < last_block_in_truck_trj,1,2),
      continue = c(TRUE, TRUE),
      trajectory('truck_dump_stocks_set_up_for_next_block') %>%
        set_attribute('truck_next_block', 1, mod = '+'),
      trajectory('truck_dump_stocks_set_up_for_start') %>%
        set_attribute('truck_next_block', 2)
      ) %>% 
        # specific code for get_put
        release('hoist_stock_access') %>% 
              ### This is generic BD type code ..
              log_('truck:dump_stocks :test for truck BD', level = 1) %>%
              log_(function() {paste0('truck:dump_stocks :ute time = ',get_attribute(env, 'truck_ute_time'))}, level = 1) %>%
              log_(function() {paste0('truck:dump_stocks :Next BD  = ',get_attribute(env, 'truck_next_bd'))}, level = 1) %>%
              branch(
                #breakdown
                option = function() ifelse(get_attribute(env, 'truck_ute_time') >= get_attribute(env, 'truck_next_bd'),1,2),
                continue = c(TRUE, TRUE),
                trajectory('broken_down') %>%
                  set_attribute('local_truck_dump_stocks_status', s_breakdown) %>% 
                  log_('truck : dump_stocks : status= s,breakdown',level=1) %>% 
                  set_attribute('bd_delay', truck_mttr) %>%
                  timeout_from_attribute('bd_delay') %>%
                  log_('truck:dump_stocks :continuing! after breakdown', level = 1) %>% 
                  set_attribute('truck_next_bd', truck_mtbf, mod='+') %>% 
                  log_(function() {paste0('truck:dump_stocks :update next bd   ',get_attribute(env, 'truck_next_bd'))}, level = 1),
                trajectory('working') %>%
                  log_('truck:dump_stocks :not broken down', level = 1)
              ) %>% 
        log_(function() paste0('truck:dump_stocks:hoist_stock level = ',get_global(env,'hoist_stock_stocks_val')),level = 1) %>% 

        log_(function() paste0('truck:dump_stocks: secondary unit capacity = ',hoist_capacity),level=1) %>%  
        branch( option = function() ifelse(get_global(env, 'hoist_stock_stocks_val') < hoist_capacity,1,2),
          continue = c(TRUE, TRUE),
          trajectory('truck_dump_stocks_can_free_up_secondary') %>%
          log_('truck:dump_stocks:already ok for potentially starved resource',level = 1),
          trajectory('truck_dump_stocks_secondary_eq_is_ok') %>%
            log_('truck:dump_stocks:sending signal to free up hoist for hoist_stock',level = 1) %>% 

            send('stock_added_to_hoist_stock_signal')
            
        )
      ),
    #skipedblock start 
    trajectory('truck_dump_stocks_skip_this_block') %>%
      log_('truck:dump_stocks:Block id is not next block so skip block',level=1)
  )
  #skipedblock end

## branch_if_not_haul_empty start
truck_trj <- truck_trj %>%
  branch(
    option = function()ifelse(get_attribute(env, 'truck_next_block') == truck_haul_empty_block_id,1,2),
    continue = c(TRUE, TRUE),
    trajectory('truck_haul_empty_stay_in_block') %>% 
      ## seizeresourceandpaperwork start
      set_attribute('truck_haul_empty_success', 0) %>%
      set_attribute('local_truck_haul_empty_status', s_wait_res) %>%
      seize('truck_haul_empty_resource', 1) %>% 
      ## delayandpaperwork start
      set_attribute('local_truck_haul_empty_status', s_working) %>%
      set_attribute('truck_haul_empty_delay_att', truck_haul_empty_delay) %>%
      timeout_from_attribute('truck_haul_empty_delay_att') %>%
      set_attribute('truck_haul_empty_success', 1) %>%
      set_attribute('truck_ute_time', function() get_attribute(env, 'truck_haul_empty_delay_att'), mod = '+') %>%
      set_attribute('truck_haul_empty_cap_prod', truck_unit_capacity, mod = '+') %>%
      release('truck_haul_empty_resource', 1) %>%
      branch(option = function() ifelse(get_attribute(env, 'truck_next_block') < last_block_in_truck_trj,1,2),
      continue = c(TRUE, TRUE),
      trajectory('truck_haul_empty_set_up_for_next_block') %>%
        set_attribute('truck_next_block', 1, mod = '+'),
      trajectory('truck_haul_empty_set_up_for_start') %>%
        set_attribute('truck_next_block', 2)
      ) %>% 
              ### This is generic BD type code ..
              log_('truck:haul_empty :test for truck BD', level = 1) %>%
              log_(function() {paste0('truck:haul_empty :ute time = ',get_attribute(env, 'truck_ute_time'))}, level = 1) %>%
              log_(function() {paste0('truck:haul_empty :Next BD  = ',get_attribute(env, 'truck_next_bd'))}, level = 1) %>%
              branch(
                #breakdown
                option = function() ifelse(get_attribute(env, 'truck_ute_time') >= get_attribute(env, 'truck_next_bd'),1,2),
                continue = c(TRUE, TRUE),
                trajectory('broken_down') %>%
                  set_attribute('local_truck_haul_empty_status', s_breakdown) %>% 
                  log_('truck : haul_empty : status= s,breakdown',level=1) %>% 
                  set_attribute('bd_delay', truck_mttr) %>%
                  timeout_from_attribute('bd_delay') %>%
                  log_('truck:haul_empty :continuing! after breakdown', level = 1) %>% 
                  set_attribute('truck_next_bd', truck_mtbf, mod='+') %>% 
                  log_(function() {paste0('truck:haul_empty :update next bd   ',get_attribute(env, 'truck_next_bd'))}, level = 1),
                trajectory('working') %>%
                  log_('truck:haul_empty :not broken down', level = 1)
              ),
    #skipedblock start 
    trajectory('truck_haul_empty_skip_this_block') %>%
      log_('truck:haul_empty:Block id is not next block so skip block',level=1)
  )
  #skipedblock end
truck_trj <- truck_trj %>% 
  simmer::rollback(target = 'truck_rollback_to_start') 

 env <-  env  %>%
  add_global('drawpoint_stocks_val',9999999) %>%
  add_resource('drawpoint_access', 3,preemptive = TRUE,preempt_order = 'fifo')
  

env <-  env  %>%
  add_global('stope_stock_stocks_val',500) %>%
  add_resource('stope_stock_access', 2,preemptive = TRUE,preempt_order = 'fifo')
  

env <-  env  %>%
  add_global('hoist_stock_stocks_val',0) %>%
  add_resource('hoist_stock_access', 1,preemptive = TRUE,preempt_order = 'fifo')
  

env <- env %>%
	add_generator('bogger', trajectory = bogger_trj, at((1:15)), mon = 2)



## env for bogger get_stocks
  env <- env %>%
    add_global('bogger_get_stocks_count',0) %>%
    add_resource('bogger_get_stocks_resource', 15,preemptive = TRUE,preempt_order = 'fifo')
  


## env for bogger haul_loaded
env <- env %>%
add_resource('bogger_haul_loaded_resource', 15,preemptive = TRUE,preempt_order = 'fifo')



## env for bogger dump_stocks
  env <- env %>%
    add_global('bogger_dump_stocks_count',0) %>%
    add_resource('bogger_dump_stocks_resource', 15,preemptive = TRUE,preempt_order = 'fifo')
  


## env for bogger haul_empty
env <- env %>%
add_resource('bogger_haul_empty_resource', 15,preemptive = TRUE,preempt_order = 'fifo')

env <- env %>%
	add_generator('truck', trajectory = truck_trj, at((1:6)), mon = 2)



## env for truck get_stocks
  env <- env %>%
    add_global('truck_get_stocks_count',0) %>%
    add_resource('truck_get_stocks_resource', 1,preemptive = TRUE,preempt_order = 'fifo')
  


## env for truck haul_loaded
env <- env %>%
add_resource('truck_haul_loaded_resource', 6,preemptive = TRUE,preempt_order = 'fifo')



## env for truck dump_stocks
  env <- env %>%
    add_global('truck_dump_stocks_count',0) %>%
    add_resource('truck_dump_stocks_resource', 1,preemptive = TRUE,preempt_order = 'fifo')
  


## env for truck haul_empty
env <- env %>%
add_resource('truck_haul_empty_resource', 6,preemptive = TRUE,preempt_order = 'fifo')

