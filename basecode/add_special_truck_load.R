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
                
                ## Following is modified from normal delay with single stock action
                #define new delay code and wait for loader signal
                set_attribute('local_truck_get_stocks_status', s_wait_sec_eq) %>%
                set_global("truck_waiting_loader",1,mod="+") %>% 
                trap('wait_for_loader') %>% 
                log_('truck:get_stocks:waiting for loader',level = 1) %>% 
                wait() %>% 
                
                set_global("truck_waiting_loader",1,mod="-") %>% 
                log_('truck:get_stocks:got loader',level = 1) %>% 
                ## delayandpaperwork start
                set_attribute('local_truck_get_stocks_status', s_working) %>%
                set_attribute('truck_get_load_time_start',  function() simmer::now(env)) %>%
                trap('wait_complete_load') %>% 
                log_('truck:get_stocks:wait while loading',level = 1) %>% 
                wait() %>% 
                
                log_('truck:get_stocks:finished loading',level = 1) %>% 
                set_attribute("truck_get_stocks_delay_att",function() simmer::now(env)-get_attribute(env,'truck_get_load_time_start')) %>% 
                ## End specific
                
                set_attribute('truck_get_stocks_success', 1) %>%
                set_attribute('truck_ute_time', function() get_attribute(env, 'truck_get_stocks_delay_att'), mod = '+') %>%
                set_attribute('truck_get_stocks_cap_prod', truck_unit_capacity, mod = '+') %>%
                release('truck_get_stocks_resource', 1) %>%
                branch(option = function() ifelse(get_attribute(env, 'truck_next_block') < last_block_in_truck_trj,1,2),
                       continue = c(TRUE, TRUE),
                       trajectory('truck_get_stocks_set_up_for_next_block') %>%
                         #set_attribute('truck_next_block', 1, mod = '+'),
                         set_attribute('truck_next_block',3),
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