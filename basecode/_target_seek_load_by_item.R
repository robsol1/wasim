# notes
# Currently the item breaks down after completion of the event and releases both the activity resource and access to the stockpile
  
add_target_seek_load_by_item<- function(modelname,
                                                 mod_df,
                                                 item,
                                                 activity = 'get_stocks',
                                                 trj_step = -1,
                                                 number_of_resources = '1',
                                                 item_activity_delay = 'function() max(1, rnorm(1, 5, .2))',
                                                 stockpile,
                                                 secondary_unit_name,
                                                 next_trj_step = -1) {

  if (trj_step < 0) {
    trj_step <- length(which(mod_df$item == item)) + 1
  }
  if (next_trj_step < 0) {
    next_trj_step <- trj_step + 1
  }

  

trj_txt <- paste0("
  item_trj <- item_trj %>%
  branch(option = function()ifelse(get_attribute(env, 'item_next_block') == item_activity_block_id,1,2),
         continue = c(TRUE, TRUE),
         trajectory('item_activity_stay_in_block') %>%
         ",robs_log("Actioning this block",ret=FALSE),"
           set_attribute('item_activity_success', 0) %>%
           set_attribute('local_item_activity_status', s_wait_stock_access) %>%
           ",robs_log("Entering seize",ret=FALSE),"
           seize('",paste0('item_activity_',trj_step,'_res'),"') %>% 
           branch( option = function() ifelse(get_global(env, 'stockpile_stocks_val') < item_unit_capacity,1,2),
                   continue=c(TRUE,TRUE),
                   trajectory('item_activity_constrained_by_stocks') %>%
                     ",robs_log("entering trap waiting stock movement on stockpile",ret=FALSE),"
                     trap('stock_added_to_stockpile_signal') %>%
                     wait() %>%
                     release('",paste0('item_activity_',trj_step,'_res'),"') %>% 
                     ",robs_log("continuing from constrained stocks",pipe=FALSE,ret=FALSE),",
                   trajectory('have_stocks_so_do_work') %>% 
                     set_attribute('local_item_activity_status', s_wait_sec_eq) %>%
                     set_global('item_activity_waiting_secondary_unit_name',1,mod='+') %>% 
                     trap('secondary_unit_name_available') %>% 
                     ",robs_log('waiting for loader',ret=FALSE),"
                     wait() %>% 
                     set_global('item_activity_waiting_secondary_unit_name',-1,mod='+') %>% 
                     set_attribute('local_item_activity_status', s_working) %>%
                     set_attribute('item_get_load_time_start',  function() simmer::now(env)) %>%
                     trap('wait_complete_load') %>% 
                     ",robs_log('wait while loading',ret=FALSE)," 
                     wait() %>%
                     set_attribute('item_activity_success', 1) %>%
                     set_attribute('item_ute_time', function() get_attribute(env, 'item_activity_delay_att'), mod = '+') %>%
                     set_attribute('item_activity_cap_prod', item_unit_capacity, mod = '+') %>%
                     release('",paste0('item_activity_',trj_step,'_res'),"') %>% 
                     # branch(option = function() ifelse(get_attribute(env, 'item_next_block') < last_block_in_item_trj,1,2),
                     #        continue = c(TRUE, TRUE),
                     #        trajectory('item_activity_set_up_for_next_block') %>%
                     #          set_attribute('item_next_block',", next_trj_step,"),
                     #        trajectory('item_activity_set_up_for_start') %>%
                     #          set_attribute('item_next_block', 2)
                     # )
                     set_attribute('item_next_block',", next_trj_step,")
                  ) %>% " ,
        item_breakdown_code()," ,
         trajectory('item_activity_skip_this_block') %>%
           log_('item:activity:Block id is not next block so skip block',level=1)
         )%>% 
  log_('item:activity:End and go to next block',level=1)")

var_txt <-
  paste0(
    "## Vars for item_activity\nitem_activity_delay <- ",
    item_activity_delay,
    "\nitem_activity_block_id <- ",
    trj_step,
    "\n\n"
  )

env_txt = paste0(
  "\n\n## env for item activity
  env <- env %>%
    add_global('item_activity_count',0) %>%
    add_global('item_activity_waiting_secondary_unit_name',0) %>% 
    add_resource('",paste0("item_activity_",trj_step,"_res'"),",1,preemptive = TRUE,preempt_order = 'fifo')
  "
)
mod_df <- add_code_row(
  modelname=modelname,
  modeldf=mod_df,
  item =item,
  trj_step =trj_step,
  next_trj_step=next_trj_step,
  activity=activity ,
  stockpile = stockpile,
  secondary_unit_name = secondary_unit_name,
  var_txt=var_txt ,
  trj_txt=trj_txt ,
  env_txt=env_txt
)
}

