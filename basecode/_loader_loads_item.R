# notes
# Currently the item breaks down after completion of the event and releases both the activity resource and access to the stockpile

add_loader_loads_item<- function(modelname,
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
item_trj <- trajectory() %>% 
  branch(option = function()ifelse(get_attribute(env, 'item_next_block') == item_activity_block_id,1,2),
         continue = c(TRUE, TRUE),
         trajectory('item_activity_stay_in_block') %>% 
           set_attribute('item_activity_success', 0) %>%
           set_attribute('local_item_activity_status', s_wait_stock_access) %>%
           seize('stockpile_access') %>% 
           branch( option = function() ifelse(get_global(env, 'stockpile_stocks_val') > item_unit_capacity,1,2),
                   continue=c(TRUE,TRUE),
                   trajectory('item_activity_have_stocks') %>% 
                     branch( option = function() ifelse(get_global(env, 'item_activity_waiting_load_item_name') > 0,1,2),
                             continue=c(TRUE,TRUE),
                             trajectory('item_activity_truck_needs_loading') %>% 
                               set_attribute('local_item_activity_status', s_working) %>%
                               send('item_name_available') %>%  # send signal to move truck into loading state
                               set_attribute('item_activity_haul_loaded_status', s_working) %>%
                               set_attribute('item_activity_loaded_delay_att', secondary_unit_name_activity_delay) %>%
                               timeout_from_attribute('item_activity_loaded_delay_att') %>%
                               set_attribute('item_activity_loaded_success', 1) %>%
                               set_attribute('item_ute_time', function() get_attribute(env, 'item_delay_att'), mod = '+') %>%
                               set_attribute('item_activity_loaded_cap_prod', secondary_unit_name_unit_capacity, mod = '+') %>% 
                               release('stockpile_access') %>% ",
                               item_breakdown_code(),", 
                             trajectory('item_activity_no_truck_required') %>% 
                               ",robs_log('no_truck_required',ret=FALSE,pipe=FALSE),"
                     ),
                   trajectory('item_activity_insufficient_stocks') %>% 
                     ",robs_log('insufficient stocks',ret=FALSE,pipe=FALSE),"
           )
           ,
         trajectory('item_activity_skip_this_block') %>% 
           ",robs_log('Block id is not next block so skip block',ret=FALSE,pipe=FALSE),"
  ) %>% 
  ",robs_log('End and go to next block',ret=FALSE,pipe=FALSE),"
")
           
save_text_to_file(trj_txt,"temp2.R")       
         


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

