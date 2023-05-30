# notes
# Currently the item breaks down after completion of the event and releases both the activity resource and access to the stockpile
  
add_delay_with_single_stock_movement <- function(modelname,
                                                 mod_df,
                                                 item,
                                                 activity = 'get_stocks',
                                                 action='get',
                                                 trj_step = -1,
                                                 number_of_resources = '1',
                                                 item_activity_delay = 'function() max(1, rnorm(1, 5, .2))',
                                                 stockpile,
                                                 secondary_unit_name,
                                                 secondary_unit_capacity,
                                                 next_trj_step = -1) {

  if (trj_step < 0) {
    trj_step <- length(which(mod_df$item == item)) + 1
  }
  if (next_trj_step < 0) {
    next_trj_step <- trj_step + 1
  }
  # set up initial bits of code specific difference between put and get
  if (action == 'put') {
    branch_option = "function() ifelse( stockpile_max_stock - get_global(env, 'stockpile_stocks_val') < item_unit_capacity,1,2)"
    release_trap = "stock_removed_from_stockpile_signal"
    log_word = "blocked"
    anti_word = "starved"
    move_action <- "'stockpile_stocks_val', item_unit_capacity,mod='+'"
    check_secondary <- "function() ifelse(get_global(env, 'stockpile_stocks_val') < secondary_unit_name_capacity,1,2)" #send signal if false
    send_signal <- "stock_added_to_stockpile_signal"
    noprodstattext <- 's_wait_downstream_stock'
  } else if (action == 'get') {
    branch_option = "function() ifelse(get_global(env, 'stockpile_stocks_val') < item_unit_capacity,1,2)"
    release_trap = "stock_added_to_stockpile_signal"
    log_word = "starved"
    anti_word = "blocked"
    move_action <-"'stockpile_stocks_val' , function() get_global(env, 'stockpile_stocks_val')-item_unit_capacity"
    check_secondary <-"function() ifelse( stockpile_max_stock - get_global(env, 'stockpile_stocks_val') < secondary_unit_name_capacity,1,2)" #send signal if false
    send_signal <- "stock_removed_from_stockpile_signal"
    noprodstattext <- 's_wait_upstream_stock'
  }
  

trj_txt <- paste0("
",branch_if_not_activity," %>% ",
seizeresourceandpaperwork," %>% ","
      ## specific code for get_put
      set_attribute('local_item_activity_status', s_wait_stock_access) %>%
      seize('stockpile_access') %>% 
      set_attribute('local_item_activity_status', ",noprodstattext,") %>%","
      branch( option = ",branch_option,",
        continue=c(TRUE,TRUE),
        trajectory('item_activity_constrained_by_stocks') %>% 
          release('stockpile_access') %>% 
          release('item_activity_resource', 1) %>%
          ",robs_log("entering trap waiting stock movement on stockpile"),"
          trap('",release_trap,"') %>%
          wait() %>%
          ",robs_log('continuing from constrained stocks',pipe=FALSE,ret=FALSE),",
        trajectory('have_stocks_so_do_work') %>% 
          ",robs_log('adjusting stocks prior to delay'),"
           set_global(",move_action,") %>%
      ",delayandpaperwork(next_trj_step)," %>% 
        # specific code for get_put
        release('stockpile_access') %>% ",
        item_breakdown_code()," %>% 
        ",robs_log_global(text = "stockpile level = ",global = 'stockpile_stocks_val'),"
        log_(function() paste0('item:activity: secondary unit capacity = ',secondary_unit_name_capacity),level=1) %>%  
        branch( option = ",check_secondary,",
          continue = c(TRUE, TRUE),
          trajectory('item_activity_can_free_up_secondary') %>%
          ",robs_log(paste0("already ok for potentially ",anti_word," resource"),pipe=FALSE,ret=FALSE),",
          trajectory('item_activity_secondary_eq_is_ok') %>%
            ",robs_log("sending signal to free up secondary_unit_name for stockpile"),"
            send('",send_signal,"')
            
        )
      ),"
      ,skipedblock
)
var_txt <-
  paste0(
    "## Vars for item_activity\nitem_activity_delay <- ",
    item_activity_delay,
    "\nsecondary_unit_name_capacity <- ",
    secondary_unit_capacity,
    "\nitem_activity_block_id <- ",
    trj_step,
    "\n\n"
  )

env_txt = paste0(
  "\n\n## env for item activity
  env <- env %>%
    add_global('item_activity_count',0) %>%
    add_resource('item_activity_resource', ",number_of_resources,",preemptive = TRUE,preempt_order = 'fifo')
  "
)
mod_df <- add_code_row(
  modelname=modelname,
  modeldf=mod_df,
  item =item,
  trj_step =trj_step,
  activity=activity ,
  stockpile = stockpile,
  secondary_unit_name = secondary_unit_name,
  var_txt=var_txt ,
  trj_txt=trj_txt ,
  env_txt=env_txt
)
}

