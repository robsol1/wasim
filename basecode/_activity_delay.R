branch_if_not_activity <- "
## branch_if_not_activity start
item_trj <- item_trj %>%
  branch(
    option = function()ifelse(get_attribute(env, 'item_next_block') == item_activity_block_id,1,2),
    continue = c(TRUE, TRUE),
    trajectory('item_activity_stay_in_block')"

seizeresourceandpaperwork <- "
      ## seizeresourceandpaperwork start
      set_attribute('item_activity_success', 0) %>%
      set_attribute('local_item_activity_status', s_wait_res) %>%
      seize('item_activity_resource', 1)"
delayandpaperwork <- function(next_trj_step){
  paste0("
      ## delayandpaperwork start
      set_attribute('local_item_activity_status', s_working) %>%
      set_attribute('item_activity_delay_att', item_activity_delay) %>%
      timeout_from_attribute('item_activity_delay_att') %>%
      set_attribute('item_activity_success', 1) %>%
      set_attribute('item_ute_time', function() get_attribute(env, 'item_activity_delay_att'), mod = '+') %>%
      set_attribute('item_activity_cap_prod', item_unit_capacity, mod = '+') %>%
      release('item_activity_resource', 1) %>%
      branch(option = function() ifelse(get_attribute(env, 'item_next_block') < last_block_in_item_trj,1,2),
      continue = c(TRUE, TRUE),
      trajectory('item_activity_set_up_for_next_block') %>%
        set_attribute('item_next_block',", next_trj_step,"),
      trajectory('item_activity_set_up_for_start') %>%
        set_attribute('item_next_block', 2)
      )")
}

checkatendcode <- "
      ## checkatendcode start
      #release('item_activity_resource', 1) %>%
      branch(option = function() ifelse(get_attribute(env, 'item_activity_success') == 1, 1, 2),
             continue = c(TRUE, TRUE),
             trajectory('item_activity_sucess_in_this_block') %>%
               branch(option = function() ifelse(get_attribute(env, 'item_next_block') < last_block_in_item_trj,1,2),
                      continue = c(TRUE, TRUE),
                      trajectory('item_activity_set_up_for_next_block') %>%
                        set_attribute('item_next_block', 1, mod = '+'),
                      trajectory('item_activity_set_up_for_start') %>%
                        set_attribute('item_next_block', 2)
               ),
             trajectory('item_failed_so_skip_blocks') %>% 
               log_('some stuff')
      )

  ## checkatendcode end"

skipedblock <- "
    #skipedblock start 
    trajectory('item_activity_skip_this_block') %>%
      log_('item:activity:Block id is not next block so skip block',level=1)
  )%>% 
  log_('item:activity:End and go to next block',level=1)
  #skipedblock end"


add_activity_delay <- function(modelname,
                               mod_df,
                               item,
                               activity = 'activity_delay',
                               trj_step = -1,
                               number_of_resources = '1',
                               item_activity_delay = 'function() max(1, rnorm(1, 5, .2))',
                               next_trj_step=-1) {
  if (trj_step < 0) {
    trj_step <- length(which(mod_df$item == item)) + 1
  }
  if (next_trj_step < 0) {
    next_trj_step <- trj_step + 1
  }
  var_txt <- paste0(
    "\nitem_activity_block_id <- ",
    trj_step,
    "\nitem_activity_delay <- ",
    item_activity_delay
  )
  trj_txt <-   paste0(
    branch_if_not_activity," %>% ",
    seizeresourceandpaperwork, " %>% ",
    delayandpaperwork(next_trj_step)," %>% ",
    item_breakdown_code(),",",
    #checkatendcode,
   # "\n,",
    skipedblock
  )
  
  env_txt = paste0(
    "\n\n## env for item activity\nenv <- env %>%\nadd_resource('item_activity_resource', ",
    number_of_resources,
    ",preemptive = TRUE,preempt_order = 'fifo')\n"
  )
  mod_df <- add_code_row(
    modelname=modelname,
    modeldf=mod_df,
    item =item,
    trj_step=trj_step ,
    next_trj_step=next_trj_step,
    activit=activity ,
    var_txt=var_txt ,
    trj_txt=trj_txt ,
    env_txt=env_txt
  )
}
