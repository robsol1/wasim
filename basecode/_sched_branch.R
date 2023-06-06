
add_schedule_branch <- function(modelname,
                            mod_df,
                            item,
                            activity = 'sched_branch',
                            trj_step = -1,
                            sched_name = "sched",
                            next_trj_step = c(1, 2, 3),
                            continue = c(TRUE, FALSE, TRUE)){
  # Create next block id if not defined
  if (trj_step < 0) {
    trj_step <- length(which(mod_df$item == item)) + 1
  }
  # Create Continue Text
  continuetext <- "continue=c("
  for (i in 1:(length(next_trj_step) - 1)) {
    continuetext = paste0(continuetext, continue[i], ',')
  }
  continuetext = paste0(continuetext, continue[length(next_trj_step)], '),')
  subtrj_txt=""
  for (i in 1:(length(next_trj_step) - 1)) {
    subtrj_txt <- paste0(subtrj_txt,
    "\t\ttrajectory('",sched_name,"_trajectory_",i ,"') %>% 
    \t\t\t",robs_log(paste0('Entering branch trajectory ',i),ret=FALSE,pipe=TRUE),"
    \t\t\tset_attribute('item_next_block',",trj_step+ next_trj_step[i],"),
    ")  }
    subtrj_txt <- paste0(subtrj_txt,
    "\t\ttrajectory('",sched_name,"_rajectory_",length(next_trj_step),"') %>% 
    \t\t\t",robs_log(paste0('Entering branch trajectory ',length(next_trj_step)),ret=FALSE,pipe=TRUE),"
    \t\t\tset_attribute('item_next_block',", trj_step+next_trj_step[length(next_trj_step)],")
    \t) %>% 
    ")
    
  
  trj_txt <- paste0("
  LHD_trj <- LHD_trj %>% 
    branch(option = function()ifelse(get_attribute(env, 'item_next_block') == item_activity_block_id,1,2),
           continue = c(TRUE, TRUE),
           trajectory('item_activity_stay_in_block') %>% 
             ",robs_log('Entering branch decision',ret=FALSE,pipe=TRUE),"
           branch( option = function() scheduler('",sched_name,"','item:activity:'),
                    ",continuetext,"\n",subtrj_txt,
                     
"
\ttrajectory('item_activity_skip_this_block') %>% 
\t\t",robs_log('Block id is not next block so skip block',ret=FALSE,pipe=FALSE),"
    ) %>% 
    ",robs_log('End and go to next block',ret=FALSE,pipe=FALSE),"
")
  next_trj_step =next_trj_step+trj_step
  var_txt <- ""
  env_txt <-  ""
  
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
                            
 


