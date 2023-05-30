add_decision_branch <-
  function(modelname,
           mod_df,
           item,
           activity,
           decision_code,
           block_if_true,
           block_if_false,
           trj_step) {
    if (trj_step < 0) {
      trj_step <- length(which(mod_df$item == item)) + 1
    }
    var_txt <- paste0(
      "\nitem_activity_block_id <- ",
      trj_step
    )
    code <- paste0("branch(option = function() ",str,",1,2),
      continue = c(TRUE, TRUE),
      trajectory('bogger_get_stocks_set_up_for_next_block') %>%
        set_attribute('bogger_next_block',",block_if_true,"),
      trajectory('bogger_get_stocks_set_up_for_start') %>%
        set_attribute('bogger_next_block',",block_if_false,")
                   ),"
    )
    
    trj_txt <-   paste0(branch_if_not_activity, " %>% \n",code,"\n",
                        skipedblock)
    env_txt = ""
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






