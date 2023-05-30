


add_lhd <- function(modelname,
                    mod_df,
                    item,
                    activity = 'lhd',
                    # Get Vars
                    get_resources,
                    get_delay,
                    get_stockpile,
                    get_secondary_unit_name,
                    get_secondary_unit_capacity,
                    # haul loaded Vars
                    haul_loaded_resources,
                    haul_loaded_delay,
                    # put vars
                    put_resources,
                    put_delay,
                    put_stockpile,
                    put_secondary_unit_name,
                    put_secondary_unit_capacity,
                    # haul empty Vars
                    haul_empty_resources,
                    haul_empty_delay,
                    trj_step = -1,
                    next_trj_step = -1
                    ) {
  mod_df <- add_delay_with_single_stock_movement(
    modelname = modelname,
    mod_df = mod_df,
    item = item,
    activity = 'get_stocks',
    action = 'get',
    number_of_resources = get_resources,
    item_activity_delay = get_delay,
    stockpile = get_stockpile,
    secondary_unit_name = get_secondary_unit_name,
    secondary_unit_capacity = get_secondary_unit_capacity,
    trj_step = trj_step
  )
  
  mod_df <- add_activity_delay (
    modelname = modelname,
    mod_df = mod_df,
    item = item,
    activity = 'haul_loaded',
    number_of_resources = haul_loaded_resources,
    item_activity_delay = haul_loaded_delay
  )
  
  mod_df <- add_delay_with_single_stock_movement(
    modelname = modelname,
    mod_df = mod_df,
    item = item,
    activity = 'dump_stocks',
    action = 'put',
    number_of_resources = put_resources,
    item_activity_delay = put_delay,
    stockpile = put_stockpile,
    secondary_unit_name = put_secondary_unit_name,
    secondary_unit_capacity = put_secondary_unit_capacity,
    trj_step = trj_step
  )
  
  mod_df <- add_activity_delay (
    modelname = modelname,
    mod_df = mod_df,
    item = item,
    activity = 'haul_empty',
    number_of_resources = haul_empty_resources,
    item_activity_delay = haul_empty_delay,
    next_trj_step=next_trj_step
  )
  
}
