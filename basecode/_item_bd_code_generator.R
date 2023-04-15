item_breakdown_code <- function(){
  code <- "
              ### This is generic BD type code ..
              log_('item:activity :test for item BD', level = 1) %>%
              log_(function() {paste0('item:activity :ute time = ',get_attribute(env, 'item_ute_time'))}, level = 1) %>%
              log_(function() {paste0('item:activity :Next BD  = ',get_attribute(env, 'item_next_bd'))}, level = 1) %>%
              branch(
                #breakdown
                option = function() ifelse(get_attribute(env, 'item_ute_time') >= get_attribute(env, 'item_next_bd'),1,2),
                continue = c(TRUE, TRUE),
                trajectory('broken_down') %>%
                  set_attribute('local_item_activity_status', s_breakdown) %>% 
                  log_('item : activity : status= s,breakdown',level=1) %>% 
                  set_attribute('bd_delay', item_mttr) %>%
                  timeout_from_attribute('bd_delay') %>%
                  log_('item:activity :continuing! after breakdown', level = 1) %>% 
                  set_attribute('item_next_bd', item_mtbf, mod='+') %>% 
                  log_(function() {paste0('item:activity :update next bd   ',get_attribute(env, 'item_next_bd'))}, level = 1),
                trajectory('working') %>%
                  log_('item:activity :not broken down', level = 1)
              )"
}
