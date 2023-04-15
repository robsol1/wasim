
env <- simmer()
df <- data.frame(x=c(1:100),y=c(1:100))
simplefn <- function(now,xl,yl){
  cat("time=",now," result = ",df$x[xl]+df$y[yl], ' and now from log ')
  df$x[xl]+df$y[yl]
}

data.env <- new.env()
assign('global.df',data.frame(x=c(1:200),y=c(1:200)),data.env)
check.global.array <- function(row,col,val){
 df <- get('global.df',envir = data.env)
 df[row,col] =2*33
 assign('global.df',df,envir = data.env)
 df[row,col]
}
t <- check.global.array(2,2)

trj <- trajectory() %>% 
  log_('into trajectory') %>% 
  log_(function() as.character(simplefn(now(env),1,2))) %>% 
  set_global('basic',simplefn(now(env),1,1)) %>% 
  log_(function() paste0('global val set from simple fn shoild be 2 :  ',get_global(env, 'basic'))) %>% 
  set_global('basic',function() simplefn(now(env),get_global(env,'basic'),2)) %>%
  log_(function() paste0('global set from simple and global call should be 4 ',get_global(env, 'basic'))) %>%
  set_attribute('new',function() get_global(env,'basic')) %>% 
  log_(function() {paste0('local direct from global should be 4  ',get_attribute(env,'new'))}) %>% 
  set_attribute('new',function() simplefn(now(env),3,3)) %>% 
  log_(function() {paste0('local attribute from simple function should be 6 ',get_attribute(env,'new'))}) %>% 
  set_attribute('new', function() simplefn(now(env),get_global(env,'basic'),6)) %>% 
  log_(function() {paste0('local attribute from function global should be 10  ',get_attribute(env,'new'))}) %>% 
  
  ## testing global array with a multiple argument call
  set_attribute('new', function() check.global.array(get_global(env,'basic'),1,2)) %>% 
  log_(function() {paste0('local attribute from function global should be 66  ',get_attribute(env,'new'))}) %>% 
  log_('end')

env %>% 
  add_global('basic',1) %>% 
  add_generator('item',trajectory = trj,at(0))

sink(file='./code.log.txt')
env %>% run(20)
sink()  ## close it!
file.show('./code.log.txt')

