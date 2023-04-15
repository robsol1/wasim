#get basic data 
resourcemon <- get_mon_resources(env) 
arrivemon <- get_mon_arrivals(env, per_resource = T)
atts <- get_mon_attributes(env) 

# log <- read.delim('code.log.txt',sep=":",col.names=c('time','item','item_type','activity','record')) %>% 
#   mutate(time=as.numeric(time),
#          hrs=time/3600)


stocks <- atts %>% 
  filter(key %in% paste0(pilenames,'.stocks'))
  


#check if inventory seems OK

stocks %>% filter(value<=10000) %>% ggplot(aes(x = time / 3600, y = value)) +
  geom_point() +
  facet_grid(rows = vars(replication),
             cols = vars(key),
             scales = "free_y")


#Check if stockpile access is ok for no orepass case

# atts %>% filter(key=='stope.stocks.access') %>%
#   ggplot(aes(x=time,y=value))+
#   geom_step()+
#   lims(x=c(51000,53000))


atts <- left_join(atts,read.csv('statuscodes.csv'))


  
status <- atts %>%
  filter(key %like% 'status' & name != "") %>% 
  arrange(replication, name, time) %>%
  group_by(replication, name) %>%
  mutate(
    item_id=as.numeric(gsub("[^0-9.-]", "", name)),
    event_end = lead(time),
    event_duration = event_end - time,
    hrs = time / 3600
  ) %>%
  filter(event_duration > 0)

  
sum_status_by_item <- status %>%
  group_by(replication,name,ID) %>% 
  summarise(n=n(),duration=sum(event_duration,na.rm=TRUE),lastevent_start=max(time)) %>% 
  mutate(mtbe=lastevent_start/n,mtoe=duration/n)

sumstat_by_Fleet <- sum_status_by_item%>% 
  mutate(fleet=ifelse(name %like% 'bogger',"Bogger","truck")) %>% 
  group_by(replication,fleet,ID) %>% 
  summarise(n=n(),duration=sum(duration,na.rm=TRUE),lastevent_start=max(lastevent_start))

check_tum_sum <- status %>% group_by(replication,name) %>% 
  summarise(n=n(),duration=sum(event_duration,na.rm=TRUE),lastevent_start=max(time)) %>% 
  mutate(mtbe=lastevent_start/n,mtoe=duration/n,
         pct_error=(duration-lastevent_start)*100/lastevent_start)
  

tonnes_moved_eq <- atts %>% 
  filter(key %like% 'cap.prod') %>% 
  group_by(replication,name,key) %>% 
  summarise(unit.activity.tput=max(value))

tonnes_moved_fleet <- tonnes_moved_eq %>% 
  mutate(fleet=ifelse(name %like% 'bogger',"Bogger","truck")) %>% 
  group_by(replication,fleet,key) %>% 
  summarise(tot_moved=sum(unit.activity.tput))


overall.tonnes <- tonnes_moved_fleet %>% 
  filter(key %like% 'put') %>% 
  group_by(fleet,key) %>% 
  summarise(n=n(),avg_tonnes=mean(tot_moved),stddev_tonnes=sd(tot_moved))
  
travel <- status %>%
  mutate( posis = ifelse(key %like% 'get', 1, ifelse(
    key %like% 'loaded', 2, ifelse(key %like% 'empty', 2, 3)
  )),
  dir=ifelse(key %like% 'loaded' | key %like% 'get' ,-1,1),
  eq=ifelse(name %like% 'truck',-1,1),
  posis=(posis-seq*dir+item_id/100 -1)*eq) %>% 
  arrange(replication,name,time) %>% 
  group_by(replication,name) %>% 
  mutate(posant=lag(posis)) %>% 
  pivot_longer(cols = starts_with('pos'),names_to = 'pos',values_to = 'location') %>% 
  arrange(replication,name,time,pos)

pltframe <- travel %>% filter(replication==1 & hrs>4 & hrs<4.2  & name %like% 'truck' & name=='truck1')
pltframe %>% 
  ggplot(aes(x=hrs,y=location,colour=name)) +
  geom_line()+
  #geom_hline(yintercept = 0.5)+
  annotate("text", x=min(pltframe$hrs), y=0.6, label="Wait Res")+
  lims(y=c(-3.5,3.5))

