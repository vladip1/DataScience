#!/usr/bin/env python
# coding: utf-8

# In[94]:


#import os.path

#!/usr/bin/python
import time;  # This is required to include time module.
import random;
sleep_period = 5
    
while(True):
    ticks = round(time.time())
    file = open('testfile.txt','w') 
    file.write('# HELP dp_inbound_kbytes_total The total number of HTTP requests.\n')
    file.write('# TYPE dp_inbound_kbytes_total counter\n')

    for i in range(10):
        file.write("dp_inbound_kbytes_total{policy=\"Best\",destination=\"%s\"} %d %d\n"%(i, (ticks + random.randint(0,sleep_period-1)) % 10000000,ticks)) 

    file.close() 

    file1 = open('testfile.txt','r') 
    
    print(file1.readlines())
    
    file1.close()
    
    time.sleep(sleep_period)
    
