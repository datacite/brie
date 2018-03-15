
# coding: utf-8

# In[7]:

# from urllib2 import Request, urlopen
import urllib.request
import json
from pandas.io.json import json_normalize

path1 = '42.974049,-81.205203|42.974298,-81.195755'
rows=100
request=Request('http://query.eventdata.crossref.org/events?rows='+rows+'&filter=source:crossref')
response = urlopen(request)
data = json.loads(response.read())
json_normalize(data['message']['events'])


# In[ ]:



