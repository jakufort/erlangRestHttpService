erlangRestHttpService
=====================

HTTP server with REST API to store measures written in erlang

yaws.conf should be modified like this to work (of course you should modify paths to your environment):

ebin_dir = /var/git/httpRestService/ebin
ebin_dir = /var/git/httpRestService/lib/ebin/jiffy

In server configuration (\<server name\>) have to be line:
appmods = \<pathName,httpRestService\>

(service will be available at adres: http://name/pathName/measures )

Operations:

**GET** 
  - for "measures" - returns list of measurers
  - for "measures/measurerName" - returns measures of this measurer (it's possible to do this: "measures/measurerName?limit=X (ie. limit=10) to return last X of measures)

**POST**
  - for "measures" it allows to add new measurer to database. Fields: 
    * *name* - administrator login
    * *password* - administrator password
    * *measurer* - measurer name
    * *measurerPassword* - measurer password

**PUT**
  - for "measures" it allows to replace all measurers with new ones (using JSON)
  - for "measures/measurerName" allows to add new measure; can be used "application/x-www-form-urlencoded" or "application/json". For both fields are:
    * *password* - measurer password
    * *timestamp* - timestamp of measure
    * *value* - measurement 

**DELETE**
  - for "measures" deletes all of the measurers
  - for "measures/measurerName" it deletes all of the measures of this measurer and also this measurer from database

**Example JSON for PUT (replacing all measurers)**

```
{
    "measurers": [
        {
            "name": "m1",
            "password": "m1",
            "measures": [
                {
                    "timestamp": 123,
                    "value": 13.0
                },
                {
                    "timestamp": 141,
                    "value": 41.0
                }
            ]
        },
        {
            "name": "m2",
            "password": "m2",
            "measures": [
                {
                    "timestamp": 134,
                    "value": 14.0
                }
            ]
        }
    ]
}
```



