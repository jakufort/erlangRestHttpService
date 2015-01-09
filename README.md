erlandRestHttpService
=====================

HTTP server with REST API to store measures written in erlang

yaws.conf should be modified like this to work (of course you should modify paths to your environment):

ebin_dir = /home/fortun/git/erlangRestHttpService/out/production/erlangRestHttpService

(inside <server...>)
appmods = <api,httpRestService>


