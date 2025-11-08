#!/bin/sh
 
python3 ~/Mail/oauth2/google-oauth2-refresh-access_token.py | awk -F" " '{if(NR==1)print $3}'
