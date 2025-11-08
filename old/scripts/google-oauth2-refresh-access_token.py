#!/usr/bin/python3.11

# Copyright (c) 2020 Google LLC
# Copyright (c) 2022 Ryo ONODERA <ryo@tetera.org>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Get new access token using saved refresh token.

# Follow:
# https://googleapis.github.io/google-api-python-client/docs/oauth-installed.html

import os
import pickle
from google.auth.transport.requests import Request

# Constants
CREDENTIAL_CACHE_FILE = '.refresh_token.pickle'

## Configurations
### To identify scopes, See:
### https://developers.google.com/identity/protocols/oauth2/scopes#gmail
SCOPES=['https://mail.google.com/']

## Refresh credentials and get new ones
def get_refreshed_credentials():
    # Get home directory path and create a path for cache file
    homeDir = os.environ['HOME']
    if not homeDir:
        homeDir = '.'
    credPath = os.path.join(homeDir, CREDENTIAL_CACHE_FILE)

    if os.path.exists(credPath):
        with open(credPath, 'rb') as tokenCache:
            credentials = pickle.load(tokenCache)

        if credentials:
            # Refresh access token with refresh token
            credentials.refresh(Request())

    return credentials


if __name__ == '__main__':
    credentials = get_refreshed_credentials()

    # https://google-auth.readthedocs.io/en/stable/reference/google.oauth2.credentials.html#google.oauth2.credentials.Credentials
    print('Access Token:', credentials.token)
    #print('Expiry:', credentials.expiry) # in UTC
