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

# Get refresh token and save it as ~/.refresh_token.pickle.
# Saved information will be used to refresh access token hourly.

# Follow:
# https://googleapis.github.io/google-api-python-client/docs/oauth-installed.html

import os
import pickle
from google_auth_oauthlib.flow import InstalledAppFlow

# Constants
CLIENT_SECRETS_FILE = 'client_secret.json'
CREDENTIAL_CACHE_FILE = '.refresh_token.pickle'

## Configurations
### To identify scopes, See:
### https://developers.google.com/identity/protocols/oauth2/scopes#gmail
SCOPES=['https://mail.google.com/']

## Get credentials
def get_credentials():
    ## Create client object
    flow = InstalledAppFlow.from_client_secrets_file(
        CLIENT_SECRETS_FILE,
        scopes=SCOPES)

    ## Open with local web browser, input credential manually
    flow.run_local_server(host='localhost',
        port=8091,
        authorization_prompt_message='Please visit this URI: {url}',
        success_message='The auth flow is complete; you may close this web browser window.',
        open_browser=True)

    return flow.credentials


if __name__ == '__main__':
    credentials = get_credentials()

    # Get home directory path and create a path for cache file
    homeDir = os.environ['HOME']
    if not homeDir:
        homeDir = '.'
    credPath = os.path.join(homeDir, CREDENTIAL_CACHE_FILE)

    # Save credentials as file
    with open(credPath, 'wb') as tokenCache:
        pickle.dump(credentials, tokenCache)

    # https://google-auth.readthedocs.io/en/stable/reference/google.oauth2.credentials.html#google.oauth2.credentials.Credentials
    print('Refresh Token:', credentials.refresh_token)
