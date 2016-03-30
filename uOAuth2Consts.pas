{
  Simple OAuth2 client

  (C) 2016, Stefan Ascher
}

unit uOAuth2Consts;

{$IFDEF FPC}
  {$mode objfpc}
  {$H+}
{$ENDIF}

interface

const
  OAUTH2_TOKEN_ENDPOINT = '/token';

  OATUH2_ACCESS_TOKEN = 'access_token';
  OAUTH2_GRANT_TYPE = 'grant_type';
  OAUTH2_REFRESH_TOKEN = 'refresh_token';
  OAUTH2_EXPIRES_IN = 'expires_in';
  OAUTH2_MACKEY = 'mac_key';
  OAUTH2_MAXALGORITHM = 'mac_algorithm';
  OAUTH2_CLIENT_ID = 'client_id';
  OAUTH2_CLIENT_SECRET = 'client_secret';
  OAUTH2_TOKEN_TYPE = 'token_type';
  OAUTH2_USERNAME = 'username';
  OAUTH2_PASSWORD = 'password';
  OAUTH2_SCOPE = 'scope';
  OAUTH2_REDIRECT_URI = 'redirect_uri';
  OAUTH2_CODE = 'code';
  OAUTH2_TOKENTYPE_BEARER = 'Bearer';
  OAUTH2_TOKENTYPE_MAC = 'MAC';
  OATUH2_BASIC = 'Basic';
  OAUTH2_AUTHORIZATION = 'Authorization';

  CONTENTTYPTE_JSON = 'application/json';
  CONTENTTYPTE_XML = 'application/xml';
  CONTENTTYPTE_URL_ENCODED = 'application/x-www-form-urlencoded';

  HTTP_OK = 200;
  HTTP_UNAUTHORIZED = 401;
  HTTP_FORBIDDEN = 403;

implementation

end.
