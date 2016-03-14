{
  Simple OAuth2 client

  (C) 2016, Stefan Ascher
}

unit uOAuth2Config;

{$IFDEF FPC}
  {$mode objfpc}
  {$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, uOAuth2Consts;

type
  TOAuth2Config = record
    TokenEndPoint: string;
  end;

const
  DEF_OATUH2_CONFIG: TOAuth2Config = (
    TokenEndPoint: OAUTH2_TOKEN_ENDPOINT
  );

implementation

end.

