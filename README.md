# POAuth2

Simple OAuth2 client for Pascal.

## License

MIT

## Usage

~~~pascal
var
  client: TOAuth2Client;
  res: TOAuth2Response;
  cli: TIndyHttpClient;
begin
  cli := TIndyHttpClient.Create;
  client := TOAuth2Client.Create(cli);
  try
    client.Site := 'localhost';
    client.GrantType := 'password';   // Only password is supported
    client.UserName := 'testuser';
    client.PassWord := 'testpass';
    client.ClientId := 'testclient';
    client.ClientSecret := 'testsecret';
    resp := client.GetReosurce('/path/to/resource?name=value');
  finally
    client.Free;
    cli.Free;
  end;
end;
~~~

### If you have an AccessToken

~~~pascal
var
  client: TOAuth2Client;
  res: TOAuth2Response;
  cli: TIndyHttpClient;
begin
  cli := TIndyHttpClient.Create;
  client := TOAuth2Client.Create(cli);
  try
    client.AccessToken := TOAuth2Token.Create;   // Freed by client
    with client.AccessToken do begin
      TokenType := 'Bearer';
      AccessToken := '486b9c03a389e6e747f19cf202afbb026036cf9a';
      RefreshToken := '13664cc7c4b89e03fea2c4ab140ca546461f71cf';
      ExpiresIn := 3600;
      // Set after ExpiresIn overwrites calculated value
      ExpiresAt := 42442.406256875
    end;
    client.Site := 'localhost';
    client.GrantType := 'password';   // Only password is supported
    client.UserName := 'testuser';
    client.PassWord := 'testpass';
    client.ClientId := 'testclient';
    client.ClientSecret := 'testsecret';
    resp := client.GetReosurce('/path/to/resource?name=value');
  finally
    client.Free;
    cli.Free;
  end;
end;
~~~

## Limitations

* Only the password GrantType (i.e. user credentials) is supported.
* Server must return JSON
* Refresh token must be returned with the access token

## Credits

* JSON parser: [https://github.com/koldev/JsonParser](https://github.com/koldev/JsonParser)

## Get it

* [GitHub](https://github.com/stievie/POAuth2)
