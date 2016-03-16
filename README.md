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
    client.Site := 'http://localhost';
    client.GrantType := gtPassword;   // Only password is supported
    client.UserName := 'testuser';
    client.PassWord := 'testpass';
    client.ClientId := 'testclient';
    client.ClientSecret := 'testsecret';
    resp := client.Get('/path/to/resource?name=value');
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
    client.Site := 'http://localhost';
    client.GrantType := gtPassword;   // Only password is supported
    client.UserName := 'testuser';
    client.PassWord := 'testpass';
    client.ClientId := 'testclient';
    client.ClientSecret := 'testsecret';
    resp := client.Get('/path/to/resource?name=value');
  finally
    client.Free;
    cli.Free;
  end;
end;
~~~

### Lazarus

To compile the demo you need Indy 10 for Lazarus, see [here](http://wiki.freepascal.org/Indy_with_Lazarus).
If you can't use Indy you must implement a `TOAuth2HttpClient` descendant which
implements the methods `Get` and `Post`.

## Limitations

* Only the password GrantType (i.e. user credentials) is supported.
* Server must return JSON
* Refresh token must be returned with the access token

## Credits

* JSON parser: [https://github.com/koldev/JsonParser](https://github.com/koldev/JsonParser)

## Get it

* [GitHub](https://github.com/stievie/POAuth2)
