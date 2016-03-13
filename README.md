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

## Limitations

* Only the password GrantType (i.e. user credentials) is supported.
* Server must return JSON
* Refresh token must be returned with the access token

## Credits

* JSON parser: [https://github.com/koldev/JsonParser](https://github.com/koldev/JsonParser)

## Get it

* [GitHub](https://github.com/stievie/POAuth2)
