# POAuth2

OAuth2 client for Pascal.

## Usage

~~~pascal
var
  client: TOAuth2Client;
  res: TOAuth2Response;
begin
  client := TOAuth2Client.Create(TIndyHttpClient.Create);
  try
    client.Site := 'localhost';
    client.UserName := 'testuser';
    client.PassWord := 'testpass';
    client.ClientId := 'testclient';
    client.ClientSecret := 'testsecret';
    resp := client.GetReosurce('/path/to/resource?name=value');
  finally
    client.Free;
  end;
end;
~~~
