# POAuth2

Simple OAuth2 client for Pascal.

## License

MIT

## Compatibility

OS
: Windows, Linux

Compiler
: Delphi, Free Pascal

## Limitations/Features

* Supported GrantTypes: user credentials, authorization code, client credentials
* Supported token types: Bearer, MAC (untested)
* Supports HTTP GET and POST.
* Server must return JSON.
* Refresh token must be returned with the access token.

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
    client.GrantType := gtPassword;
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
      TokenType := 'Bearer';
    end;
    client.Site := 'http://localhost';
    client.GrantType := gtPassword;
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

To compile the Lazarus demo (`laz` subdirectory) you need Indy 10 for Lazarus, 
see [here](http://wiki.freepascal.org/Indy_with_Lazarus), download from [here](http://www.indyproject.org/Sockets/fpc/index.de.aspx).
If you can't use Indy for some reason, you must implement a `TOAuth2HttpClient` descendant which
implements the methods `Get` and `Post`.

### HTTPS

The Indy client (`TIndyHttpClient`) supports it, but you must copy the file `libeay32.dll`
and `ssleay32.dll` from the [OpenSLL](https://www.openssl.org/) distribution to the application directory.
If you compile a 64 Bit executeable you need the 64 Bit libraries. You can download OpenSSL binaries for 
Windows [here](https://indy.fulgan.com/SSL/).
Depending on the version of OpenSSL you may get errors. Version 1.0.2a seems to work
fine, later versions may have some functions removed.

A quick solution for this problem would be to replace:

~~~pascal
@IdSslMethodV2 := LoadFunction(fn_SSLv2_method);
@IdSslMethodServerV2 := LoadFunction(fn_SSLv2_server_method);
@IdSslMethodClientV2 := LoadFunction(fn_SSLv2_client_method);
~~~

with:

~~~pascal
@IdSslMethodV2 := LoadFunction(fn_SSLv2_method, false);
@IdSslMethodServerV2 := LoadFunction(fn_SSLv2_server_method, false);
@IdSslMethodClientV2 := LoadFunction(fn_SSLv2_client_method, false);
~~~

in `IdSSLOpenSSLHeaders.pas`.

If it still failes, try also making these non-critical:

~~~pascal
@IdSslMethodV3 := LoadFunction(fn_SSLv3_method, false);
@IdSslMethodServerV3 := LoadFunction(fn_SSLv3_server_method, false);
@IdSslMethodClientV3 := LoadFunction(fn_SSLv3_client_method, false);
~~~

## Server 

I used [oauth2-server-php](https://github.com/bshaffer/oauth2-server-php) as Server.
To extend the Resource controller to accept HTTP POST add a new route:

~~~iphp
class Resource
{
    // Connects the routes in Silex
    public static function addRoutes($routing)
    {
      $routing->get('/resource', array(new self(), 'resource'))->bind('access');
      // Add a POST route
      $routing->post('/resource', array(new self(), 'resourceFormSubmit'))->bind('resource_post');
    }

    // The new POST handler
    public function resourceFormSubmit(Application $app)
    {
        // get the oauth server (configured in src/OAuth2Demo/Server/Server.php)
        $server = $app['oauth_server'];

        // get the oauth response (configured in src/OAuth2Demo/Server/Server.php)
        $response = $app['oauth_response'];

        if (!$server->verifyResourceRequest($app['request'], $response)) {
            return $server->getResponse();
        } else {
            // return a fake API response - not that exciting
            // @TODO return something more valuable, like the name of the logged in user
            $api_response = array(
                'friends' => array(
                    'john',
                    'matt',
                    'jane'                    
                ),
                'get' => $app['request']->query->all(),
                'post' => $app['request']->request->all()
            );
            return new Response(json_encode($api_response));
        }
    }
    
    // The existing GET handler
    public function resource(Application $app)
    {
        // get the oauth server (configured in src/OAuth2Demo/Server/Server.php)
        $server = $app['oauth_server'];

        // get the oauth response (configured in src/OAuth2Demo/Server/Server.php)
        $response = $app['oauth_response'];

        if (!$server->verifyResourceRequest($app['request'], $response)) {
            return $server->getResponse();
        } else {
            // return a fake API response - not that exciting
            // @TODO return something more valuable, like the name of the logged in user
            $api_response = array(
                'friends' => array(
                    'john',
                    'matt',
                    'jane'                    
                ),
                'get' => $app['request']->query->all()
            );
            return new Response(json_encode($api_response));
        }
    }
}   
~~~

### JSON required

To make the resource controller correctly return JSON:

~~~iphp 
public function resource(Application $app)
{
    // get the oauth server (configured in src/OAuth2Demo/Server/Server.php)
    $server = $app['oauth_server'];

    // get the oauth response (configured in src/OAuth2Demo/Server/Server.php)
    $response = $app['oauth_response'];

    if (!$server->verifyResourceRequest($app['request'], $response)) {
        return $server->getResponse();
    } else {
        // return a fake API response - not that exciting
        // @TODO return something more valuable, like the name of the logged in user
        $api_response = array(
            'friends' => array(
                'john',
                'matt',
                'jane'                    
            ),
            'get' => $app['request']->query->all()
        );
        // Add application/json as Content-Type header
        return new Response(json_encode($api_response), 200, array('Content-Type' => 'application/json'));
    }
}
~~~

## Credits

* JSON parser: [https://github.com/koldev/JsonParser](https://github.com/koldev/JsonParser)

## Get it

* [GitHub](https://github.com/stievie/POAuth2)
* [Lazarus Win32 Demo binary](https://0x2a.wtf/files/poat.zip)
