$local = $pwd
write-host "serving files from $local"

$http = [System.Net.HttpListener]::new()
# Hostname and port to listen on
$http.Prefixes.Add("http://localhost:8080/")
# Start the Http Server
$http.Start()

Add-Type -AssemblyName System.Web
# Log ready message to terminal
if ($http.IsListening) {
    write-host "HTTP Server Ready!  " -f 'black' -b 'gre'
    write-host "$($http.Prefixes)" -f 'y'
}



# INFINTE LOOP
# Used to listen for requests
while ($http.IsListening) {
    # Get Request Url
    # When a request is made in a web browser the GetContext() method will return a request object
    # Our route examples below will use the request object properties to decide how to respond
    $context = $http.GetContext()

    if ($context.Request.HttpMethod -eq 'GET') {

        # We can log the request to the terminal
        write-host "$($context.Request.UserHostAddress)  =>  $($context.Request.Url)" -f 'mag'


        $URL = $context.Request.Url.LocalPath

        # Redirect root to index.html
        if($URL -eq "/") {
          $URL = "/index.html"
        }

        $ContentStream = [System.IO.File]::OpenRead( "$local/$URL" );
        $Context.Response.ContentType = [System.Web.MimeMapping]::GetMimeMapping("$local/$URL")
        $ContentStream.CopyTo( $Context.Response.OutputStream );
        $Context.Response.Close()
    }
    # powershell will continue looping and listen for new requests...
}
