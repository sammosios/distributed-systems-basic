-module(rudy_improved).
-export([init/1]).
-export([start/1, stop/0]).
-define(WEBROOT, "www").

%% Server start/stop functions
start(Port) ->
  register(rudy_improved, spawn(fun() -> init(Port) end)).
stop() ->
  exit(whereis(rudy_improved), "time to die").

init(Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->
      handler(Listen),
      gen_tcp:close(Listen),
      ok;
    {error, Error} ->
      io:format("TCP listen error: ~w~n", [Error]),
      {error, Error}
  end.

handler(Listen) ->
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      %% Handles each client in a separate process, continues accepting new clients
      spawn(fun() -> request(Client) end),
      handler(Listen);
    {error, Error} ->
      io:format("rudy: error accepting TCP: ~w~n", [Error]),
      {error, Error}
  end.

request(Client) ->
  Recv = gen_tcp:recv(Client, 0),
  case Recv of
    {ok, Str} ->
      Request = http:parse_request(Str),
      Response = reply(Request),
      gen_tcp:send(Client, Response);
    {error, closed} ->
        ok;  % normal case, no need to log
    {error, Reason} ->
      io:format("request: error: ~p~n", [Reason])
  end,
  gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
    timer:sleep(40),
    Webroot = ?WEBROOT,
    case URI of
        %% Default to index.html for root requests
        "/" ->
            FullPath = filename:join([Webroot, "index.html"]),
            serve_file(FullPath);
        %% Returns a dynamic list of files in the webroot directory
        "/files" ->
            %% Read the webroot directory contents
            case file:list_dir(Webroot) of
                {ok, Files} ->
                    %% Generate HTML list items for each file
                    FileLinks = lists:map(fun(F) -> 
                        "<li><a href=\"/" ++ F ++ "\">" ++ F ++ "</a></li>"
                    end, Files),
                    FileLinksHtml = lists:flatten(FileLinks),
                    
                    %% Read the files.html template
                    TemplatePath = "files.html",
                    case file:read_file(TemplatePath) of
                    {ok, TemplateContent} ->
                      TemplateStr = lists:flatten(binary_to_list(TemplateContent)),
                      %% Inject the file list into the HTML, replacing the placeholder
                      Html = string:replace(TemplateStr, "<!-- FILE_LIST_PLACEHOLDER -->", FileLinksHtml, all),
                      http:ok(Html);
                    {error, Reason} ->
                      ErrorMsg = io_lib:format("Could not read template ~s: ~p", [TemplatePath, Reason]),
                      http:error(lists:flatten(ErrorMsg))
                    end;

                    {error, Reason} ->
                      ErrorMsg = io_lib:format("Could not list directory: ~p", [Reason]),
                      http:error(lists:flatten(ErrorMsg))
            end;

        File when is_list(File) ->
            %% Remove leading '/' from the URI, otherwise server will look in the root directory
            CleanFile = case File of
                [$\/ | Rest] -> Rest;
                _ -> File
            end,
            FullPath = filename:join([Webroot, CleanFile]),
            serve_file(FullPath);

        _ ->
            http:ok("All good mate! You requested " ++ URI)
    end.

serve_file(File) ->
    %% Read the file from disk and serve it with appropriate headers
    case file:read_file(File) of
        {ok, Content} ->
            ExtraHeaders = [
                {"Content-Type", mime_type(File)},
                {"X-Powered-By", "Rudy v0.1"},
                {"Cache-Control", "no-cache"},
                {"Connection", "close"}
            ],
            http:ok(Content, ExtraHeaders);
        {error, enoent} ->
            http:not_found("File not found: " ++ File);
        {error, eacces} ->
            http:no_access("Access forbidden to file: " ++ File);
        {error, Reason} ->
            ErrorMsg = io_lib:format("Could not read file ~s: ~p", [File, Reason]),
            http:error(lists:flatten(ErrorMsg))
    end.

%% Returns the MIME type for a given file extension
mime_type(File) ->
    case filename:extension(File) of
        ".html" -> "text/html; charset=UTF-8";
        ".htm"  -> "text/html; charset=UTF-8";
        ".css"  -> "text/css; charset=UTF-8";
        ".erl"  -> "text/plain; charset=UTF-8";
        ".txt"  -> "text/plain; charset=UTF-8";
        ".js"   -> "application/javascript";
        ".json" -> "application/json";
        ".yaml" -> "application/x-yaml";
        ".yml"  -> "application/x-yaml";
        ".xml"  -> "application/xml";
        ".pdf"  -> "application/pdf";
        ".zip"  -> "application/zip";
        ".png"  -> "image/png";
        ".jpg"  -> "image/jpeg";
        ".jpeg" -> "image/jpeg";
        ".gif"  -> "image/gif";
        ".svg"  -> "image/svg+xml";
        ".mp3"  -> "audio/mpeg";
        ".mp4"  -> "video/mp4";
        _       -> "application/octet-stream"
    end.
