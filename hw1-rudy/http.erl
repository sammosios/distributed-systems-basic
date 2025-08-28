-module(http).
-export([listen/0]).
-export([parse_request/1]).
-export([ok/1, get/1]).

listen() ->
  receive
    HttpRequest ->
      io:format("HTTP Request Received: ~p~n", [HttpRequest]),
      try parse_request(HttpRequest) of
        {Request, Headers, Body} ->
            io:format("Parsed Request Line: ~p~n", [Request]),
            io:format("Headers: ~p~n", [Headers]),
            io:format("Body: ~p~n", [Body])
      catch
        Class:Reason:Stacktrace ->
          io:format("Parsing failed: ~p ~p~nStacktrace: ~p~n",
            [Class, Reason, Stacktrace])
      end,
      listen()
  end.


parse_request(R0) ->
  {Request, R1} = request_line(R0),
  {Headers, R2} = headers(R1),
  {Body, _} = message_body(R2),
  {Request, Headers, Body}.

% Request-Line = Method SP Request-URI SP HTTP-Version CRLF
request_line("GET " ++ R0) ->
  {URI, R1} = request_uri(R0),
  {Version, R2} = http_version(R1),
  % 13,10 = CRLF
  [13,10|R3] = R2,
  {{get, URI, Version}, R3}.

request_uri([32|R0])->
  {[], R0};
request_uri([C|R0]) ->
  {Rest, R1} = request_uri(R0),
  {[C|Rest], R1}.

http_version("HTTP/1.1" ++ R0) ->
  {v11, R0};
http_version("HTTP/1.0" ++ R0) ->
  {v10, R0}.


% Headers
headers([13,10|R0]) ->
  {[],R0};
headers(R0) ->
  {Header, R1} = header(R0),
  {Rest, R2} = headers(R1),
  {[Header|Rest], R2}.

header([13,10|R0]) ->
  {[], R0};
header([C|R0]) ->
  {Rest, R1} = header(R0),
  {[C|Rest], R1}.

message_body(R) ->
  {R, []}.


%% Helpers to construct simple HTTP messages
ok(Body) ->
"HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.
get(URI) ->
"GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".