let () =
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun req ->
             Dream.from_filesystem "public" "index.html" req);
         Dream.get "/**" (Dream.static "public");
       ]
