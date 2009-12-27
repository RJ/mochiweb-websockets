{application, websocket,
 [{description, "websocket"},
  {vsn, "0.01"},
  {modules, [
    websocket,
    websocket_app,
    websocket_sup,
    websocket_web,
    websocket_deps
  ]},
  {registered, []},
  {mod, {websocket_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
