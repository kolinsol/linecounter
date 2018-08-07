{
  application, linecounter,
  [
    {description, "line countin utility"},
    {vsn, "0.1.0"},
    {modules, [lc_app, lc_sup, lc_file_store]},
    {registered, [lc_sup]},
    {application, [kernel, stdlib]},
    {mod, {lc_app, []}}
  ]
}.
