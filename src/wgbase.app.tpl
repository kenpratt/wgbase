{application, wgbase
    [{description, "the wg base libarary"},
     {vsn, "@APP_VSN@"},
     {modules, [@MODULES@]},
     {registered, []},
     {applications, [kernel, stdlib, sasl, inets]},
     {mod, {wg_app, []}},
     {env, []}
    ]
}.
