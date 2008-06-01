{application, wrsd, [
    {description, "An Erlang World of Warcraft realm status daemon"},
    {vsn, "0.1"},
    {modules, [
        wrsd,
        wrsd_handler,
        wrsd_realm,
        wrsd_realmserver,
        wrsd_sup,
        wrsd_yaws
    ]},
    {registered, [wrsd]},
    {applications, [kernel, stdlib, sasl]},
    {mod, {wrsd, []}},
    {start_phases, [
        {update_loop, []}
    ]}
]}.
