{application, wrsd, [
    {description, "An Erlang World of Warcraft realm status daemon"},
    {vsn, "0.2"},
    {modules, [
        wrsd,
        wrsd_realmserver,
        wrsd_web
    ]},
    {registered, [wrsd]},
    {applications, [kernel, stdlib, sasl]},
    {mod, {wrsd, []}},
    {start_phases, [
        {update_loop, []}
    ]}
]}.
