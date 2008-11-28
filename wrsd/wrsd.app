{application, wrsd, [
    {description, "An Erlang World of Warcraft realm status daemon"},
    {vsn, "0.2.1"},
    {modules, [
        wrsd,
		wrsd_eurealmserver,
		wrsd_usrealmserver,
		wrsd_realm,
        wrsd_web
    ]},
    {registered, [wrsd]},
    {applications, [kernel, stdlib, sasl, inets]},
    {mod, {wrsd, []}},
]}.
