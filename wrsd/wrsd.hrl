
-record(realm, {name, status, type, population}).

-define(PORT, 5040).
-define(PRODDOMAIN, "wrs.flux-medivh.com").
-define(DEVDOMAIN, "localhost").
-define(PRODIP, {0, 0, 0, 0}).
-define(DEVIP, {0, 0, 0, 0}).

-define(UPDATEINTERVAL, 60000 * 10).