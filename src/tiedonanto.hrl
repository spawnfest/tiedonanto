-type tiedonanto_struct() :: map().
-type tiedonanto_rules() :: map().
-type tiedonanto_endpoint() :: map().
-type tiedonanto_payload() :: term().

-record(rule, { content = undefined :: list() | atom() | undefined
              , action = undefined :: function() | undefined
              , return = undefined :: term()
              }).
