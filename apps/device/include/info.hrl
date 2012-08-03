-record(controller_version, {board :: byte(),
                             major :: byte(),
                             minor :: byte()}).

-record(converter_info, {ip :: inet:ip4_address(),
                         mask :: inet:ip4_address(),
                         gateway :: inet:ip4_address(),
                         port :: 0..65535,
                         mac :: 0..16#ffffffffffff,
                         encryption :: boolean(),
                         version :: #controller_version{}}).
