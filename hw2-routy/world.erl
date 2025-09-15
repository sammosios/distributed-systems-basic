-module(world).
-export([
    europe/0, asia/0, africa/0, north_america/0, south_america/0,
    oceania/0,
    stop_europe/0, stop_asia/0, stop_africa/0, stop_north_america/0,
    stop_south_america/0, stop_oceania/0
]).
-export([connect/2, disconnect/2]).

%% ----------------------
%% Create bidirectional links between two nodes
%% ----------------------
connect({Node1, Host1}, {Node2, Host2}) ->
    Node1 ! {add, Node2, {Node2, Host2}},
    Node2 ! {add, Node1, {Node1, Host1}}.

disconnect(Node1, Node2) ->
    Node1 ! {remove, Node2},
    Node2 ! {remove, Node1}.

%% ----------------------
%% Europe
%% ----------------------
europe() ->
    Countries = [
        {greece, [athens, thessaloniki, larissa, crete, ioannina]},
        {sweden, [stockholm, gothernburg, uppsala, kiruna, malmo]},
        {germany, [berlin, munich, frankfurt, hamburg, cologne]},
        {italy, [rome, milan, naples, turin, florence]},
        {spain, [madrid, barcelona, valencia, seville, bilbao]}
    ],
    start_countries(Countries).

stop_europe() -> stop_countries([
        {greece, [athens, thessaloniki, larissa, crete, ioannina]},
        {sweden, [stockholm, gothernburg, uppsala, kiruna, malmo]},
        {germany, [berlin, munich, frankfurt, hamburg, cologne]},
        {italy, [rome, milan, naples, turin, florence]},
        {spain, [madrid, barcelona, valencia, seville, bilbao]}
    ]).

%% ----------------------
%% Asia
%% ----------------------
asia() ->
    Countries = [
        {japan, [tokyo, fukinawa, osaka, kyoto, nagoya, sapporo]},
        {china, [beijing, shanghai, guangzhou, shenzhen, chengdu]},
        {india, [delhi, mumbai, bengaluru, kolkata, chennai]},
        {south_korea, [seoul, busan, incheon, daegu, gwangju]},
        {thailand, [bangkok, chiangmai, phuket, pattaya, khonkaen]}
    ],
    start_countries(Countries).

stop_asia() -> stop_countries([
        {japan, [tokyo, fukinawa, osaka, kyoto, nagoya, sapporo]},
        {china, [beijing, shanghai, guangzhou, shenzhen, chengdu]},
        {india, [delhi, mumbai, bengaluru, kolkata, chennai]},
        {south_korea, [seoul, busan, incheon, daegu, gwangju]},
        {thailand, [bangkok, chiangmai, phuket, pattaya, khonkaen]}
    ]).

%% ----------------------
%% Africa
%% ----------------------
africa() ->
    Countries = [
        {egypt, [cairo, alexandria, giza, luxor, aswan]},
        {south_africa, [cape_town, johannesburg, durban, pretoria, port_elizabeth]},
        {nigeria, [lagos, abuja, kano, ibadan, port_harcourt]},
        {kenya, [nairobi, mombasa, kisumu, nakuru, eldoret]},
        {morocco, [casablanca, rabat, marrakech, fes, tangier]}
    ],
    start_countries(Countries).

stop_africa() -> stop_countries([
        {egypt, [cairo, alexandria, giza, luxor, aswan]},
        {south_africa, [cape_town, johannesburg, durban, pretoria, port_elizabeth]},
        {nigeria, [lagos, abuja, kano, ibadan, port_harcourt]},
        {kenya, [nairobi, mombasa, kisumu, nakuru, eldoret]},
        {morocco, [casablanca, rabat, marrakech, fes, tangier]}
    ]).

%% ----------------------
%% North America
%% ----------------------
north_america() ->
    Countries = [
        {usa, [newyork, losangeles, chicago, houston, miami]},
        {canada, [toronto, montreal, vancouver, calgary, ottawa]},
        {mexico, [mexico_city, guadalajara, monterrey, cancun, tijuana]},
        {cuba, [havana, santiago, camaguey, holguin, trinidad]},
        {jamaica, [kingston, montego_bay, portmore, spanish_town, negril]}
    ],
    start_countries(Countries).

stop_north_america() -> stop_countries([
        {usa, [newyork, losangeles, chicago, houston, miami]},
        {canada, [toronto, montreal, vancouver, calgary, ottawa]},
        {mexico, [mexico_city, guadalajara, monterrey, cancun, tijuana]},
        {cuba, [havana, santiago, camaguey, holguin, trinidad]},
        {jamaica, [kingston, montego_bay, portmore, spanish_town, negril]}
    ]).

%% ----------------------
%% South America
%% ----------------------
south_america() ->
    Countries = [
        {brazil, [rio, sao_paulo, salvador, brasilia, fortaleza]},
        {argentina, [buenos_aires, cordoba, rosario, mendoza, la_plata]},
        {chile, [santiago, valparaiso, concepcion, temuco, coquimbo]},
        {peru, [lima, arequipa, cusco, trujillo, iquitos]},
        {colombia, [bogota, medellin, cali, barranquilla, cartagena]}
    ],
    start_countries(Countries).

stop_south_america() -> stop_countries([
        {brazil, [rio, sao_paulo, salvador, brasilia, fortaleza]},
        {argentina, [buenos_aires, cordoba, rosario, mendoza, la_plata]},
        {chile, [santiago, valparaiso, concepcion, temuco, coquimbo]},
        {peru, [lima, arequipa, cusco, trujillo, iquitos]},
        {colombia, [bogota, medellin, cali, barranquilla, cartagena]}
    ]).

%% ----------------------
%% Oceania
%% ----------------------
oceania() ->
    Countries = [
        {australia, [sydney, melbourne, brisbane, perth, adelaide]},
        {new_zealand, [auckland, wellington, christchurch, hamilton, dunedin]},
        {fiji, [suva, lautoka, nadi, labasa, ba]},
        {papua_new_guinea, [port_moresby, lae, mount_hagen, madang, kokopo]},
        {samoa, [apia, tuasivi, fasitoouta, leulumoega, salelologa]}
    ],
    start_countries(Countries).

stop_oceania() -> stop_countries([
        {australia, [sydney, melbourne, brisbane, perth, adelaide]},
        {new_zealand, [auckland, wellington, christchurch, hamilton, dunedin]},
        {fiji, [suva, lautoka, nadi, labasa, ba]},
        {papua_new_guinea, [port_moresby, lae, mount_hagen, madang, kokopo]},
        {samoa, [apia, tuasivi, fasitoouta, leulumoega, salelologa]}
    ]).

%% ----------------------
%% Helper functions
%% ----------------------
start_countries(Countries) ->
    lists:foreach(
        fun({_Country, Cities}) ->
            country_node:init(Cities)
        end,
        Countries
    ).

stop_countries(Countries) ->
    lists:foreach(
        fun({_Country, Cities}) ->
            country_node:stop(Cities)
        end,
        Countries
    ).
