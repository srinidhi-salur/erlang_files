%%%!-----------------------------------------------------------------
%%% yin_file -- Parses a yin-xml file.
%%%
%%% @Date Created: 
%%% @Description:  API towards the yin-xml file.
%%%-------------------------------------------------------------------
-module(yin_models_parser_i).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-export([parse_yin/1,convert_yin/1,build_yin_files/1,build_yin_files/2]).
-export([scan_yin/1,parse_yin_files/1,ets_to_mnesia/1]).
-export([preparse_yin/1,preparse_yin_files/1,preparse_yin_files/2,validate_models/1]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([process_yin/1,get_XML_datatype_tag/2]).
-export([process_choice_body/6,print_models_order/1]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include_lib("/home/zsrisal/csvaluetypes.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

-define(MIM_CLASS, class).
-define(MIM_ATTRIBUTE, attribute).
-define(MIM_ACTION, action).

%%YIN Difenations
-define(YIN_MODULE, module).
-define(YIN_SUBMODULE, submodule).
-define(YIN_CONTAINER, container).
-define(YIN_LIST,list).
-define(YIN_LEAF, leaf).
-define(YIN_LEAF_LIST,'leaf-list').
-define(YIN_ACTION, action).
-define(YIN_DESCRIPTION, description).
-define(YIN_DATATYPE, type).
-define(YIN_TABLE,yinmodeltable).
-define(YIN_DERV_DATA_TABLE,yinderiveddt).

%%!-------------------------------------------------------------------
%% scan_yin, parse_yin -- Starts the scanning/parsing of the yin file.
%% 
%% scan_yin(File) -> ScanResult
%% parse_yin(File) -> ParseResult
%%   File = string()
%%   ScanResult = <See: xmerl_scan:file>
%%   ParseResult = {yin_version(), idl_additions(), classes(), rel()}
%%
%% Scanning a YIN means XML-reading an XML-file, whereas parsing the
%% YIN means scanning the YIN and then reformatting the result to
%% CS-internal YIN-format.
%%--------------------------------------------------------------------


%%DELETE me  Internal function for testing 
convert_yin(File) ->
    {Scan,_} = scan_yin(File),%%Scan xml into XML elements and Texts records
    {ok, _ParsedMIM} = parse(Scan).%% converts XML records into lists and tuple

scan_yin(File) ->
  	    scan_yin_2(File).

scan_yin_2(File) ->
    Fetcher = fun(_DTDSpec, State) -> {ok, State} end,
    xmerl_scan:file(File, [{fetch_fun, Fetcher, _InitState = x}, {comments, false}]).
	
process_yin(ParsedMIM) ->
    Platform = get_platform(ParsedMIM),
    {parse_mim_version(ParsedMIM), 
     parse_idl_additions(ParsedMIM, Platform),
     parse_classes_relationships(ParsedMIM, Platform)}.  

get_platform({{module,_},_}) ->
    yang.

%%!-------------------------------------------------------------------
%% parse -- parses the yin and converts it into list-tuple format
%% 
%% parse(Scanned_yin_file) -> Result
%%   
%% 
%%--------------------------------------------------------------------
parse(Scan) ->
    {ok, reformat(Scan, [])}.

reformat(Element, _PName) when is_record(Element, xmlElement) ->
    ElemName = Element#xmlElement.name,
    Attributes = lists:map(fun(Attr) -> 
				   {Attr#xmlAttribute.name,
				    Attr#xmlAttribute.value}
			   end,
			   Element#xmlElement.attributes),
    Children0 = lists:map(fun(Child) ->
				  reformat(Child, Element#xmlElement.name)
			  end, 
			  Element#xmlElement.content),
    Children1 = lists:filter(fun(empty_string) -> false;
				(_) -> true
			     end,
			     Children0),
    Children = case is_list_of_one_string(Children1) of
		   true -> hd(Children1);
		   false -> Children1
	       end,
    {{ElemName, Attributes}, Children};
reformat(Element, _PName) when is_record(Element, xmlText) ->
    case is_only_whitespace(Element#xmlText.value) of
	true ->  empty_string;
	false -> Element#xmlText.value
    end;
reformat(Element, _PName) ->
    error_logger:info_msg("~p:~p:~p: ~p~n",
			  [self(), ?MODULE, ?LINE, element(1, Element)]),
    {unexpected, element(1, Element)}.

is_only_whitespace(Str) ->
    lists:all(fun($\n) -> true;
		 ($\t) -> true;
		 ($\ ) -> true;
		 (_) -> false
	      end,
	      Str).

is_list_of_one_string([X]) ->
    io_lib:printable_list(X);
is_list_of_one_string(_X) ->
    false.


parse_mim_version({{module,_},_}) ->
    [].
%%FIX me handle submodules
%% parse_classes_relationships({{?YIN_SUB_MODULE, _}, Body}, _Platform) ->
%%     Tag =get_XML_tag(?YIN_CONTAINER, Body),
%%     %%io:format("Parseing the Container(class): ~n~p~n",[Tag]),
%%     covert_yin_to_classes_relations(Tag).
parse_classes_relationships({{?YIN_MODULE, _}, Body}, _Platform) ->
    Tag =get_XML_tag(?YIN_CONTAINER, Body),
    %%io:format("Parseing the Container(class): ~n~p~n",[Tag]),
    covert_yin_to_classes_relations(Tag).

covert_yin_to_classes_relations(ContainerTag) ->
    process_class_rels([],ContainerTag,[],[]).

process_class_rels(_,[],Class, Reln) ->
    [Class, Reln];
process_class_rels([],[{{?YIN_CONTAINER,[{name,Name}]},Body}],Class, Reln) -> 
    {ClassBody,RemBody} = process_class_body(Body,[],[]),
    io:format("~n>>>>>>>> CB ~p~n",[Body]),
    %%FullClass = [{{?MIM_CLASS,[{name,Name}]},ClassBody}],
    FullClass = [{name,Name}]++[ClassBody],
    process_class_rels(Name,RemBody,FullClass++Class,Reln);
process_class_rels([],[{{?YIN_LIST,[{name,Name}]},Body}],Class, Reln) -> 
    {ClassBody,RemBody} = process_class_body(Body,[],[]),
    io:format("~n<<<<<<<< LB~p~n",[Body]),
    FullClass = [{name,Name}]++[ClassBody],
    process_class_rels(Name,RemBody,FullClass++Class,Reln);
process_class_rels(Parent,[{{?YIN_CONTAINER,[{name,Name}]},Body}|Rest],Class, Reln) ->
    {ClassBody,RemBody} = process_class_body(Body,[],[]),
    io:format("~n>>>>>>>> CB ~p~n",[Body]),
    %%FullClass = [{{?MIM_CLASS,[{name,Name}]},ClassBody}],
    FullClass = [{name,Name}]++[ClassBody],
    Rel=[{{relationship,[{name,Parent++"_to_"++Name}]},
	  [{{containment,[]},
	    [{{parent,[]},[{{hasClass,[{name,Parent}]},[{{mimName,[]},"YANG_PM"}]}]},
	     {{child,[]},
	      [{{hasClass,[{name,Name}]},
		[{{mimName,[]},"YANG_PM"}]},
	       {{cardinality,[]},[{{min,[]},"0"},{{max,[]},"1"}]}]}]}]}],%%FIX ME get cardinality
   [Class1, Reln1]= process_class_rels(Name,RemBody,FullClass++Class,Rel++Reln),
    process_class_rels(Parent,Rest,Class1,Reln1);
process_class_rels(Parent, [{{?YIN_LIST,[{name,Name}]},Body}|Rest],Class, Reln) ->
    {ClassBody,RemBody} = process_class_body(Body,[],[]),
    io:format("~n<<<<<<<< LB1 ~p~n",[Body]),
    %%FullClass = [{{?MIM_CLASS,[{name,Name}]},ClassBody}],
    FullClass = [{name,Name}]++[ClassBody],
    Rel=[{{relationship,[{name,Parent++"_to_"++Name}]},
	  [{{containment,[]},
	    [{{parent,[]},[{{hasClass,[{name,Parent}]},[{{mimName,[]},"YANG_PM"}]}]},
	     {{child,[]},
	      [{{hasClass,[{name,Name}]},
		[{{mimName,[]},"YANG_PM"}]},
	       {{cardinality,[]},[{{min,[]},"1"},{{max,[]},"100"}]}]}]}]}],%%FIX ME get cardinality
    [Class1, Reln1]= process_class_rels(Name,RemBody,FullClass++Class,Rel++Reln),
    process_class_rels(Parent,Rest,Class1,Reln1).

process_class_body([],ClassBody, ExtBody) ->
    {ClassBody, ExtBody};
process_class_body([{{?YIN_CONTAINER,_Name},_}=Container|Body],ClassBody,ExtBody) ->
    process_class_body(Body,ClassBody,ExtBody++[Container]);
process_class_body([{{?YIN_LIST,_Name},_ListBody}=ListC|Body],ClassBody,ExtBody) ->
    process_class_body(Body,ClassBody,ExtBody++[ListC]);
process_class_body([{{?YIN_LEAF,_Name},_LeafBody}=LEAF|Body],ClassBody,ExtBody) ->
    process_class_body(Body,[LEAF]++ClassBody,ExtBody);
process_class_body([{{?YIN_ACTION,Name},_ActionBody}=ACTION|Body],ClassBody,ExtBody) ->
    process_class_body(Body,[ACTION]++ClassBody,ExtBody);
    %process_class_body(Body,ClassBody,ExtBody);
process_class_body([Head|Body],ClassBody,ExtBody) ->
    process_class_body(Body,[Head]++ClassBody,ExtBody).

parse_idl_additions({{?YIN_MODULE,_}, _Body}, yang) ->
    [{data_types, []}, 
     {structs, []}, 
     {enums, []}, 
     {exceptions, []},
     {namespaces, []},
     {rootclass, []}].
%% parse_class({{StructTag, Props}, Body}=Class, Platform = yang) ->
%%     io:format("In side LCom: Parse_class ~n ~p ~n",[Class]),
%%     Attributes = [parse_attribute(A, Platform) || 
%% 		     A <- get_XML_tag(?YIN_LEAF, Body)],
%%     io:format("Attributes ~n~n ~p ~n",[Attributes]),
%%     Actions = [parse_action(A, Platform) ||
%% 		  A <- get_XML_tag(?YIN_ACTION, Body)],
%%     lists:append(Props,
%% 		 [{descr, parse_descr(Body)},
%% 		  {flags, parse_flags(Body)},
%% 		  {attributes, Attributes},
%% 		  {actions, Actions}]).


get_XML_tag(_Tag, []) ->
    [];
%%{{dataType,[]},[{{boolean,[]},[{{defaultValue,[]},"true"}]}]}
get_XML_tag(Tag, [{{Tag, Props}, Body} | Tail]) ->
    [{{Tag, Props}, Body} | get_XML_tag(Tag, Tail)];
get_XML_tag(Tag, [_ | Tail]) ->
    get_XML_tag(Tag, Tail).

get_XML_datatype_tag(_,[]) ->
    [];
%{{type,[{name,"uint16"}]},[]},{{default,[{value,"0"}]},[]},
get_XML_datatype_tag(Tag,[{{Tag,[{name,Type}]},[]},{{default,[{value,Value}]},[]}|Tail]) ->
    [{{Tag,[]},[{{list_to_atom(Type),[]},[{{defaultValue,[]},Value}]}]}|get_XML_datatype_tag(Tag,Tail)];
get_XML_datatype_tag(Tag,[{{Tag,[{name,Type}]},[]} |Tail]) ->
%     {{Tag,[]},[{{stre,[]},[]}]}
    [{{Tag,[]},[{{list_to_atom(Type),[]},[]}]}|get_XML_datatype_tag(Tag,Tail)];
get_XML_datatype_tag(Tag,[_|Tail]) ->
    get_XML_datatype_tag(Tag,Tail).
    
parse_yin_files(ModelsDir)->
    io:format("~nmodels dir ~p~n",[ModelsDir]),
    FileList = os:cmd("cd "++ ModelsDir ++ "&& ls *.yin"),
    Inte = os:cmd("cd "++ ModelsDir++"/internal" ++ "&& ls *.yin"),
    _Exte = os:cmd("cd "++ ModelsDir ++ "&& ls *.yin"),
    %% {ok,Inte} = file:consult(ModelsDir ++ "internal_files_list"),
    %% {ok,Exte} = file:consult(ModelsDir ++ "external_files_list"),
    %%File_List={FileList, string:tokens(Inte,"\n"), string:tokens(Exte,"\n")},
    Models_List = string:tokens(FileList,"\n"),
    io:format("~nExternal YIN models List ~p~n",[Models_List]),
    parse_external_yins(ModelsDir++"/",Models_List),
    io:format("~nInternal YIN models List ~p~n",[string:tokens(Inte,"\n")]),
    parse_external_yins(ModelsDir++"/internal/",string:tokens(Inte,"\n")).
   
parse_external_yins(_,[])->
    ok;
parse_external_yins(Dir,[Head|YinList]) ->
    %%io:format("~n Processing yin file:~p ~n",[Head]),
    io:format("~n ~p ~n~n",[parse_yin(Dir++Head)]),
    parse_external_yins(Dir,YinList).
%%erl -noshell -run yin_models_parser build_yin_files /proj/netsimdesign/YangModels/vDU-0_4_0 vDU-0_4_0
%%erl -run yin_models_parser build_yin_files "/proj/netsimdesign/YangModels/vDU-0_4_0" "vDU-0_4_0"
build_yin_files([NodeDir,TName])->
    build_yin_files(NodeDir,TName),
    erlang:halt().
build_yin_files(NodeDir,TName) ->
    %io:format("GenericModels"),
    %%GenericModels= filelib:wildcard("/proj/netsimdesign/YangModels/GenericModels/*.yin"),
    %%GenericModels= filelib:wildcard(NodeDir ++"/Generic/*.yin"),
    %% NodeSpecModels=filelib:wildcard(NodeDir ++"/*.yin"),
    %% Models_List=GenericModels++NodeSpecModels,

    Models_List=filelib:wildcard(NodeDir ++"/*.yin"),
    %%Models_List=GenericModels++NodeSpecModels,
    ModelsDir=[],%%Fix Me:: Remove models dir every where.(This is a quick fix)
    io:format("Validating models Dependencies...~n"),
    case validate_imports_includes(ModelsDir,Models_List,[],[]) of
	[] ->    
	    case {ets:info(?YIN_TABLE),ets:info(?YIN_DERV_DATA_TABLE)} of 
		{undefined, undefined} ->
		    ets:new(?YIN_TABLE, [set, public, named_table]),
		    ets:new(?YIN_DERV_DATA_TABLE,[set, public, named_table]);
		_else ->
		    ok
	    end,
	    %% ets:insert(Table, {Key, Element}),
	    io:format("Preparing models order...~n"),
	    OrderedList=prepare_models_order(ModelsDir,Models_List,[],[],[]),
	    io:format("started processing...~n"),
	    ets:insert(?YIN_TABLE,{{dirpath,models},ModelsDir}),
	    preparse_external_yins(ModelsDir++"/",OrderedList),
	    derived_types_table_resolution(),
	    postparse_yin_files(ModelsDir++"/",OrderedList),
	    process_deviations(),
	    %%  postparse_external_yins(ModelsDir++"/",Models_List),
	    %%multi root
	    %% ets:insert(?YIN_TABLE,{{mim_extended_definitions,rootclass},"ComTop:ManagedElement"}),
	    %% ets:insert(?YIN_TABLE,{{mim_mimdata,{name,"ComTop:ManagedElement"}},[{actions,[]},{attributes,[{managedElementId,[{flags,[mandatory,noNotification,restricted,key]},{descr,<<"Adopted Parent for Yang MOs as they don't have a Parent">>},{type,string}]}]}]}),
	    %% ets:insert(?YIN_TABLE,{{mim_mimdata,{{name,"ComTop:ManagedElement"},{attribute, managedElementId}}},[{flags,[mandatory,noNotification,restricted,key]},{descr,<<"Adopted Parent for Yang MOs as they don't have a Parent">>},{type,string}]}),
	    %% ets:insert(?YIN_TABLE,{{new_mimdata,{name,"ComTop:ManagedElement"}},[{actions,[]},{attributes,[{managedElementId,[{flags,[mandatory,noNotification,restricted,key]},{descr,<<"Adopted Parent for Yang MOs as they don't have a Parent">>},{type,string}]}]}]}),
	    %% ets:insert(?YIN_TABLE,{{new_mimdata,{{name,"ComTop:ManagedElement"},{attribute, managedElementId}}},[{flags,[mandatory,noNotification,restricted,key]},{descr,<<"Adopted Parent for Yang MOs as they don't have a Parent">>},{type,string}]}),
	    %% ets_key_value_append({mim_extended_definitions,namespaces},"ComTop"),
	    %% ets_key_value_append({mim_mimdata,names},"ComTop:ManagedElement"),
	    %% ets:insert(?YIN_TABLE,{{mim_extended_definitions,{namespace,"ComTop"}},[{"urn:com:ericsson:ecim:ComTop","ECIM_Top","2.3","10.21"}]}),

	    ets:tab2file(?YIN_TABLE,TName++".tab");
	    %% mnesia:start(),
	    %% TableName='v16A.V9--MSRBS-V2--Netsim_MSRBS-V2_NODE_MODEL_16A-V9.xml',
	    %% case mnesia:create_table(TableName,
	    %% 			     [{ram_copies, []},
	    %% 			      {attributes, [k,v]},
	    %% 			      {type,       ordered_set}]) of
	    %% 	{atomic, ok}      ->
	    %% 	    case mnesia:wait_for_tables([TableName], 60000) of
	    %% 		ok -> ok;
	    %% 		{timeout, [TableName]} = Reason -> 
	    %% 		    io:format("{error,~p}~n", [Reason])
	    %% 	    end;
	    %% 	{aborted, Reason} ->
	    %% 	    io:format("{error,~p}~n", [Reason])
	    %% end,
	    %% MimData=ets:select(?YIN_TABLE, [{{'_','_'},[],['$_']}]),
	    %% populate_mnesia(TableName,MimData);
	Missing->
	    io:format("Missing Model/s ~p ~n",[Missing])
    end.
	    
    %%{ok, Models_List}=file:list_dir(ModelsDir),
preparse_yin_files(ModelsDir) ->
    preparse_yin_files(ModelsDir,"yin_models").

preparse_yin_files(ModelsDir,TName)->
    io:format("~nmodels dir ~p~n",[ModelsDir]),

    FileList = os:cmd("cd "++ ModelsDir ++ "&& ls *.yin"),   
    Models_List = string:tokens(FileList,"\n"),
    io:format("Validating models Dependencies...~n"),
    case validate_imports_includes(ModelsDir,Models_List,[],[]) of
	[] ->    
	    case {ets:info(?YIN_TABLE),ets:info(?YIN_DERV_DATA_TABLE)} of 
		{undefined, undefined} ->
		    ets:new(?YIN_TABLE, [set, public, named_table]),
		    ets:new(?YIN_DERV_DATA_TABLE,[set, public, named_table]);
		_else ->
		    ok
	    end,
	    %% ets:insert(Table, {Key, Element}),
	    io:format("Preparing models order...~n"),
	    OrderedList=prepare_models_order(ModelsDir,Models_List,[],[],[]),
	    io:format("started processing...~n"),
	    ets:insert(?YIN_TABLE,{{dirpath,models},ModelsDir}),
	    preparse_external_yins(ModelsDir++"/",OrderedList),
	    derived_types_table_resolution(),
	    postparse_yin_files(ModelsDir++"/",OrderedList),
	    %%  postparse_external_yins(ModelsDir++"/",Models_List),

	    ets:insert(?YIN_TABLE,{{mim_extended_definitions,rootclass},"ComTop:ManagedElement"}),
	    ets:insert(?YIN_TABLE,{{mim_mimdata,{name,"ComTop:ManagedElement"}},[{actions,[]},{attributes,[{managedElementId,[{flags,[mandatory,noNotification,restricted,key]},{descr,<<"Adopted Parent for Yang MOs as they don't have a Parent">>},{type,string}]}]}]}),
	    ets:insert(?YIN_TABLE,{{mim_mimdata,{{name,"ComTop:ManagedElement"},{attribute, managedElementId}}},[{flags,[mandatory,noNotification,restricted,key]},{descr,<<"Adopted Parent for Yang MOs as they don't have a Parent">>},{type,string}]}),
	    ets:insert(?YIN_TABLE,{{new_mimdata,{name,"ComTop:ManagedElement"}},[{actions,[]},{attributes,[{managedElementId,[{flags,[mandatory,noNotification,restricted,key]},{descr,<<"Adopted Parent for Yang MOs as they don't have a Parent">>},{type,string}]}]}]}),
	    ets:insert(?YIN_TABLE,{{new_mimdata,{{name,"ComTop:ManagedElement"},{attribute, managedElementId}}},[{flags,[mandatory,noNotification,restricted,key]},{descr,<<"Adopted Parent for Yang MOs as they don't have a Parent">>},{type,string}]}),
	    ets_key_value_append({mim_extended_definitions,namespaces},"ComTop"),
	    ets_key_value_append({mim_mimdata,names},"ComTop:ManagedElement"),
	    ets:insert(?YIN_TABLE,{{mim_extended_definitions,{namespace,"ComTop"}},[{"urn:com:ericsson:ecim:ComTop","ECIM_Top","2.3","10.21"}]}),
	   
	    ets:tab2file(?YIN_TABLE,TName++".tab"),
	    mnesia:start(),
	    TableName='v16A.V9--MSRBS-V2--Netsim_MSRBS-V2_NODE_MODEL_16A-V9.xml',
	    case mnesia:create_table(TableName,
				     [{ram_copies, []},
				      {attributes, [k,v]},
				      {type,       ordered_set}]) of
		{atomic, ok}      ->
		    case mnesia:wait_for_tables([TableName], 60000) of
			ok -> ok;
			{timeout, [TableName]} = Reason -> 
			    io:format("{error,~p}~n", [Reason])
		    end;
		{aborted, Reason} ->
		    io:format("{error,~p}~n", [Reason])
	    end,
	    MimData=ets:select(?YIN_TABLE, [{{'_','_'},[],['$_']}]),
	    populate_mnesia(TableName,MimData);
	Missing->
	    io:format("Missing Model/s ~p ~n",[Missing])
    end.

populate_mnesia(Table,[]) ->
     %%interface deviation 
    ets:insert('v16A.V9--MSRBS-V2--Netsim_MSRBS-V2_NODE_MODEL_16A-V9.xml',{'v16A.V9--MSRBS-V2--Netsim_MSRBS-V2_NODE_MODEL_16A-V9.xml',{mim_mimdata,{name,"if:interface"}},[{{description,[]}, [{{text,[]},"The list of interfaces on the device. System-controlled\ninterfaces created by the system are always present in this\nlist, whether they are configured or not."}]},{attributes,[{speed,[{type,int64},{typeinfo,[]},{flags,[]}]},{'lower-layer-if',[{seq,"if:interface-state-ref"},{typeinfo,[]},{flags,[]}]},{'higher-layer-if',[{seq,"if:interface-state-ref"}, {typeinfo,[]},{flags,[]}]},{'phys-address',[{type,string}, {typeinfo,[]}, {flags,[]}]},{'if-index',[{type,int32}, {typeinfo,[]}, {range,{1,2147483647}}, {flags,[mandatory]}]},{enabled,[{type,boolean},  {defaultvalue,true},  {typeinfo,[]},  {flags,[mandatory]}]},{'last-change',[{type,string},{typeinfo,[]},{flags,[]}]},{description,[{type,string},{typeinfo,[]},{flags,[]}]},{'oper-status',[{type,{enum,"if:oper-status"}},{typeinfo,[]},{flags,[mandatory]}]},{'admin-status',[{type,{enum,"if:admin-status"}}, {typeinfo,[]}, {flags,[mandatory]}]},{type,[{type,string},{typeinfo,[]},{flags,[mandatory]}]},{name,[{type,string},{typeinfo,[]},{flags,[key]}]}]},{actions,[]}]}),
    ets:insert('v16A.V9--MSRBS-V2--Netsim_MSRBS-V2_NODE_MODEL_16A-V9.xml',{'v16A.V9--MSRBS-V2--Netsim_MSRBS-V2_NODE_MODEL_16A-V9.xml',{mim_mimdata,{name,"ifxipos:address"}},[{desc,<<"IP address to be configured">>}, {actions,[]},{attributes,[{'addr-primary',[{desc,<<"IP address to be configured">>},{type,string},{typeinfo,[]},{pattern,"((:|[0-9a-fA-F]{0,4}):)([0-9a-fA-F]{0,4}:){0,5}((([0-9a-fA-F]{0,4}:)?(:|[0-9a-fA-F]{0,4}))|(((25[0-5]|2[0-4][0-9]|[01]?[0-9]?[0-9])\\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9]?[0-9])))(/(([0-9])|([0-9]{2})|(1[0-1][0-9])|(12[0-8])))"},{pattern,"(([^:]+:){6}(([^:]+:[^:]+)|(.*\\..*)))|((([^:]+:)*[^:]+)?::(([^:]+:)*[^:]+)?)(/.+)"},{flags,[]}]}]}]}),
    ets:tab2file(Table,Table);
populate_mnesia(TableName,[{Key,Value}|MimData]) ->
    ets:insert(TableName, {TableName, Key, Value}),  
    populate_mnesia(TableName,MimData).

ets_to_mnesia(TableName) ->
    MimData=ets:select(?YIN_TABLE, [{{'_','_'},[],['$_']}]),
populate_mnesia(TableName,MimData).

%%STab=yinmodeltable,TableName='v16A.V9--MSRBS-V2--Netsim_MSRBS-V2_NODE_MODEL_16A-V9.xml',[ets:insert(TableName, {TableName, Key, Value})||{Key,Value} <- ets:select(STab, [{{'_','_'},[],['$_']}])].
    %%ets:delete(?YIN_TABLE).
postparse_yin_files(ModelsDir,Models_List)->
    io:format("~nmodels dir ~p~n",[ModelsDir]),
    postparse_external_yins(ModelsDir,Models_List).
  
   %%ets:delete(?YIN_TABLE).
print_models_order(Dir) ->
    io:format("~nmodels dir ~p~n",[Dir]),
    FileList = os:cmd("cd "++ Dir ++ "&& ls *.yin"),
    Models_List = string:tokens(FileList,"\n"),
    case validate_imports_includes(Dir,Models_List,[],[]) of
	[] -> prepare_models_order(Dir,Models_List,[],[],[]);
	Missing->
	    io:format("Missing Model/s ~p ~n",[Missing])
    end.
prepare_models_order(_,[],[],_,YinOrder) ->
    lists:reverse(YinOrder);
    %%done;
prepare_models_order(Dir,[],Dependant,Loaded,YinOrder) ->
    prepare_models_order(Dir,Dependant,[],Loaded,YinOrder);
prepare_models_order(Dir,[{Head,Module,Imports}|YinList],Dependant,Loaded,YinOrder) ->
    %%io:format("Processing file ~p ~n",[Head]),
  	    case is_import_include_loaded(Imports,Loaded) of
		true ->
		    %%preprocess_yin(ParsedMIM),
		    %%io:format("~p :::2::~p~n",[Head,Imports]),
		    prepare_models_order(Dir,YinList,Dependant,[Module]++Loaded,[Head]++YinOrder);
		false ->
		    prepare_models_order(Dir,YinList,[{Head,Module,Imports}]++Dependant,Loaded,YinOrder)
	    end;

prepare_models_order(Dir,[Head|YinList],Dependant,Loaded,YinOrder) ->
    %%io:format("Processing file ~p ~n",[Head]),
    {Scan,_} = scan_yin(Dir++"/"++Head),%%Scan xml into XML elements and Texts records
    {ok, ParsedMIM} = parse(Scan),%% converts XML records into lists and tuple
    case fetch_import_include_tags(ParsedMIM) of
	 {Module,[]} ->
	    %%preprocess_yin(ParsedMIM),
	    io:format("~p  NoImport ~n",[Head]),
	    prepare_models_order(Dir,YinList,Dependant,[Module]++Loaded,[Head]++YinOrder);
	{Module,Imports} ->	   
	    case is_import_include_loaded(Imports,Loaded) of
		true ->
		    %%preprocess_yin(ParsedMIM),
		   %% io:format("~p :::::~p~n",[Head,Imports]),
		    prepare_models_order(Dir,YinList,Dependant,[Module]++Loaded,[Head]++YinOrder);
		false ->
		    prepare_models_order(Dir,YinList,[{Head,Module,Imports}]++Dependant,Loaded,YinOrder)
		    %%prepare_models_order(Dir,YinList,[Head]++Dependant,Loaded)
	    end
    end.

validate_models(Dir) ->
    io:format("~nmodels dir ~p~n",[Dir]),
    FileList = os:cmd("cd "++ Dir ++ "&& ls *.yin"),
    Models_List = string:tokens(FileList,"\n"),
    validate_imports_includes(Dir,Models_List,[],[]).
validate_imports_includes(_,[],Import,Modules) ->
    Imports=sets:to_list(sets:from_list(Import)),
    Imports -- Modules;
validate_imports_includes(Dir,[Head|YinList],Dependant,Loaded) ->
   %% io:format("Processing file ~p ~n",[Head]),
    {Scan,_} = scan_yin(Dir++"/"++Head),
    {ok, ParsedMIM} = parse(Scan),
    case fetch_import_include_tags(ParsedMIM) of
	 {Module,[]} ->
	    validate_imports_includes(Dir,YinList,Dependant,[Module]++Loaded);
	{Module,Imports} ->	   
	    validate_imports_includes(Dir,YinList,lists:merge(Imports,Dependant),[Module]++Loaded)
    end.
%%=====================================================
%%is_import_loaded
%% return - true/false
%%=====================================================
is_import_include_loaded(Imports,Loaded) ->
    %io:format("Imports ~p ~n ~p ~n",[Imports,Loaded]),
    case lists:filter(fun(Import) ->
			      lists:member(Import,Loaded) end,Imports) of
	[] ->
	    false;
	Imports ->
	    true;
	_Else ->
	    false
    end.
%%return []/List
fetch_import_include_tags(ParsedMIM) ->
    {Module,Tags}= case ParsedMIM of
		       {{?YIN_MODULE, [{name,Mod}|_]}, Body} ->
			   {Mod,Body};
		       {{?YIN_SUBMODULE,[{name,Mod}|_]}, Body} ->
			   {Mod,Body}
		   end,
    Imports=case get_XML_tag(import,Tags) of
		[] ->
		    [];
		Impts->		   
		    Im=[M||{{import,[{module,M}]},[{{prefix,[{value,_}]},[]}]} <-Impts],
		    %%io:format("Imports Identified ~p~n",[Im]),
		    Im		   
	    end,
    Includes=case get_XML_tag(include,Tags) of
		 [] ->
		     [];
		 Incls->
		     In=[M||{{include,[{module,M}]},[{{prefix,[{value,_}]},[]}]} <-Incls],
		     %%io:format("Includes Identified ~p~n",[In]),
		     In
	     end,
    {Module,Imports++Includes}.

preparse_external_yins(_,[])->
    ok;
preparse_external_yins(Dir,[Head|YinList]) ->
    io:format("~n Processing yin file:~p ~n",[Head]),
    io:format("~n ~p ~n~n",[preparse_yin(Dir++Head)]),
    preparse_external_yins(Dir,YinList).

postparse_external_yins(_,[])->
    ok;
postparse_external_yins(Dir,[Head|YinList]) ->
    io:format("~n Post Processing yin file:~p ~n",[Head]),
    io:format("~n ~p ~n~n",[postparse_yin(Dir++Head)]),
    postparse_external_yins(Dir,YinList).

preparse_yin(File) ->
     {Scan,_} = scan_yin(File),%%Scan xml into XML elements and Texts records
    {ok, ParsedMIM} = parse(Scan),%% converts XML records into lists and tuple
    preprocess_yin(ParsedMIM).%% Processes all tags except containers
postparse_yin(File) ->
     {Scan,_} = scan_yin(File),%%Scan xml into XML elements and Texts records
    {ok, ParsedMIM} = parse(Scan),%% converts XML records into lists and tuple
    postprocess_yin(ParsedMIM).%% Processes all tags except containers
parse_yin(File) ->
    {Scan,_} = scan_yin(File),%%Scan xml into XML elements and Texts records
    {ok, ParsedMIM} = parse(Scan),%% converts XML records into lists and tuple
    process_yin(ParsedMIM).%%

preprocess_yin({{?YIN_MODULE, _}, Body}) ->
    io:format("Module"),    
    process_submodule_tags(Body,[]);
preprocess_yin({{?YIN_SUBMODULE, _}, Body}) ->
    io:format("SM processing"),
    process_submodule_tags(Body,[]).

postprocess_yin({{?YIN_MODULE, _}, Body}) ->
    io:format("PostModule"),    
    process_typedef_enum_unions(Body,[]);
postprocess_yin({{?YIN_SUBMODULE, _}, Body}) ->
    io:format("PostSM processing"),
    process_typedef_enum_unions(Body,[]).

%%%=====Module===========
%%{{namespace,[{uri,"urn:rdns:com:ericsson:oammodel:ericsson-bfd-dev-ssr"}]},[]}
%% process_submodule_tags([{{namespace,[{uri,_NS}]},[]}|Rest],Prfx) ->
%%     io:format("SM namespace"),
%% %% {mim_extended_definitions,{namespace,"RcsSwM"}},
%% %%     [{"urn:com:ericsson:ecim:RcsSwM","ECIM_SwM","4.0","3.0"}]}
%%     process_submodule_tags(Rest,Prfx);
%% %%{{prefix,[{value,"rsvpdevssr"}]},[]}
%% process_submodule_tags([{{prefix,[{value,Prefix}]},[]}|Rest],_) ->
%%     io:format("~n SM Prefix ~p ~n",[Prefix]),
%%     process_submodule_tags(Rest,Prefix);
process_submodule_tags([{{prefix,[{value,Prefix}]},[]},{{namespace,[{uri,NS}]},[]}|Rest],_) ->
    io:format("SM namespace and Prefix"),
    ets:insert(?YIN_TABLE,{{mim_extended_definitions,{namespace,Prefix}},[{NS,"","",""}]}),
    ets_key_value_append({mim_extended_definitions,namespaces},Prefix),
%% {mim_extended_definitions,{namespace,"RcsSwM"}},
%%     [{"urn:com:ericsson:ecim:RcsSwM","ECIM_SwM","4.0","3.0"}]}
    process_submodule_tags(Rest,Prefix);

process_submodule_tags([{{namespace,[{uri,NS}]},[]},{{prefix,[{value,Prefix}]},[]}|Rest],_) ->
    io:format("SM namespace and Prefix"),
    ets:insert(?YIN_TABLE,{{mim_extended_definitions,{namespace,Prefix}},[{NS,"","",""}]}),
    ets_key_value_append({mim_extended_definitions,namespaces},Prefix),
%% {mim_extended_definitions,{namespace,"RcsSwM"}},
%%     [{"urn:com:ericsson:ecim:RcsSwM","ECIM_SwM","4.0","3.0"}]}
    process_submodule_tags(Rest,Prefix);
%%{{prefix,[{value,"rsvpdevssr"}]},[]}
%% process_submodule_tags([|Rest],_) ->
%%     io:format("SM namespace"),
%%     process_submodule_tags(Rest,Prefix);

%%==========================

%{{'yang-version',[{value,"1.1"}]},[]}
process_submodule_tags([{{'yang-version',[{value,_VN}]},[]}|Rest],Prfx) ->
    io:format("SM yang-version"),
    process_submodule_tags(Rest,Prfx);

%{{'belongs-to',[{module,"ericsson-epg"}]},
%       [{{prefix,[{value,"epg"}]},[]}]}
process_submodule_tags([{{'belongs-to',[{module,_Mod}]},[{{prefix,[{value,Prfx}]},[]}]}|Rest],_) ->
    io:format("SM belongs-to"),
    process_submodule_tags(Rest,Prfx);

%%{{import,[{module,"tailf-common"}]},[{{prefix,[{value,"tailf"}]},[]}]},
process_submodule_tags([{{import,[{module,_Mod}]},[{{prefix,[{value,_Prefx}]},[]}]}|Rest],Prfx) ->
    io:format("SM Import"),
    process_submodule_tags(Rest,Prfx);

%%{{include,[{module,"ericsson-types-seg"}]},[]}
process_submodule_tags([{{include,[{module,_Mod}]},[]}|Rest],Prfx) ->
    io:format("SM Include"),
    process_submodule_tags(Rest,Prfx);

%%{{organization,[]}, [{{text,[]}, "example"}]}

process_submodule_tags([{{organization,_},_}|Rest],Prfx) ->
    io:format("SM organization"),
    process_submodule_tags(Rest,Prfx);

%%{{contact,[]},[{{text,[]},"<example@mail.com>"}]}
process_submodule_tags([{{contact,_},_}|Rest],Prfx) ->
    io:format("SM contact"),
    process_submodule_tags(Rest,Prfx);
%%{{description,[]},[{{text,[]}, "description"}]}
process_submodule_tags([{{description,_},_}|Rest],Prfx) ->
    io:format("SM description"),
    process_submodule_tags(Rest,Prfx);
%{{revision,[{date,"2017-04-07"}]},[{{description,[]},[{{text,[]},"Initial revision."}]}]}
process_submodule_tags([{{revision,_},_}|Rest],Prfx) ->
    io:format("SM revision"),
    process_submodule_tags(Rest,Prfx);

process_submodule_tags([{{typedef,[{name,UnionName}]},[{{type,[{name,"union"}]},UnionTypeBody}|_Rem]}|Rest],Prfx) ->
    case lists:keysearch({status,[{value,"obsolete"}]}, 1, UnionTypeBody) of
	false ->
	    io:format("~nSM Typedef Union ~p",[UnionName]),
	    UnionTypes={uniontypes,process_uniontype_body(UnionTypeBody,[],Prfx,UnionName,0)},
	    ets:insert(?YIN_TABLE,{{mim_extended_definitions,{union,Prfx++":"++UnionName}},[UnionTypes]}),
	    process_submodule_tags(Rest,Prfx);
	_ ->
	    process_submodule_tags(Rest,Prfx)
    end;

process_submodule_tags([{{typedef,[{name,EnumName}]},[{{type,[{name,"enumeration"}]},EnumMemBody}|Rem]}|Rest],Prfx) ->
    case lists:keysearch({status,[{value,"obsolete"}]}, 1, EnumMemBody) of
	false ->
	    io:format("~nSM Typedef Enumeration ~p and SKIPPED ~p",[EnumName,Rem]),
	    EnumMembers={enummembers,process_enumeration_body(EnumMemBody)},
	    ets:insert(?YIN_TABLE,{{mim_extended_definitions,{enum,A=Prfx++":"++EnumName}},[{flags,[]},EnumMembers]}),
	    ets_key_value_append({mim_extended_definitions,enums},A),
	    process_submodule_tags(Rest,Prfx);
	_ ->
	    process_submodule_tags(Rest,Prfx)
    end;

process_submodule_tags([{{typedef,[{name,TpdfName}]},TpdfType}|Rest],Prfx) ->
    case lists:keysearch({status,[{value,"obsolete"}]}, 1, TpdfType) of
	false ->
	    io:format("~nSM Typedef ~p",[TpdfName]),
	    %%io:format("~nDatatype ~p~n",[TpdfType]),
	    case process_typedef_body(TpdfType,[]) of
		{derived, DDTBody} ->
		    ets:insert(?YIN_DERV_DATA_TABLE,{{mim_extended_definitions,{datatype,Prfx++":"++TpdfName}},DDTBody});
		DTBody ->
		    ets:insert(?YIN_TABLE,{{mim_extended_definitions,{datatype,Prfx++":"++TpdfName}},DTBody})
	    end,
	    process_submodule_tags(Rest,Prfx);
	_ ->
	    io:format("~nSM Typedef Obsoleted ~p",[TpdfName]),
	    process_submodule_tags(Rest,Prfx)
    end;
 
%%Internal grouping
process_submodule_tags([{{grouping,[{name,GrpName}]},GrpBody}|Rest],Prfx) ->
    case lists:keysearch({status,[{value,"obsolete"}]}, 1, GrpBody) of
	false ->
	    io:format("~nSM Grouping ~p~n ",[{Prfx,GrpName}]),
	    ets:insert(?YIN_TABLE,{{Prfx,GrpName},GrpBody}),
	    process_submodule_tags(Rest,Prfx);
	_ ->
	    io:format("~nSM Grouping Obsoleted ~p~n",[{Prfx,GrpName}]),
	    process_submodule_tags(Rest,Prfx)
    end;
process_submodule_tags([_Tag|Rest],Prfx) ->
    %%io:format("~nSM Skipped ~p ~n",[Tag]),
    process_submodule_tags(Rest,Prfx);
process_submodule_tags([],_) ->
ok.
%%PostProcessing -Second Iteration
process_typedef_enum_unions([{{prefix,[{value,Prefix}]},[]},{{namespace,[{uri,NS}]},[]}|Rest],_) ->
    io:format("Postprocess-namespace and Prefix"),
    ets:insert(?YIN_TABLE,{{mim_extended_definitions,{namespace,Prefix}},[{NS,"","",""}]}),
    process_typedef_enum_unions(Rest,Prefix);

process_typedef_enum_unions([{{namespace,[{uri,NS}]},[]},{{prefix,[{value,Prefix}]},[]}|Rest],_) ->
    io:format("Posrprocess-namespace and Prefix"),
    ets:insert(?YIN_TABLE,{{mim_extended_definitions,{namespace,Prefix}},[{NS,"","",""}]}),
    process_typedef_enum_unions(Rest,Prefix);
process_typedef_enum_unions([{{'belongs-to',[{module,_Mod}]},[{{prefix,[{value,Prfx}]},[]}]}|Rest],_) ->
    io:format("Postprocess-belongs-to"),
    process_typedef_enum_unions(Rest,Prfx);
process_typedef_enum_unions([{{typedef,[{name,UnionName}]},[{{type,[{name,"union"}]},UnionTypeBody}|_Rem]}|Rest],Prfx) ->
    case lists:keysearch({status,[{value,"obsolete"}]}, 1, UnionTypeBody) of
	false ->
	    io:format("~nPostProcess-Typedef Union ~p",[UnionName]),
	    UnionTypes={uniontypes,process_uniontype_body(UnionTypeBody,[],Prfx,UnionName,0)},
	    ets:insert(?YIN_TABLE,{{mim_extended_definitions,{union,Prfx++":"++UnionName}},[UnionTypes]}),
	    process_typedef_enum_unions(Rest,Prfx);
	_ ->
	    process_typedef_enum_unions(Rest,Prfx)
    end;
process_typedef_enum_unions([Container={{?YIN_CONTAINER,[{name,Name}]},Body}|Rest],Prfx) ->
	    io:format("~nPostProcess-container ~p",[Name]),
	    case lists:keysearch({status,[{value,"obsolete"}]}, 1, Body) of
		false ->
		    process_container_list_rels([],Container,Prfx,[]),
		    process_typedef_enum_unions(Rest,Prfx);
		_ ->
		    process_typedef_enum_unions(Rest,Prfx)
	    end;
	%%ets:insert(?YIN_TABLE,{{mim_extended_definitions,{enum,Prfx++":"++EnumName}},[{flags,[]},EnumMembers]}),
process_typedef_enum_unions([Container={{?YIN_LIST,[{name,Name}]},Body}|Rest],Prfx) ->
    io:format("~nPostProcess-List ~p",[Name]),
    case lists:keysearch({status,[{value,"obsolete"}]}, 1, Body) of
	false ->
	    process_container_list_rels([],Container,Prfx,[]),
	    process_typedef_enum_unions(Rest,Prfx);
	_ ->
	    process_typedef_enum_unions(Rest,Prfx)
    end;
    %%ets:insert(?YIN_TABLE,{{mim_extended_definitions,{enum,Prfx++":"++EnumName}},[{flags,[]},EnumMembers]}),
process_typedef_enum_unions([_Augment={{augment,[{'target-node',TargetNodePath}]},AugBody}|Rest],Prfx) ->
    io:format("~nPostProcess-Augment : targetnode ~p",[TargetNodePath]),
    %process_augment(AugBody
    Parent=lists:last(string:tokens(TargetNodePath,"/")),
    %% Prfx=hd(string:tokens(Parent, ":")),
    process_container_list_rels(Parent,AugBody,Prfx,{augment,Parent}),
    %%ets:insert(?YIN_TABLE,{{mim_extended_definitions,{enum,Prfx++":"++EnumName}},[{flags,[]},EnumMembers]}),
    process_typedef_enum_unions(Rest,Prfx);
process_typedef_enum_unions([Deviation={{deviation,[{'target-node',TargetNodePath}]},DeviaBody}|Rest],Prfx) ->
    io:format("~nPostProcess-Deviation: targetnode ~p",[TargetNodePath]),
    %process_augment(AugBody
    Parent=lists:last(string:tokens(TargetNodePath,"/")),
    %% Prfx=hd(string:tokens(Parent, ":")),
    process_container_list_rels(Parent,DeviaBody,Prfx,{deviation,TargetNodePath}),
    %%ets:insert(?YIN_TABLE,{{mim_extended_definitions,{enum,Prfx++":"++EnumName}},[{flags,[]},EnumMembers]}),
    process_typedef_enum_unions(Rest,Prfx);
process_typedef_enum_unions([Tag|Rest],Prfx) ->
    io:format("~nPostProcess-Skipped ~p ~n",[Tag]),
    process_typedef_enum_unions(Rest,Prfx);
process_typedef_enum_unions([],_) ->
ok.

process_typedef_body([],Body) ->
    Body;
process_typedef_body([{{type,[{name,Type}]},Typebody}|Rest],Body) when 

Type == "bits";
Type == "boolean";
Type == "decimal64";
Type == "instance-identifier";
Type == "int8";
Type == "int16";
Type == "int32";
Type == "int64";
Type == "string" -> 
    TpBody = process_typebody(Typebody),
    process_typedef_body(Rest,[{type,list_to_atom(Type)},{typeinfo,[]}]++TpBody++Body);
process_typedef_body([{{type,[{name,Type=[_U|RType]}]},Typebody}|Rest],Body) when 
Type == "uint8";
Type == "uint16";
Type == "uint32";
Type == "uint64" ->
    TpBody = process_typebody(Typebody),
    process_typedef_body(Rest,[{type,list_to_atom(RType)},{typeinfo,[]}]++TpBody++Body);
%% FIX ME: Although base type of both identityref and leafref appears to be string as per current level of understanding, validations regarding whether the specified value is actually an existing container or list or leaf needs to be handled in later stages. string may need to be changed to some other type if better understanding in future points to different base data type. Leafref in particular requires further extensive study.
process_typedef_body([{{type,[{name,Type}]},Typebody}|Rest],Body) when 
Type == "binary";
Type == "identityref";
Type == "leafref" ->
    TpBody = process_typebody(Typebody),
    process_typedef_body(Rest,[{type,string},{typeinfo,[]}]++TpBody++Body);
%% FIX ME: using string for "empty" type leafs just to view in MOBrowser. However as per rfc empty datatype denotes a leaf that cannot be set with any value. The information is conveyed simply by the presence or absence of the empty type leaf.
process_typedef_body([{{type,[{name,Type}]},Typebody}|Rest],{derived,Body}) when 
Type == "empty" -> 
    TpBody = process_typebody(Typebody),
    case lists:keysearch(flags,1,Body) of
	false ->
	    process_typedef_body(Rest,{derived,[{type,string},{typeinfo,[]}]++TpBody++Body});
	{value,{flags,Val}} ->
	    process_typedef_body(Rest,{derived,[{type,string}] ++ lists:keyreplace(flags,1,Body,{flags,[isNillable] ++ Val})++TpBody})		    
    end;
%% FIX ME: using string for "empty" type leafs just to view in MOBrowser. However as per rfc empty datatype denotes a leaf that cannot be set with any value. The information is conveyed simply by the presence or absence of the empty type leaf.
process_typedef_body([{{type,[{name,Type}]},Typebody}|Rest],Body) when 
Type == "empty" -> 
    TpBody = process_typebody(Typebody),
    case lists:keysearch(flags,1,Body) of
	false ->
	    process_typedef_body(Rest,[{type,string},{typeinfo,[]}]++TpBody++Body);
	{value,{flags,Val}} ->
	    process_typedef_body(Rest,[{type,string}] ++ lists:keyreplace(flags,1,Body,{flags,[isNillable] ++ Val})++TpBody)
    end;
process_typedef_body([{{type,[{name,Type}]},Typebody}|Rest],Body) ->
    TpBody = process_typebody(Typebody),
    process_typedef_body(Rest,{derived,[{type,list_to_atom(Type)},{typeinfo,[]}]++TpBody++Body});
process_typedef_body([{{default,[{value,DefValue}]},[]}|Rest],{derived,Body}) ->
    process_typedef_body(Rest,{derived,[{defaultvalue,DefValue}]++Body});
process_typedef_body([{{default,[{value,DefValue}]},[]}|Rest],Body) ->
    process_typedef_body(Rest,[{defaultvalue,DefValue}]++Body);
process_typedef_body([{{mandatory,[{value,"true"}]},[]}|Rest],{derived,Body}) ->
    case lists:keysearch(flags,1,Body) of
	false ->
	    process_typedef_body(Rest,{derived,[{flags,[mandatory]}]++Body});
	{value,{flags,Val}} ->
	    process_typedef_body(Rest,{derived,lists:keyreplace(flags,1,Body,{flags,[mandatory] ++ Val})})		    
    end;
%%--MM--%%
%% process_typedef_body([{{config,[{value,Value}]},[]}|Rest],{derived,Body}) ->
%%     process_typedef_body(Rest,{derived,[{config,Value}]++Body});
%% process_typedef_body([{{config,[{value,Value}]},[]}|Rest],Body) ->
%%     process_typedef_body(Rest,[{config,Value}]++Body);
    %% case lists:keysearch(flags,1,Body) of
    %% 	false ->
    %% 	    process_typedef_body(Rest,{derived,[{flags,[readonly]}]++Body});
    %% 	{value,{flags,Val}} ->
    %% 	    process_typedef_body(Rest,{derived,lists:keyreplace(flags,1,Body,{flags,[readonly] ++ Val})})		    
    %% end;
%% process_typedef_body([{{units,[{name,Value}]},[]}|Rest],{derived,Body}) ->
%%     process_typedef_body(Rest,{derived,[{units,Value}]++Body});
%% process_typedef_body([{{units,[{name,Value}]},[]}|Rest],Body) ->
%%     process_typedef_body(Rest,[{units,Value}]++Body);
%%--MM--%%
process_typedef_body([{{'notife:notifiable-state-data',[{value,"false"}]},[]}|Rest],{derived,Body}) ->
    case lists:keysearch(flags,1,Body) of
	false ->
	    process_typedef_body(Rest,{derived,[{flags,[noNotification]}]++Body});
	{value,{flags,Val}} ->
	    process_typedef_body(Rest,{derived,lists:keyreplace(flags,1,Body,{flags,[noNotification] ++ Val})})		    
    end;
%{{mandatory,[{value,"true"}]},[]}
process_typedef_body([{{mandatory,[{value,"true"}]},[]}|Rest],Body) ->
    case lists:keysearch(flags,1,Body) of
	false ->
	    process_typedef_body(Rest,[{flags,[mandatory]}]++Body);
	{value,{flags,Val}} ->
	    process_typedef_body(Rest,lists:keyreplace(flags,1,Body,{flags,[mandatory] ++ Val}))		    
    end;
%% %%--MM--%%
%% process_typedef_body([{{config,[{value,Value}]},[]}|Rest],Body) ->
%%     process_typedef_body(Rest,[{config,Val}]++Body);
    %% case lists:keysearch(flags,1,Body) of
    %% 	false ->
    %% 	    process_typedef_body(Rest,[{flags,[readonly]}]++Body);
    %% 	{value,{flags,Val}} ->
    %% 	    process_typedef_body(Rest,lists:keyreplace(flags,1,Body,{flags,[readonly] ++ Val}))		    
    %% end;
%%--MM--%%
process_typedef_body([{{'notife:notifiable-state-data',[{value,"false"}]},[]}|Rest],Body) ->
    case lists:keysearch(flags,1,Body) of
	false ->
	    process_typedef_body(Rest,[{flags,[noNotification]}]++Body);
	{value,{flags,Val}} ->
	    process_typedef_body(Rest,lists:keyreplace(flags,1,Body,{flags,[noNotification] ++ Val}))		    
    end;
process_typedef_body([Desc_Ref|Rest],Body) ->
    io:format("Datatype Skipped ~p ~n",[Desc_Ref]),
    process_typedef_body(Rest,Body).

process_leaf_typedef_body([],Body,_Prfx) ->
    Body;
process_leaf_typedef_body([{{type,[{name,Type}]},Typebody}|Rest],Body,Prfx) when 
Type == "bits";
Type == "boolean";
Type == "decimal64";
Type == "instance-identifier";
Type == "int8";
Type == "int16";
Type == "int32";
Type == "int64";
Type == "string" -> 
    TpBody = process_typebody(Typebody),
    process_leaf_typedef_body(Rest,[{type,list_to_atom(Type)},{typeinfo,[]}]++TpBody++Body,Prfx);
process_leaf_typedef_body([{{type,[{name,Type=[_U|RType]}]},Typebody}|Rest],Body,Prfx) when 
Type == "uint8";
Type == "uint16";
Type == "uint32";
Type == "uint64" ->
    TpBody = process_typebody(Typebody),
    process_leaf_typedef_body(Rest,[{type,list_to_atom(RType)},{typeinfo,[]}]++TpBody++Body,Prfx);
%% FIX ME: Although base type of both identityref and leafref appears to be string as per current level of understanding, validations regarding whether the specified value is actually an existing container or list or leaf needs to be handled in later stages. string may need to be changed to some other type if better understanding in future points to different base data type. Leafref in particular requires further extensive study.
process_leaf_typedef_body([{{type,[{name,Type}]},Typebody}|Rest],Body,Prfx) when 
Type == "binary";
Type == "identityref";
Type == "leafref" ->
    TpBody = process_typebody(Typebody),
    process_leaf_typedef_body(Rest,[{type,string},{typeinfo,[]}]++TpBody++Body,Prfx);
%% FIX ME: using string for "empty" type leafs just to view in MOBrowser. However as per rfc empty datatype denotes a leaf that cannot be set with any value. The information is conveyed simply by the presence or absence of the empty type leaf.
process_leaf_typedef_body([{{type,[{name,Type}]},Typebody}|Rest],Body,Prfx) when 
Type == "empty" -> 
    TpBody = process_typebody(Typebody),
    case lists:keysearch(flags,1,Body) of
	false ->
	    process_leaf_typedef_body(Rest,[{type,string},{typeinfo,[]}]++TpBody++Body,Prfx);
	{value,{flags,Val}} ->
	    process_leaf_typedef_body(Rest,[{type,string}] ++ lists:keyreplace(flags,1,Body,{flags,[isNillable] ++ Val})++TpBody,Prfx)
    end;
process_leaf_typedef_body([{{type,[{name,"enumeration"}]},Typebody}|Rest],Body,Prfx) ->
    EnumBody = {enummembers,process_enumeration_body(Typebody)},%%FIX me processing desc,mandate etc
    process_leaf_typedef_body(Rest,[{type,enumeration},EnumBody,{typeinfo,[]}]++Body,Prfx);
process_leaf_typedef_body([{{type,[{name,DerType}]},Typebody}|Rest],Body,Prfx) ->
    TpBody = process_typebody(Typebody),
    Type = case re:run(DerType, ":") of
		      nomatch ->
			  Prfx ++ ":" ++ DerType;
		      _other ->
			  DerType
		  end,
    
    case ets:lookup(?YIN_TABLE,
		    {mim_extended_definitions,{datatype,Type}}) of
	[{_Key,BaseBody}] ->
	    NewBaseBody = simdiv:key_mergereplace([{typeinfo,[]}],simdiv:key_mergereplace(TpBody,BaseBody)),
	    process_leaf_typedef_body(Rest,NewBaseBody++Body,Prfx);
        []->
	    case ets:lookup(?YIN_TABLE,
			    {mim_extended_definitions,{union,Type}}) of
		[{_Key,BaseBody}] ->
		    NewBaseBody = simdiv:key_mergereplace([{type,string},{prasad,union,Type},{typeinfo,[]}],simdiv:key_mergereplace(TpBody,BaseBody)),
		    process_leaf_typedef_body(Rest,NewBaseBody++Body,Prfx);
		[]->
		    case ets:lookup(?YIN_TABLE,
				    {mim_extended_definitions,{enum,Type}}) of
			[{_Key,BaseBody}] ->
			    %% ENUM STRING
			    NewBaseBody = simdiv:key_mergereplace([{type,string},{typeinfo,[]}],simdiv:key_mergereplace(TpBody,BaseBody)),
			    process_leaf_typedef_body(Rest,NewBaseBody++Body,Prfx);
			[] ->
			    error_logger:info_msg("~nLeaf Data Type not found ~p ~n",
						  [Type]),
			    process_leaf_typedef_body(Rest,[{type,string},{prasad,manish,Type},{typeinfo,[]}]++TpBody++Body,Prfx)
		    end
	    end
    end;
process_leaf_typedef_body([{{mandatory,[{value,"true"}]},[]}|Rest],Body,Prfx) ->
    case lists:keysearch(flags,1,Body) of
	false ->
	    process_leaf_typedef_body(Rest,[{flags,[mandatory]}]++Body,Prfx);
	{value,{flags,Val}} ->
	    process_leaf_typedef_body(Rest,lists:keyreplace(flags,1,Body,{flags,[mandatory] ++ Val}),Prfx)		    
    end;
process_leaf_typedef_body([{{config,[{value,Value=false}]},[]}|Rest],Body,Prfx) ->
    %%--MM--%%
    process_leaf_typedef_body(Rest,[{config,Value}]++Body,Prfx);
    %% case lists:keysearch(flags,1,Body) of
    %% 	false ->
    %% 	    process_leaf_typedef_body(Rest,[{flags,[readonly]}]++Body,Prfx);
    %% 	{value,{flags,Val}} ->
    %% 	    process_leaf_typedef_body(Rest,lists:keyreplace(flags,1,Body,{flags,[readonly] ++ Val}),Prfx)		    
    %% end;
process_leaf_typedef_body([{choice,ChName}|Rest],Body,Prfx) ->
    process_leaf_typedef_body(Rest,[{choice,ChName}]++Body,Prfx);
process_leaf_typedef_body([{{units,[{name,Value}]},[]}|Rest],Body,Prfx) ->
    process_leaf_typedef_body(Rest,[{units,Value}]++Body,Prfx);
    %%--MM--%%
process_leaf_typedef_body([{{'notife:notifiable-state-data',[{value,"false"}]},[]}|Rest],Body,Prfx) ->
    case lists:keysearch(flags,1,Body) of
	false ->
	    process_leaf_typedef_body(Rest,[{flags,[noNotification]}]++Body,Prfx);
	{value,{flags,Val}} ->
	    process_leaf_typedef_body(Rest,lists:keyreplace(flags,1,Body,{flags,[noNotification] ++ Val}),Prfx)		    
    end;
process_leaf_typedef_body([{{description,[]},[{{text,[]},[Desc]}]}|Rest],Body,Prfx) ->
    process_leaf_typedef_body(Rest,[{desc,Desc}]++Body,Prfx);
process_leaf_typedef_body([{{description,[]},[{{text,[]},Desc}]}|Rest],Body,Prfx) ->
    process_leaf_typedef_body(Rest,[{desc,list_to_binary(Desc)}]++Body,Prfx);
process_leaf_typedef_body([Desc_Ref|Rest],Body,Prfx) ->
    io:format("Leaf Datatype body process Skipped ~p ~n",[Desc_Ref]),
    process_leaf_typedef_body(Rest,Body,Prfx).

convert_range(Range) -> %"1..6|8|10|20|30|40|50|60|80|100|200|300|400|500|600|800|1000|1200|1600"
    case string:tokens(Range,"..") of %1 , 6|8|10|20|30|40|50|60|80|100|200|300|400|500|600|800|1000|1200|1600
	["min",Value] ->
	    {0,list_to_integer(string:strip(Value))};
	["min ",Value] ->
	    {0,list_to_integer(string:strip(Value))};
	[Value, "max"] ->
	    {list_to_integer(string:strip(Value)),9999999999};
	[Value, " max"] ->
	    {list_to_integer(string:strip(Value)),9999999999};
	[Min,Max] ->	    
	    [Min1|Min2] = string:tokens(Min,"|") ,	
	    [Max1|Max2] =  string:tokens(Max,"|"),			    
	    {list_to_integer(string:strip(Min1)),list_to_integer(string:strip(Max1))};	
	[Value] ->
	    case string:tokens(Value,"|") of
		[Min] ->
		    list_to_integer(string:strip(Min));
		[Min1|Min2] ->
		    {list_to_integer(string:strip(Min1)),list_to_integer(string:strip(lists:last(Min2)))}		
	    end;	   
	[Min|Tail] ->
	    {list_to_integer(string:strip(Min)),list_to_integer(string:strip(lists:last(Tail)))}
    end.
process_typebody(Typebody) ->
    process_typebody(Typebody,[]).
process_typebody([],Acc) ->
    lists:reverse(Acc);
process_typebody([{{range,[{value,Range}]},[]}|Rest],Acc) ->
   process_typebody(Rest,[{range,convert_range(Range)}]++Acc);
process_typebody([{{length,[{value,Range}]},[]}|Rest],Acc) ->
    process_typebody(Rest,[{lengthRange,convert_range(Range)}]++Acc);
process_typebody([{{pattern,[{value,Pattern}]},[]}|Rest],Acc) ->
    process_typebody(Rest,[{pattern,Pattern}]++Acc);
%%[{{path,[{value,"/if:interfaces-state/if:interface/if:name"}]},[]}]}
process_typebody([{{path,[{value,Path}]},[]}|Rest],Acc) ->
    process_typebody(Rest,[{path,Path}]++Acc);
process_typebody([Rem|Rest],Acc) ->
    io:format("TypeBody Skipped: ~p ~n",[Rem]),
    process_typebody(Rest,Acc).
process_deviations() ->
    %DDataType=ets:select(?YIN_DERV_DATA_TABLE, [{{'_','_'},[],['$_']}]),
    %%--MM--%%
    DDataType=ets:select(?YIN_DERV_DATA_TABLE, [{{{deviation,'_','_'},'_'},[],['$_']}]),
    io:format("~nDev from db ddatatype ~p~n",[DDataType]),
    %%--MM--%%
    deviation_resolution(DDataType).

deviation_resolution([])->
    deviation_resolution_comp;
%%--MM--%%
deviation_resolution([{{deviation,_,_},[]}]) ->
    ok;
%% %% deviation_resolution([{{mim_extended_definitions,{datatype,Type}},Data}|Next]) ->
%% %%     deviation_resolution(Next);
%% %%--MM--%%
%% {deviation,"not-supported",
%%             "/sys:system/sysxe:ldap/sysxe:security/sysxe:transport-security/sysxe:tls/sysxe:tls/sysxe:keepalives"}
deviation_resolution([{{deviation,"not-supported",Target},_Data}|Next]) -> 
    case string:tokens(Target,"/") of
	[TargetClass] ->
	    delete_attrs(TargetClass),
	    %% R = ets:lookup(?YIN_TABLE,{mim_mimdata,{name,TargetClass}}),
	    %% io:format("<<<<<< dev classbody ~p",[R]),
	    ets:delete(?YIN_TABLE,{mim_mimdata,{name,TargetClass}}),
	    ets:delete(?YIN_TABLE,{new_mimdata,{name,TargetClass}}),
	    [{{mim_extended_definitions,yangrootclasses},Roots}] = 
		ets:lookup(?YIN_TABLE,{mim_extended_definitions,yangrootclasses}),
	    NewRoots = Roots -- [TargetClass],
	    ets:insert(?YIN_TABLE,{{mim_extended_definitions,yangrootclasses},NewRoots}),
	    delete_from_class_list(TargetClass);
	Tokens ->
	    %TargetAttr=lists:last(string:tokens(lists:last(Tokens),":")),
	    TargetAttr=lists:last(Tokens),
	    TargetClass=lists:nth(2,lists:reverse(Tokens)),
	    case classify_type(TargetClass, TargetAttr) of
		"class" ->
		    %% FIX ME PB MN: Handle the following scenarios for deleted Class
		    %%     - Deletion of Relations between the deleted class and it's children
		    %%     - Deletion of children MOs of the deleted class ?? 
		    %%       (Required?? Would also mean deltion of the children's attribute abd relations) 
		    TarClass = TargetAttr,
		    ParentClass = TargetClass,
		    delete_attrs(TarClass),
		    ets:delete(?YIN_TABLE,{mim_mimdata,{name,TarClass}}),
		    ets:delete(?YIN_TABLE,{new_mimdata,{name,TarClass}}),
		    delete_from_class_list(TarClass),
		    case ets:lookup(?YIN_TABLE,{mim_mimrelationship,
						{{rel,ParentClass},
						 {type,containment}}}) of
			[{{mim_mimrelationship,{{rel,ParentClass},
						{type,containment}}},Children}] ->
			    NewChildren = Children -- [TarClass],
			    ets:insert(?YIN_TABLE,{{mim_mimrelationship,
						    {{rel,ParentClass},
						     {type,containment}}},
						   NewChildren});
			_ ->
			    ok
		    end;
		"attr" ->
		    TarAttr = list_to_atom(lists:last(string:tokens(TargetAttr,":"))),
		    case ets:lookup(?YIN_TABLE,{mim_mimdata,{name,TargetClass}}) of
			[{{mim_mimdata,{name,TargetClass}},ClassData}] ->
			    case lists:keysearch(attributes,1,ClassData) of
				{value,{attributes,[]}} ->
				    ok;
				{value,{attributes,Attrs}} ->
				    case lists:keysearch(TarAttr,1,Attrs) of
					{value,TgtAttrData={TarAttr,_}} ->
					    Rest = Attrs -- [TgtAttrData],
					    NewClassData=lists:keyreplace(attributes,1,
									  ClassData,{attributes,Rest}),
					    ets:insert(?YIN_TABLE,
						       {{mim_mimdata,{name,TargetClass}},
							NewClassData}),
					    ets:delete(?YIN_TABLE, 
						       {mim_mimdata,{{name,TargetClass}
								     ,{attribute,TarAttr}}}),
					    ets:insert(?YIN_TABLE,
						       {{new_mimdata,{name,TargetClass}},
							NewClassData}),
					    ets:delete(?YIN_TABLE, 
						       {new_mimdata,{{name,TargetClass}
								     ,{attribute,TarAttr}}});
					false ->
					    ok
				    end;
				%% ets:insert(?YIN_TABLE,{{mim_extended_definitions,{datatype,Type}}, BaseType ++ Rest}),
				%% ets:delete(?YIN_DERV_DATA_TABLE,{mim_extended_definitions,{datatype,Type}});
				false ->
				    ok
			    end;
			_ ->
			    error_logger:info_msg("Deviation :: Class Data Not Found ~p",
						  [TargetClass])
		    end;
		"ambiguity" ->
		    error_logger:info_msg("Deviation :: Present as both class and attrubute ~p",
					  [TargetAttr]);
		"not_defined" ->
		    error_logger:info_msg("Deviation :: Not Defined in models ~p/~p", 
					  [TargetClass, TargetAttr])
	    end
    end,
    deviation_resolution(Next);
%% {{deviation,"replace",
%%                                      "/brme:brm/brme:backup-manager/brme:id"},
%%                           {[{{deviate,[{value,"replace"}]},
%%                              [{{config,[{value,"false"}]},[]}]}]}}
deviation_resolution([{{deviation,"add",Target},{[{_,[{{DevKey,[{value,_DevVal}]},[]}]} = DevBody | Rest]}}|Next]) 
  when DevKey == 'max-elements'; DevKey == 'min-elements'->
    %% A deviation with 'max-elements' or 'min-elements' is possible only for a list
    %% as only list supports these sub-statements
    deviate_min_max_elements(Target,DevBody),
    deviation_resolution([{{deviation,"add",Target},{Rest}}]),
    deviation_resolution(Next);
%{{deviation,"replace", "/ks:keystore/ks:asymmetric-keys/ks:asymmetric-key/ks:public-key"}, {[]}}
deviation_resolution([{{deviation,"replace",Target},{[{_,[{{DevKey,[{value,_DevVal}]},[]}]} = DevBody | Rest]}}|Next]) 
  when DevKey == 'max-elements'; DevKey == 'min-elements'->
    %% A deviation with 'max-elements' or 'min-elements' is possible only for a list
    %% as only list supports these sub-statements
    deviate_min_max_elements(Target,DevBody),
    deviation_resolution([{{deviation,"replace",Target},{Rest}}]),
    deviation_resolution(Next);
deviation_resolution([{{deviation,Deviate,Target},{[{_,[{{mandatory,[{value,Val}]},[]}]} | Rest]} = DevBody}|Next]) ->
    case string:tokens(Target,"/") of
	[TgtAttr] ->
	    error_logger:info_msg("Deviation :: ~p - mandatory case: Attribute ~ p does not have Class defined",
				  [Deviate,TgtAttr]);
	Tokens ->
	    [_,TargetAttr] = 
		[list_to_atom(T) || T <- string:tokens(lists:last(Tokens),":")],
	    TargetClass = lists:nth(2,lists:reverse(Tokens)),
	    case ets:lookup(?YIN_TABLE,{mim_mimdata,{name,TargetClass}}) of
		[] ->
		    error_logger:info_msg("Deviation Resolution Class ~p does not exist in db");
		[{_, ClassBody}] ->
		    {value,{AL,AttrList}}=lists:keysearch(attributes,1,ClassBody),
		    {value,{AB,AttrBody}}=lists:keysearch(TargetAttr,1,
							  AttrList),
		    {_,{F,Flags}}=lists:keysearch(flags,1,AttrBody),
		    NewFlags = case {Deviate,Val} of
				   Tuple when Tuple == {"delete","true"}; 
					      Tuple == {"delete","false"};
					      Tuple == {"replace","false"};
					      Tuple == {"add","false"}->
				       Flags -- [mandatory];
				   {_,"true"} ->
				       Flags ++ [mandatory]
			       end,
		    NewAttrBody=(AttrBody -- [{F,Flags}]) ++ [{F,NewFlags}],
		    NewAttrList=(AttrList -- [{AB,AttrBody}]) ++ [{AB,NewAttrBody}],
		    NewClassBody=(ClassBody -- [{AL,AttrList}])++[{AL,NewAttrList}],
		    yin_table_deviation_update(TargetClass,TargetAttr,
					       NewClassBody,NewAttrBody)
	    end
    end,
    deviation_resolution([{{deviation,Deviate,Target},{Rest}}]),
    deviation_resolution(Next);
%{{deviation,"replace","/nacm:nacm/tacm:cmd-read-default"}, [{{deviate,[{value,"replace"}]},[{{default,[{value,"deny"}]},[]}]}]}
deviation_resolution([{{deviation,Deviate,Target},{[{_,[{{default,[{value,Val}]},[]}]} | Rest]}}|Next]) ->
    case string:tokens(Target,"/") of
	[TgtAttr] ->
	    error_logger:info_msg("Deviation :: ~p - default : Attribute ~ p does not have Class defined",
				  [Deviate,TgtAttr]);
	Tokens ->
	    [_,TargetAttr] = 
		[list_to_atom(T) || T <- string:tokens(lists:last(Tokens),":")],
	    TargetClass = lists:nth(2,lists:reverse(Tokens)),
	    case ets:lookup(?YIN_TABLE,{mim_mimdata,{name,TargetClass}}) of
		[] ->
		    error_logger:info_msg("Deviation Resolution Class ~p does not exist in db");
		[{_, ClassBody}] ->
		    {value,{AL,AttrList}}=lists:keysearch(attributes,1,ClassBody),
		    {value,{AB,AttrBody}}=lists:keysearch(TargetAttr,1,
							  AttrList),
		    %{Value,{F,Flags}}=lists:keysearch(flags,1,AttrBody),
		    NewAttrBody = 
			case Deviate of
			    "delete" ->
				lists:keydelete(defaultvalue,1,AttrBody);
			    _Else ->
				case lists:keysearch(defaultvalue,1,AttrBody) of
				    false ->
					[{defaultvalue,Val}] ++ AttrBody;
				    _ ->
					lists:keyreplace(defaultvalue,1,
							 AttrBody,{defaultvalue,Val})
				end
			end,
		    NewAttrList=(AttrList -- [{AB,AttrBody}]) ++ [{AB,NewAttrBody}],
		    NewClassBody=(ClassBody -- [{AL,AttrList}])++[{AL,NewAttrList}],
		    yin_table_deviation_update(TargetClass,TargetAttr,
					       NewClassBody,NewAttrBody)
	    end
    end,
    deviation_resolution([{{deviation,Deviate,Target},{Rest}}]),
    deviation_resolution(Next);
deviation_resolution([{{deviation,Deviate,Target},
		       {[{_,[{{config,[{value,Val}]},[]}]} = Dev | Rest]}}|Next]) ->
    case string:tokens(Target,"/") of
	[TargetClass] ->
	    case ets:lookup(?YIN_TABLE,{mim_mimdata,{name,TargetClass}}) of
		[] ->
		    error_logger:info_msg("Deviation Resolution Class ~p does not exist in db",
					 [TargetClass]);
		[{_, ClassBody}] ->
		    NewClassBody = 
			case Deviate of
			    "delete" ->
				lists:keydelete(config,1,ClassBody);
			    _Else ->
				case lists:keysearch(config,1,ClassBody) of
				    false ->
					[{config,Val}] ++ ClassBody;
				    _ ->
					lists:keyreplace(config,1,ClassBody,
							 {config,Val})
				end
			end,
		    ets:delete(?YIN_TABLE,{mim_mimdata,
					   {name,TargetClass}}),
		    ets:insert(?YIN_TABLE,
			       {{mim_mimdata,{name,TargetClass}},
				NewClassBody})
	    end;
	Tokens ->
	    TargetAttr=lists:last(Tokens),
	    TargetClass=lists:nth(2,lists:reverse(Tokens)),
	    case classify_type(TargetClass, TargetAttr) of
		"class" ->
		    TarClass = TargetAttr,
		    case ets:lookup(?YIN_TABLE,{mim_mimdata,{name,TarClass}}) of
			[] ->
			    error_logger:info_msg("Deviation Resolution Class ~p does not exist in db",
						 [TarClass]);
			[{_, ClassBody}] ->
			    NewClassBody = 
				case Deviate of
				    "delete" ->
					lists:keydelete(config,1,ClassBody);
				    _Else ->
					case lists:keysearch(config,1,ClassBody) of
					    false ->
						[{config,Val}] ++ ClassBody;
					    _ ->
						lists:keyreplace(config,1,ClassBody,
								 {config,Val})
					end
				end,
			    ets:delete(?YIN_TABLE,{mim_mimdata,
						   {name,TarClass}}),
			    ets:insert(?YIN_TABLE,
				       {{mim_mimdata,{name,TarClass}},
					NewClassBody}),
			     ets:delete(?YIN_TABLE,{new_mimdata,
						   {name,TarClass}}),
			    ets:insert(?YIN_TABLE,
				       {{new_mimdata,{name,TarClass}},
					NewClassBody})
		    end;
		"attr" ->
		    TarAttr = list_to_atom(lists:last(string:tokens(TargetAttr,":"))),
		    case ets:lookup(?YIN_TABLE,{mim_mimdata,{name,TargetClass}}) of
			[] ->
			    error_logger:info_msg("Deviation Resolution Class ~p does not exist in db",
						 [TargetClass]);
			[{_, ClassBody}] ->
			    {value,{AL,AttrList}}=
				lists:keysearch(attributes,1,ClassBody),
			    {value,{AB,AttrBody}}=
				lists:keysearch(TarAttr,1, AttrList),
			    NewAttrBody = 
				case Deviate of
				    "delete" ->
					lists:keydelete(config,1,
							AttrBody);
				    _Else ->
					case lists:keysearch(config,1,AttrBody) of
					    false ->
						[{config,Val}] ++ AttrBody;
					    _ ->
						lists:keyreplace(config,1,
								 AttrBody,
								 {config,Val})
					end
				end,
			    NewAttrList = (AttrList -- [{AB,AttrBody}])
				++ [{AB,NewAttrBody}],
			    NewClassBody=(ClassBody -- [{AL,AttrList}])
				++[{AL,NewAttrList}],
			    yin_table_deviation_update(TargetClass,
						       TarAttr,
						       NewClassBody,
						       NewAttrBody)
		    end;
		"ambiguity" ->
		    error_logger:info_msg("Deviation :: Present as both class and attrubute ~p",
					  [TargetAttr]);
		"not_defined" ->
		    error_logger:info_msg("Deviation :: Not Defined in models ~p/~p", 
					  [TargetClass, TargetAttr])
	    
	    end
    end,
    deviation_resolution([{{deviation,Deviate,Target},{Rest}}]),
    deviation_resolution(Next);
deviation_resolution([{{deviation,Deviate,Target},{[{_,[{{type,[{name,Type}]},[]}]} | Rest]}}|Next]) ->
    ResolvedType = process_leaf_typedef_body([{{type,[{name,Type}]},[]}],[],""),
    case string:tokens(Target,"/") of
	[TgtAttr] ->
	    error_logger:info_msg("Deviation :: ~p - type case: Attribute ~ p does not have Class defined",
				  [Deviate,TgtAttr]);
	Tokens ->
	    [_,TargetAttr] = 
		[list_to_atom(T) || T <- string:tokens(lists:last(Tokens),":")],
	    TargetClass = lists:nth(2,lists:reverse(Tokens)),
	    case ets:lookup(?YIN_TABLE,{mim_mimdata,{name,TargetClass}}) of
		[] ->
		    error_logger:info_msg("Deviation Resolution Class ~p does not exist in db");
		[{_, ClassBody}] ->
		    {value,{AL,AttrList}}=lists:keysearch(attributes,1,ClassBody),
		    {value,{AB,AttrBody}}=lists:keysearch(TargetAttr,1,AttrList),
		    case lists:keysearch(type,1,AttrBody) of
			false ->
			    error_logger:info_msg("Deviation Major Error in type case. How can an attribute ~p not have a type? Preposterous!!");
			{value,{type,CurrVal}} ->
			    if CurrVal == ResolvedType ->
				    error_logger:info_msg("Deviation: Useless Deviation found. Type is same as before.");
			       true ->
				    NewAttrBody = lists:keyreplace(type,1,AttrBody,
								   {type,ResolvedType}),
				    NewAttrList=(AttrList -- [{AB,AttrBody}]) ++ 
					[{AB,NewAttrBody}],
				    NewClassBody=(ClassBody -- [{AL,AttrList}])++
					[{AL,NewAttrList}],
				    yin_table_deviation_update(TargetClass,TargetAttr,
							       NewClassBody,NewAttrBody)
			    end
		    end
	    end
    end;
deviation_resolution([{{deviation,Deviate,Target},{[{_,[{{units,[{name,Units}]},[]}]} | Rest]}}|Next]) ->
    case string:tokens(Target,"/") of
	[TgtAttr] ->
	    error_logger:info_msg("Deviation :: ~p - units: Attribute ~ p does not have Class specified",
				  [Deviate,TgtAttr]);
	Tokens ->
	    [_,TargetAttr] = 
		[list_to_atom(T) || T <- string:tokens(lists:last(Tokens),":")],
	    TargetClass = lists:nth(2,lists:reverse(Tokens)),
	    case ets:lookup(?YIN_TABLE,{mim_mimdata,{name,TargetClass}}) of
		[] ->
		    error_logger:info_msg("Deviation Resolution Class ~p does not exist in db");
		[{_, ClassBody}] ->
		    {value,{AL,AttrList}}=lists:keysearch(attributes,1,ClassBody),
		    {value,{AB,AttrBody}}=lists:keysearch(TargetAttr,1,AttrList),
		    case lists:keysearch(units,1,AttrBody) of
			false ->
			    NewAttrBody = [{units,Units}] ++ AttrBody,
			    NewAttrList=(AttrList -- [{AB,AttrBody}]) ++ 
					[{AB,NewAttrBody}],
				    NewClassBody=(ClassBody -- [{AL,AttrList}])++
					[{AL,NewAttrList}],
				    yin_table_deviation_update(TargetClass,TargetAttr,
							       NewClassBody,NewAttrBody);
			{value,{type,CurrVal}} ->
			    if CurrVal == Units ->
				    error_logger:info_msg("Deviation: Useless Deviation found. Units is same as before.");
			       true ->
				    NewAttrBody = lists:keyreplace(units,1,AttrBody,
								   {units,Units}),
				    NewAttrList=(AttrList -- [{AB,AttrBody}]) ++ 
					[{AB,NewAttrBody}],
				    NewClassBody=(ClassBody -- [{AL,AttrList}])++
					[{AL,NewAttrList}],
				    yin_table_deviation_update(TargetClass,TargetAttr,
							       NewClassBody,NewAttrBody)
			    end
		    end
	    end
    end;
deviation_resolution([{{deviation,_,_},_} = Dev|Next]) ->
    error_logger:info_msg("~nDeviation not handled ~p~n", [Dev]),
    deviation_resolution(Next).

%% deviation_resolution([{{deviation,"add",Target},Data}|Next]) -> 
%%     io:format("yyyyyyy data ~p",[Data]),
%%     %%{[{{deviate,[{value,"add"}]},[{{'max-elements',[{value,"8192"}]},[]}]}]} = Data
%%     case Data of
%% 	{[{_,[{{'max-elements',[{value,_}]},[]}] = DevVal}]} ->
%% 	    case string:tokens(Target,"/") of
%% 		[TgtClass] ->
%% 		    ClassBody = ets:lookup(?YIN_TABLE,{mim_mimdata,{name,TgtClass}}),
%% 		    io:format("<<<<<< dev classbody ~p",[ClassBody]);
%% 		Tokens ->
%% 		    io:format("xxxxx tokens ~p",[Tokens]),
%% 		    TgtAttr=lists:last(Tokens),
%% 		    TgtClass = lists:nth(2,lists:reverse(Tokens)),
%% 		    case classify_type(TgtClass,TgtAttr) of
%% 			"class" ->
%% 			    TarClass = TgtAttr,
%% 			    ParentClass = TgtClass,
%% 			    [{_,CB}] = 
%% 				ets:lookup(?YIN_TABLE,{mim_mimdata,{name,TgtClass}}),
%% 			    prepare_relations_cardinality(ParentClass,TarClass,CB++DevVal),
%% 			    io:format("<<<<<< dev classbody ~p",[CB]);
%% 			"attr" ->
%% 			    ok;
%% 			"ambiguity" ->
%% 			    error_logger:info_msg("Deviation :: Present as both class and attrubute ~p",[TgtAttr]);
%% 			"not_defined" ->
%% 			    error_logger:info_msg("Deviation :: Not Defined in models ~p/~p", 
%% 						  [TgtClass, TgtAttr])
%% 		    end
%% 	    end;
%% 	_Else ->
%% 	    ok
%%     end,
%%    deviation_resolution(Next);

delete_attrs(TarClass) ->
    case ets:lookup(?YIN_TABLE,{mim_mimdata,{name,TarClass}}) of
	[{{mim_mimdata,{name,TarClass}},ClassData}] ->
	    case lists:keysearch(attributes,1,ClassData) of
		{value,{attributes,[]}} ->
		    ok;
		{value,{attributes,Attrs}} ->
		    [ets:delete(?YIN_TABLE, 
				{mim_mimdata,{{name,TarClass},{attribute,Attr}}}) ||
			{Attr,_} <- Attrs]
	    end;
	_ ->
	    ok
    end.

delete_from_class_list(TarClass)->
    [{{mim_mimdata,names},Classes}] = 
			ets:lookup(?YIN_TABLE,{mim_mimdata,names}),
		    NewClasses = Classes -- [TarClass],
		    ets:insert(?YIN_TABLE,{{mim_mimdata,names},NewClasses}).
    

classify_type(ParentTag,Tag) ->
    case [is_class_defined(Tag),
	  is_class_attr_defined(ParentTag, 
				list_to_atom(lists:last(string:tokens(Tag,":"))))] of
	[true,false] ->
	    "class";
	[false,true] ->
	    "attr";
	[true,true] ->
	    "ambiguity";
	[_,_] ->
	    "not_defined"
    end.
    
is_class_defined(Class) ->
    case ets:lookup(?YIN_TABLE,{mim_mimdata,{name,Class}}) of
	[{{mim_mimdata,{name,Class}},_}] ->
	    true;
	_ ->
	    false
    end.

is_class_attr_defined(Class, Attr) ->
    case ets:lookup(?YIN_TABLE, {mim_mimdata,{{name,Class},{attribute,Attr}}}) of
	[] ->
	    false;
	_ ->
	    true
    end.

deviate_min_max_elements(Target,{_,[{{DevKey,[{value,DevVal}]},[]}]}) ->
    case string:tokens(Target,"/") of
	[TgtClass] ->
	    error_logger:info_msg("Deviation :: add - ~p : Root List ~ p does not have cardinality to set",
				 [DevKey,TgtClass]);
	Tokens ->
	    TargetClass = lists:last(Tokens),
	    ParentClass = lists:nth(2,lists:reverse(Tokens)),
	    case ets:lookup(?YIN_TABLE,
			    {mim_mimrelationship,
			     {{rel,ParentClass,TargetClass},{type,containment}}}) of
		[] ->
		    case DevKey of
			'max-element' ->
			    ets:insert(?YIN_TABLE,
				       {{mim_mimrelationship,
					 {{rel,ParentClass,TargetClass},
					  {type,containment}}},
					[{name,ParentClass ++ "_to_" ++ TargetClass},
					 {min,0},{max,list_to_integer(DevVal)}]});
			'min-element' ->
			    ets:insert(?YIN_TABLE,
				       {{mim_mimrelationship,
					 {{rel,ParentClass,TargetClass},
					  {type,containment}}},
					[{name,ParentClass ++ "_to_" ++ TargetClass},
					 {min,list_to_integer(DevVal)},{max,unlimited}]})
		    end;
		[{_, [_,{min,CurrMin},{max,CurrMax}]}] ->
		    ets:delete(?YIN_TABLE,
			       {mim_mimrelationship,
				{{rel,ParentClass,TargetClass},{type,containment}}}),
		    case DevKey of
			'max-elements' ->
			    ets:insert(?YIN_TABLE,
				       {{mim_mimrelationship,
					 {{rel,ParentClass,TargetClass},
					  {type,containment}}},
					[{name,ParentClass ++ "_to_" ++ TargetClass},
					 {min,CurrMin},{max,list_to_integer(DevVal)}]});
			'min-elements' ->
			    ets:insert(?YIN_TABLE,
				       {{mim_mimrelationship,
					 {{rel,ParentClass,TargetClass},
					  {type,containment}}},
					[{name,ParentClass ++ "_to_" ++ TargetClass},
					 {min,list_to_integer(DevVal)},{max,CurrMax}]})
		    end
	    end
    end.

yin_table_deviation_update(TargetClass,TargetAttr,
			NewClassBody,NewAttrBody) ->   
    ets:delete(?YIN_TABLE,{mim_mimdata,{name,TargetClass}}),
    ets:insert(?YIN_TABLE,
	       {{mim_mimdata,{name,TargetClass}},NewClassBody}),
    ets:delete(?YIN_TABLE,{new_mimdata,{name,TargetClass}}),
    ets:insert(?YIN_TABLE,
	       {{new_mimdata,{name,TargetClass}},NewClassBody}),
    ets:delete(?YIN_TABLE, 
	       {mim_mimdata,{{name,TargetClass},{attribute,TargetAttr}}}),
            ets:insert(?YIN_TABLE, 
		       {{mim_mimdata,{{name,TargetClass},{attribute,TargetAttr}}},
			NewAttrBody}),
    %% ets:delete(?YIN_TABLE,{new_mimdata,{name,TargetClass}}),
    %% ets:insert(?YIN_TABLE,
    %%       {{new_mimdata,{name,TargetClass}},NewClassBody}),
    ets:delete(?YIN_TABLE, 
	       {new_mimdata,{{name,TargetClass},{attribute,TargetAttr}}}),
    ets:insert(?YIN_TABLE, 
	       {{new_mimdata,{{name,TargetClass},{attribute,TargetAttr}}},
		NewAttrBody}).

derived_types_table_resolution() ->
    %% ets:select(yinderiveddt, [{{'_','_'},[],['$_']}]).
    DDataType=ets:select(?YIN_DERV_DATA_TABLE, [{{'_','_'},[],['$_']}]),
    derived_type_resolution(DDataType).
derived_type_resolution([])->
    dervied_datatypes_resolution_comp;

%% derived_type_resolution([{{mim_extended_definitions,{datatype,Type}},[{type,DDType}|Rest]}|Next]) -> 
%% %% [{{mim_extended_definitions,{datatype,"tailf:size"}},
%% %%                       [{desc,<<"A value that represents a number of bytes.  An example could be\nS1G8M7K956B; meaning 1GB + 8"...>>},
%% %%                        {type,string},
%% %%                        {typeinfo,[]},
%% %%                        {pattern,"S(\\d+G)?(\\d+M)?(\\d+K)?(\\d+B)?"}]}]
%%     case ets:lookup(?YIN_TABLE,{mim_extended_definitions,{datatype,atom_to_list(DDType)}}) of
%% 	[{{mim_extended_definitions,{datatype,_Typ}},[{type,BaseType}|Other]}] ->
%% 	    ets:insert(?YIN_TABLE,{{mim_extended_definitions,{datatype,Type}},[{type,BaseType}] ++ Other ++ Rest}),
%% 	    ets:delete(?YIN_DERV_DATA_TABLE,{mim_extended_definitions,{datatype,Type}});
%% 	[] ->
%% 	    ok
%%     end,
%%     derived_type_resolution(Next).
derived_type_resolution([{{mim_extended_definitions,{datatype,Type}},Data}|Next]) -> 
    case lists:keysearch(type,1,Data) of
	{value,{type,DDType}} ->
	    Rest=Data--[{type,DDType}],
	    case ets:lookup(?YIN_TABLE,{mim_extended_definitions,{datatype,atom_to_list(DDType)}}) of
		[{{mim_extended_definitions,{datatype,_Typ}},BaseType}] ->
		    ets:insert(?YIN_TABLE,{{mim_extended_definitions,{datatype,Type}}, BaseType ++ Rest}),
		    ets:delete(?YIN_DERV_DATA_TABLE,{mim_extended_definitions,{datatype,Type}});
		[] ->
		    ok
	    end;
	_ ->
	    io:format("DataType Not Found"),
	    ok
    end,
    derived_type_resolution(Next);
derived_type_resolution([_|Next]) ->
    derived_type_resolution(Next).

process_enumeration_body([]) ->
    [];
process_enumeration_body([{{enum,[{name,EnumName}]},EnumBody}|ERemBody])->		    
    [{EnumName,process_enum_body(EnumBody,[])}] ++ process_enumeration_body(ERemBody).

process_enum_body([],EnumBody) ->
    EnumBody;
process_enum_body([{{value,[{value,Value}]},[]}|RemBody],AccEnumBody) ->
    process_enum_body(RemBody,[{flags,[]},{typeinfo,integer},{value,list_to_integer(Value)}]++AccEnumBody); 
process_enum_body([{{description,[]},[{{text,[]},Desc}]}|RemBody],Acc) ->
    process_enum_body(RemBody,[{desc,list_to_binary(Desc)}]++Acc); 
process_enum_body([Foriegn|RemBody],Acc) ->
    io:format("~n Foriegn ENUM body mem ~p",[Foriegn]),
    process_enum_body(RemBody,Acc). 

process_uniontype_body([],UnionTypes,_,_,_)->
    UnionTypes;
process_uniontype_body([{{type,[{name,"enumeration"}]},_}=Type|Rest],UnionTypes,Prfx,UType,Cntr) ->
    io:format("UnionEnumeration ~p :::::::::~n",[Type]),
    Type1=reslove_union_types([Type],Prfx,UType,Cntr),	
    process_uniontype_body(Rest,Type1++UnionTypes,Prfx,UType,Cntr+1);
process_uniontype_body([Type|Rest],UnionTypes,Prfx,UType,Cntr) ->
    %%io:format("UnionEnum ~p :::::::::~n",[Type]),
    Type1= case process_typedef_body([Type],[]) of
	       {derived, DTBody} ->
		   reslove_union_types(DTBody,Prfx,UType,Cntr);
	       DDTBody ->
		   [DDTBody]
	   end,
    process_uniontype_body(Rest,Type1++UnionTypes,Prfx,UType,Cntr+1).

reslove_union_types([{{type,[{name,"enumeration"}]},EnumMemBody}],Prfx,UnionType,Cntr) ->
    io:format("Union---Enumeration~n"),
    DerivedType = Prfx ++ ":" ++ UnionType++integer_to_list(Cntr),
    EnumValue={enummembers,process_enumeration_body(EnumMemBody)},
    ets:insert(?YIN_TABLE,{{mim_extended_definitions,{enum,DerivedType}},EnumValue}),
    ets_key_value_append({mim_extended_definitions,enums},DerivedType),
    [{enum,DerivedType}];

reslove_union_types([{type,DerType}|_Rest],Prfx,_,_Cntr) ->
    DerivedType = case re:run(atom_to_list(DerType), ":") of
		      nomatch ->
			  Prfx ++ ":" ++ atom_to_list(DerType);
		      _Type when is_atom(DerType) ->
			  atom_to_list(DerType);
		      _other ->
			  DerType
		  end,
   
    case ets:lookup(?YIN_TABLE,{mim_extended_definitions,{datatype,DerivedType}}) of
	[] ->
	    case ets:lookup(?YIN_TABLE,{mim_extended_definitions,{enum,DerivedType}}) of
		[] ->
		    case ets:lookup(?YIN_TABLE,{mim_extended_definitions,{union,DerivedType}}) of
			[] ->
			    io:format("~n Derived datatype MISSING for union ~p~n",[DerivedType]),
			    ["eeee"++DerivedType];
			_ ->
			    [{type,{derivedunion,DerivedType}}]
		    end;
		_->
		    [{type,{derivedenum,DerivedType}}]
	    end;
	[{_,Value}] ->
	    Value
    end.


process_container_list_rels(_,[],_, _) ->
    [];
process_container_list_rels([],{{?YIN_CONTAINER,[{name,Name}]},Body},Prfx, Reln) -> 
    case lists:keysearch({status,[{value,"obsolete"}]}, 1, Body) of
	false ->
	    {ClassBody,RemBody} = process_container_body(Body,[],[],Name,Prfx,[],[]),
	    NewClassBody = prepare_relations_cardinality_con("ComTop:ManagedElement",
							     A = Prfx++":"++Name,
							     ClassBody),
	    append_or_add_attributes_actions_to_class(A,NewClassBody,attributes),
	    append_or_add_attributes_actions_to_class(A,NewClassBody,actions),
	    %% ets:insert(?YIN_TABLE,{{mim_mimdata,{name,A}},NewClassBody++[{actions,[]}]}),
	    %% ets:insert(?YIN_TABLE,{{new_mimdata,{name,A}},NewClassBody++[{actions,[]}]}),
	    ets_key_value_append({mim_extended_definitions,yangrootclasses},
				 Prfx++":"++Name), 
	    ets_key_value_append({mim_mimdata,names},A),
	    %% ets:insert(?YIN_TABLE,{{mim_mimdata,{name,A = Prfx++":"++Name}},ClassBody}),
	    %% ets:insert(?YIN_TABLE,{{mim_mimrelationship,{{rel,"ComTop:ManagedElement",A},{type,containment}}},
	    %% 			   [{name,"ComTop:ManagedElement_to_"++A},{min,1},{max,1}]}),
	    ets_key_value_append({mim_mimrelationship,
				  {{rel,"ComTop:ManagedElement"},{type,containment}}},A),
	    process_container_list_rels(A,RemBody,Prfx,Reln);
	_ ->
	    []
    end;
process_container_list_rels([],{{?YIN_LIST,[{name,Name}]},Body},Prfx, Reln) -> 
    case lists:keysearch({status,[{value,"obsolete"}]}, 1, Body) of
	false ->
	    {ClassBody,RemBody} = process_container_body(Body,[],[],Name, Prfx,[],[]),
	    NewClassBody = prepare_relations_cardinality("ComTop:ManagedElement",
							 A = Prfx++":"++Name, ClassBody),
	    append_or_add_attributes_actions_to_class(A,NewClassBody,attributes),
	    append_or_add_attributes_actions_to_class(A,NewClassBody,actions),
    %% ets:insert(?YIN_TABLE,{{mim_mimdata,{name,A}},NewClassBody++[{actions,[]}]}),
    %% ets:insert(?YIN_TABLE,{{new_mimdata,{name,A}},NewClassBody++[{actions,[]}]}),
	    ets_key_value_append({mim_extended_definitions,rootyangclass},A),
	    ets_key_value_append({mim_mimdata,names},A),
	    ets_key_value_append({mim_mimrelationship,
				  {{rel,"ComTop:ManagedElement"},{type,containment}}},A),
	    process_container_list_rels(A,RemBody,Prfx,Reln);
	_ ->
	    []
    end;
process_container_list_rels(Parent,[{{?YIN_CONTAINER,[{name,Name}]},Body}|Rest],
			    Prfx, Reln) ->
    case lists:keysearch({status,[{value,"obsolete"}]}, 1, Body) of
	false ->
	    {ClassBody,RemBody} = process_container_body(Body,[],[],Name,Prfx,[],[]),
	    NewClassBody = prepare_relations_cardinality_con(Parent,
							     A = Prfx++":"++Name,
							     ClassBody),
	    append_or_add_attributes_actions_to_class(A,NewClassBody,attributes),
	    append_or_add_attributes_actions_to_class(A,NewClassBody,actions),
	    ets_key_value_append({mim_mimrelationship,
				  {{rel,Parent},{type,containment}}},A),
    %% ets:insert(?YIN_TABLE,{{mim_mimdata,{name,A}},NewClassBody++[{actions,[]}]}),
    %% ets:insert(?YIN_TABLE,{{new_mimdata,{name,A}},NewClassBody++[{actions,[]}]}),
	    ets_key_value_append({mim_mimdata,names},A),
	    process_container_list_rels(A,RemBody,Prfx,Reln), 
	    process_container_list_rels(Parent,Rest,Prfx,Reln);
	_ ->
	    process_container_list_rels(Parent,Rest,Prfx,Reln)
    end;
process_container_list_rels(Parent, [{{?YIN_LIST,[{name,Name}]},Body}|Rest],Prfx, Reln) ->
    case lists:keysearch({status,[{value,"obsolete"}]}, 1, Body) of
	false ->
	    {ClassBody,RemBody} = process_container_body(Body,[],[],Name,Prfx,[],[]),
	    NewClassBody = prepare_relations_cardinality(Parent,
							 A = Prfx++":"++Name,ClassBody),
	    ets_key_value_append({mim_mimrelationship,
				  {{rel,Parent},{type,containment}}},A),
	    append_or_add_attributes_actions_to_class(A,NewClassBody,attributes),
	    append_or_add_attributes_actions_to_class(A,NewClassBody,actions),
    %% ets:insert(?YIN_TABLE,{{mim_mimdata,{name,A}},NewClassBody++[{actions,[]}]}),
    %% ets:insert(?YIN_TABLE,{{new_mimdata,{name,A}},NewClassBody++[{actions,[]}]}),
	    ets_key_value_append({mim_mimdata,names},A),
    %%FIX ME get cardinality    -- FIXED
	    process_container_list_rels(A,RemBody,Prfx,Reln),
	    process_container_list_rels(Parent,Rest,Prfx,Reln);
	_ ->
	    process_container_list_rels(Parent,Rest,Prfx,Reln)
    end;
                     %% {{deviation,
                     %%     [{'target-node',
                     %%          "/if:interfaces/if:interface/if-cmn:l2-mtu"}]},
                     %% [{{deviate,[{value,"add"}]},
                     %%   [{{must,
                     %%         [{condition,
                     %%              "../if:type != 'ianaift:softwareLoopback'"}]},
                     %%     [{{'error-message',[]},
                     %%       [{{value,[]},
                     %%         "The l2-mtu cannot be configured for loopback interface"}]},
                     %%      {{description,[]},
                     %%       [{{text,[]},
                     %%         "The l2-mtu cannot be configured for loopback interface"}]}]}]}]}


%% [{{description,[]},
%%                                                                                                   [{{text,[]},
%%                                                                                                     "Certificate expiration notification is sent as fault indication instead,\ntriggering alarms."}]},
%%                                                                                                  {{deviate,[{value,"not-supported"}]},[]}]
process_container_list_rels(Parent, Devia=[{{description,[]},_}|Rest],Prfx, {deviation,TrgtNode}) ->
    %% io:format(" deviation type ~p ~n ",[Devia]),    
    %% case Type of
    %% 	"add" ->
    %% 	    io:format("Deviation add has to be implemented");
    %% 	"not-supported" ->
    %% 	   ets:insert(?YIN_DERV_DATA_TABLE,{{deviation,Type},{Rest}});
    %% 	"delete" ->
    %% 	    ets:insert(?YIN_DERV_DATA_TABLE,{{deviation,Type},{Rest}});
    %% 	Any ->
    %% 	    ok
    %% end,
    process_container_list_rels(Parent,Rest,Prfx, {deviation,TrgtNode});
%process_container_list_rels("nacm:read-default",[{{deviate, [{value,"replace"}]}, [{{config, [{value, "false"}]}, []}]}, {{deviate, [{value, "replace"}]}, [{{default, [{value, "deny"}]}, []}]}],  "nacmxe", {deviation, "/nacm:nacm/nacm:read-default"})
process_container_list_rels(Parent, Devia=[{{deviate,[{value,Type}]},_}|_],Prfx, {deviation,TrgtNode}) ->
    io:format(" deviation type ~p ~n ",[Devia]),    
    case Type of
	"add" ->
	   %% io:format("Deviation add has to be implemented");
	    ets:insert(?YIN_DERV_DATA_TABLE,{{deviation,Type,TrgtNode},{Devia}});
	"not-supported" ->
	    ets:insert(?YIN_DERV_DATA_TABLE,{{deviation,Type,TrgtNode},{Devia}});
	"delete" ->
	    ets:insert(?YIN_DERV_DATA_TABLE,{{deviation,Type,TrgtNode},{Devia}});
	"replace" ->
	    ets:insert(?YIN_DERV_DATA_TABLE,{{deviation,Type,TrgtNode},{Devia}})
    end;
    %%--MM--%%
    %%process_container_list_rels(Parent, Rest,Prfx, {deviation,TrgtNode});
    %%--MM--%%

	    %% NewAttrs=lists:keyreplace(AttrName,1,Attrs,{AttrName,KeyAttrBody}),
	    %% ets:insert(?YIN_TABLE, {{mim_mimdata,{{name,Child},{attribute,AttrName}}},KeyAttrBody}),
	    %% ets:insert(?YIN_TABLE, {{new_mimdata,{{name,Child},{attribute,AttrName}}},KeyAttrBody}),
	    %% lists:keyreplace(attributes,1,ClassBody,{attributes,NewAttrs})--Key
   %% end,
    %% GrpValue=case ets:lookup(?YIN_TABLE,GrpKey) of
    %% 		 [] ->
    %% 		     io:format(" Group Not Found --~p ~n ",[GrpKey]),
    %% 		     case ets:match(?YIN_TABLE,{{'_',GrpName},'$1'}) of
    %% 			 []->
    %% 			     io:format(" Nested Group tag Not Found --~p~n",[GrpName]),
    %% 			     [];
    %% 			 [Head|_] ->
    %% 			     lists:flatten(Head)
    %% 		     end;
    %% 		 [{_Key,Value}] ->
    %% 		     Value
    %% 	     end,
     %%process_container_list_rels(Parent,GrpValue++Rest,Prfx,{deviation,TrgtNode});

process_container_list_rels(Parent, [{{uses,[{name,GrpName}]},_}|Rest],Prfx, {augment,TrgtNode}) ->
    io:format(" augment uses ~p ~n ",[{Prfx,GrpName}]),
    GrpKey = case re:run(GrpName, ":") of
		 %% nomatch when is_atom(GrpName) ->
		 %%     {Prfx, atom_to_list(GrpName)};
		 nomatch ->
		     {Prfx, GrpName};
		 _ -> [Pfx,GrName] =string:tokens(GrpName,":"),
		      {Pfx,GrName}
	     end,   
    GrpValue=case ets:lookup(?YIN_TABLE,GrpKey) of
		 [] ->
		     io:format(" Group Not Found --~p ~n ",[GrpKey]),
		     case ets:match(?YIN_TABLE,{{'_',GrpName},'$1'}) of
			 []->
			     io:format(" Nested Group tag Not Found --~p~n",[GrpName]),
			     [];
			 [Head|_] ->
			     lists:flatten(Head)
		     end;
		 [{_Key,Value}] ->
		     Value
	     end,
     process_container_list_rels(Parent,GrpValue++Rest,Prfx,{augment,TrgtNode});
process_container_list_rels(Parent, [{{?YIN_LEAF,[{name,LeafName}]},LeafBody}|Rest],
			    Prfx, {augment,TrgtNode}) ->
    case lists:keysearch({status,[{value,"obsolete"}]}, 1, LeafBody) of
       false ->
	    LeafDetails=
		[{Leaf=list_to_atom(LeafName),
		  AttrBody=process_leaf_typedef_body(LeafBody,[{flags,[]}],Prfx)}],
	    case ets:lookup(?YIN_TABLE,{mim_mimdata,{name,TrgtNode}}) of
		[{_Key,ClassBody}] ->
		    NewBaseBody = case lists:keysearch(attributes,1,ClassBody) of
				      false ->
					  ClassBody;
				      {value,{attributes,Attrs}} ->
					  lists:keyreplace(attributes,1,ClassBody,
							   {attributes,LeafDetails++Attrs})
				  end,	 
		    ets:insert(?YIN_TABLE, {{mim_mimdata,{{name,TrgtNode},
							  {attribute,Leaf}}},AttrBody}),
		    ets:insert(?YIN_TABLE, {{new_mimdata,{{name,TrgtNode},
							  {attribute,Leaf}}},AttrBody}),
		    append_or_add_attributes_actions_to_class(TrgtNode,
							      NewBaseBody,attributes);
	    %% ets:insert(?YIN_TABLE, {{mim_mimdata,{name,TrgtNode}},NewBaseBody}),
	    %% ets:insert(?YIN_TABLE, {{new_mimdata,{name,TrgtNode}},NewBaseBody});
		[]->
		    io:format("Target node details not loaded ~p ~n",[TrgtNode])
	    end,
	    process_container_list_rels(Parent,Rest,Prfx,{augment,TrgtNode});
	_ ->
	    process_container_list_rels(Parent,Rest,Prfx,{augment,TrgtNode})
    end;
process_container_list_rels(Parent, [{{?YIN_ACTION,[{name,ActName}]},ActBody}|Rest],
			    Prfx, {augment,TrgtNode}) ->
    case lists:keysearch({status,[{value,"obsolete"}]}, 1, ActBody) of
	false ->
	    ActDetails = 
		[{ActName,process_action_body(ActBody,[{flags,[]}])}],
	    case ets:lookup(?YIN_TABLE,{mim_mimdata,{name,TrgtNode}}) of
		[{_Key,ClassBody}] ->
		    NewBaseBody = case lists:keysearch(actions,1,ClassBody) of
				      false ->
					  ClassBody ++ [{actions,ActDetails}];
				      {value,{actions,Actions}} ->
					  lists:keyreplace(actions,1,ClassBody,
							   {actions,ActDetails++Actions})
				  end,
		    append_or_add_attributes_actions_to_class(TrgtNode,
							      NewBaseBody,actions);
		[] ->
		    io:format("In Actions: Target node details not loaded ~p ~n",
			      [TrgtNode])
	    end,
	    process_container_list_rels(Parent,Rest,Prfx,{augment,TrgtNode});
	_ ->
	    process_container_list_rels(Parent,Rest,Prfx,{augment,TrgtNode})
    end;

process_container_list_rels(Parent,[Tag|Rest],Prfx,{augment,TrgtNode}) ->
    io:format("~naugmenttag ~p~n",[Tag]),
    process_container_list_rels(Parent,Rest,Prfx,{augment,TrgtNode}).


process_action_body([],Res) ->
    Res;
process_action_body([{{description,[]},[{{text,[]},Desc}]}|Rem],Res) ->
    process_action_body(Rem,Res ++ [{descr,list_to_binary(Desc)}]);
process_action_body([{{output,[]},[{{leaf,[{name,Name}]}, OutBody}]} | Rem],Res) ->
    OutVal = [{returnType,[{returnTag,Name}] 
	       ++ get_out_param_type(OutBody)}],
    process_action_body(Rem,Res ++ OutVal);
process_action_body([{{input,[]},Params} | Rem],Res) ->
    ParamsList = [{parameters,process_action_params(Params,[])}],
    process_action_body(Rem,Res ++ ParamsList);
process_action_body([Others | Rem],Res) ->
    io:format("Actions :: Not Handled : ~p",[Others]),
    process_action_body(Rem,Res).

%% process_action_params([{{leaf,[{name,Name}]},[{{type,[{name,"inet:uri"}]},[]},{{mandatory,[{value,"true"}]},[]},{{description,[]},[{{text,[]},Des}]}]}])
process_action_params([],Res) ->
    Res;
process_action_params([{{leaf,[{name,Name}]},ParamBody} | RemParams],Res) ->
    ParamDets = [{Name, process_leaf_typedef_body(ParamBody,[{flags,[in]}],"")}],
    process_action_params(RemParams,Res ++ ParamDets);
process_action_params([_ | RemParams], Res) ->
    process_action_params(RemParams, Res).

get_out_param_type([]) ->
    [];
get_out_param_type([{{type,[{name,Type}]},_Path}|_Rest]) ->
    [{type,list_to_atom(Type)}];
get_out_param_type([_ | Rest]) ->
    get_out_param_type(Rest).
    



prepare_relations_cardinality(Parent,Child,ClassBody)->
    io:format(">>> classbody ~p",[ClassBody]),
    NewClassBody=case get_XML_tag(key,ClassBody) of
		     [] ->
			 ClassBody;
		     Key=[{{key,[{value,AttrNam}]},[]}] ->
			 %%FIX Me we are handling only first key in case of multiple 
			 AttrName=list_to_atom(hd(string:tokens(AttrNam," "))),
			 case lists:keysearch(attributes,1,ClassBody) of
			     false ->
				 ClassBody;
			     {value,{attributes,Attrs}} ->
				 KeyAttrBody = case lists:keysearch(AttrName,1,Attrs) of
						   false ->
						       [];
						   {value,{AttrName,AttrBody}} ->
						       case lists:keysearch(flags,1,AttrBody) of
							   false ->
							       [];
							   {value,{flags,Flags}} ->
							       lists:keyreplace(flags,1,AttrBody,{flags,Flags++[key]})
						       end
					       end,
				 case KeyAttrBody of
				     [] ->
					 ClassBody;
				     _Else ->
					 NewAttrs=lists:keyreplace(AttrName,1,Attrs,{AttrName,KeyAttrBody}),
					 ets:insert(?YIN_TABLE, {{mim_mimdata,{{name,Child},{attribute,AttrName}}},KeyAttrBody}),
					 ets:insert(?YIN_TABLE, {{new_mimdata,{{name,Child},{attribute,AttrName}}},KeyAttrBody}),
					 lists:keyreplace(attributes,1,ClassBody,{attributes,NewAttrs})--Key
				 end
			 end
		 end,
    {MinEntry,Min} = case get_XML_tag('min-elements',ClassBody) of
			 [] ->
			     {[],0};
			 MinVal=[{{'min-elements',[{value,Mi}]},[]}]->
			     {MinVal,list_to_integer(Mi)}
		     end,
    {MaxEntry,Max} = case get_XML_tag('max-elements',ClassBody) of
			  [] ->
			      {[],unlimited};
			  MaxVal=[{{'max-elements',[{value,Ma}]},[]}]->
			      {MaxVal,list_to_integer(Ma)}
		      end,
     ets:insert(?YIN_TABLE,{{mim_mimrelationship,{{rel,Parent,Child},{type,containment}}},
			    [{name,Parent ++ "_to_" ++ Child},{min,Min},{max,Max}]}),
    io:format(">>> classbody ~p",[NewClassBody]),
    (NewClassBody--MinEntry)--MaxEntry.
%%FIX me duplicate
prepare_relations_cardinality_con(Parent,Child,ClassBody)->
    NewClassBody=case get_XML_tag(key,ClassBody) of
		     [] ->
			 ClassBody;
		     Key=[{{key,[{value,AttrName}]},[]}] ->
			 case lists:keysearch(attribute,1,ClassBody) of
			     false ->
				 ClassBody;
			     {value,{attributes,Attrs}} ->
				 KeyAttrBody = case lists:keysearch(AttrName,1,Attrs) of
						   false ->
						       ClassBody;
						   {value,{AttrName,AttrBody}} ->
						       case lists:keysearch(flags,1,AttrBody) of
							   false ->
							       [];
							   {value,{flags,Flags}} ->
							       lists:keyreplace(flags,1,AttrBody,{flags,Flags++[key]})
						       end
					       end,
				 case KeyAttrBody of
				     [] ->
					 ClassBody;
				     _Else ->
					 NewAttrs=lists:keyreplace(AttrName,1,Attrs,KeyAttrBody),
					 lists:keyreplace(attributes,1,ClassBody,{attributes,NewAttrs})--Key
				 end
			 end
		 end,
    {MinEntry,Min} = case get_XML_tag('min-elements',ClassBody) of
			 [] ->
			     {[],1};
			 MinVal=[{{'min-elements',[{value,Mi}]},[]}]->
			     {MinVal,list_to_integer(Mi)}
		     end,
    {MaxEntry,Max} = case get_XML_tag('max-elements',ClassBody) of
			  [] ->
			      {[],1};
			  MaxVal=[{{'max-elements',[{value,Ma}]},[]}]->
			      {MaxVal,list_to_integer(Ma)}
		      end,
     ets:insert(?YIN_TABLE,{{mim_mimrelationship,{{rel,Parent,Child},{type,containment}}},
			    [{name,Parent ++ "_to_" ++ Child},{min,Min},{max,Max}]}),
     (NewClassBody--MinEntry)--MaxEntry.


%process_container_body([],ClassBody, ExtBody,_,_Prfx,AllLeafs,AllActs,[]) ->
%    {ClassBody++[{attributes,AllLeafs}]++[{actions,AllActs}], ExtBody};
process_container_body([],ClassBody, ExtBody,_,_Prfx,AllLeafs,AllActs) ->
	{ClassBody++[{attributes,AllLeafs}]++[{actions,AllActs}], ExtBody};
process_container_body([{{grouping,[{name,GrpName}]},GrpBody}|Body],ClassBody,ExtBody,Name,Prfx,AllLeafs,AllActs) ->
    ets:insert(?YIN_TABLE,{{Prfx,GrpName},GrpBody}),   	
    process_container_body(GrpBody++Body,ClassBody,ExtBody,Name,Prfx,AllLeafs,AllActs);
process_container_body([{{uses,[{name,GrpName}]},_}|Body],ClassBody,ExtBody,Name ,Prfx,AllLeafs,AllActs) -> 
    io:format(" CON/LIST uses: name ~p ~n ",[{Prfx,GrpName}]),
    GrpKey = case re:run(GrpName, ":") of
		 %% nomatch when is_atom(GrpName) ->
		 %%     {Prfx, atom_to_list(GrpName)};
		 nomatch ->
		     {Prfx, GrpName};
		 _ -> [Pfx,GrName] =string:tokens(GrpName,":"),
		      {Pfx,GrName}
	     end,   
    GrpValue=case ets:lookup(?YIN_TABLE,GrpKey) of
		 [] ->
		     io:format(" CON/LIST Group Not Found --~p ~n",[GrpKey]),
		     case ets:match(?YIN_TABLE,{{'_',GrpName},'$1'}) of
			 []->
			     io:format(" Nested Group tag Not Found --~p~n",[GrpName]),
			     [];
			 [Head|_] ->
			     lists:flatten(Head)
		     end;		    
		 [{_Key,Value}] ->
		     Value
	     end,
    process_container_body(GrpValue++Body,ClassBody,ExtBody,Name,Prfx,AllLeafs,AllActs);
process_container_body([{{?YIN_CONTAINER,CName},_}=Container|Body],ClassBody,ExtBody,Name,Prfx,AllLeafs,AllActs) ->
    io:format(" postprocess child container ~p ~n ",[CName]),
    process_container_body(Body,ClassBody,ExtBody++[Container],Name,Prfx,AllLeafs,AllActs);
process_container_body([{{?YIN_LIST,LName},_ListBody}=ListC|Body],ClassBody,ExtBody,Name,Prfx,AllLeafs,AllActs) ->
    io:format(" List ~p~n ", [LName]),
    process_container_body(Body,ClassBody,ExtBody++[ListC],Name,Prfx,AllLeafs,AllActs);
process_container_body([{{?YIN_LEAF,[{name,LeafName}]},LeafBody}|Body],ClassBody,ExtBody,Name,Prfx,AllLeafs,AllActs) ->
    io:format(" CON/LIST leaf ~p~n",[LeafName]),
    LeafDetails= case process_leaf_typedef_body(LeafBody,[{flags,[]}],Prfx) of
		     {derived, DTBody} ->
			 DTBody;
		     DDTBody ->
			 case lists:keysearch(type,1,DDTBody) of
			     {value,{type,enumeration}} ->
				 io:format("~nenummmm ~p~n",[DDTBody]),
				 {value,EnumMems} = lists:keysearch(enummembers,1,DDTBody),
				 ets:insert(?YIN_TABLE,{{mim_extended_definitions,
							 {enum,A=Prfx++":"++LeafName}},[{flags,[]},EnumMems]}),
				 ets_key_value_append({mim_extended_definitions,enums},A),
				 NewEnumBody = DDTBody--[EnumMems],
				 %%ENUM STRING
				 simdiv:key_mergereplace([{type,string}],NewEnumBody);
			     _Else ->
				 DDTBody
			 end
		 end,
    ets:insert(?YIN_TABLE,{{mim_mimdata,{{name,Prfx++":"++Name},{attribute,list_to_atom(LeafName)}}},LeafDetails}),	
    ets:insert(?YIN_TABLE,{{new_mimdata,{{name,Prfx++":"++Name},{attribute,list_to_atom(LeafName)}}},LeafDetails}),
    process_container_body(Body,ClassBody,ExtBody,Name,Prfx,[{list_to_atom(LeafName),LeafDetails}]++AllLeafs,AllActs);

process_container_body([{{?YIN_LEAF_LIST,[{name,LLName}]},LLBody}|Body],ClassBody,ExtBody,Name,Prfx,AllLeafs,AllActs) ->
    io:format(" con-leaflist ~p ",[LLName]),
    LeafListDetails= case process_leaf_typedef_body(LLBody,[{flags,[]}],Prfx) of
			 {derived, DTBody} ->
			     io:format("Impossible case"),
			     DTBody;
		     %% end;
		     DDTBody ->
			 DDTBody
		 end,
    FnlLLDet = case lists:keysearch(type,1, LeafListDetails) of
    		   {value,{type,Val}} ->		   
		       lists:keyreplace(type,1,LeafListDetails,{type,{seq,Val}});%%FIX me it is causing issue while storing the value ([{tcp,[],[]}]
		       %%lists:keyreplace(type,1,LeafListDetails,{type,string});
    		   _ ->
		       io:format("Type Not Found ~p",[LLName]),
		       LeafListDetails
    		     end,
    ets:insert(?YIN_TABLE,{{mim_mimdata,{{name,Prfx++":"++Name},{attribute,list_to_atom(LLName)}}},FnlLLDet}),	
    ets:insert(?YIN_TABLE,{{new_mimdata,{{name,Prfx++":"++Name},{attribute,list_to_atom(LLName)}}},FnlLLDet}),	
    process_container_body(Body,ClassBody,ExtBody,Name,
			   Prfx,[{list_to_atom(LLName),FnlLLDet}]++AllLeafs,AllActs);
%%{{choice,[{name,"seq-choice1"}]},

process_container_body([{{?YIN_ACTION,[{name,ActName}]},ActBody}|Body],ClassBody,ExtBody,Name,Prfx,AllLeafs,AllActs) ->
    ActDetails = [{ActName,process_action_body(ActBody,[{flags,[]}])}],
    process_container_body(Body,ClassBody,ExtBody,Name,
			   Prfx,AllLeafs,ActDetails++AllActs);

%%process_container_body([{{choice,[{name,ChName}]},ChoiceBody}=C1|Rest]=C2,ClassBody,ExtBody,Name,Prfx,AllLeafs,AllActs) ->
%%    %%Thise function has to be called when the actual choice body process with validations
%%    %%{ClsBody, ExtBdy,AllLeaf}=process_choice_body(ChoiceBody,ClassBody,ExtBody,Name,Prfx,AllLeafs,AllActs),
%%    %% process_container_body(NewChoiceBody++Rest,ClsBody,ExtBdy,Name,Prfx,AllLeaf);
%%    io:format("~nChoice resolution ~p~n",[C1]),
%%    [NewChoiceBody,Choices]=process_choice_body_simple(ChoiceBody,[]),
%%    io:format("~nNewChoiceBody ~p~n~n~p~n",[NewChoiceBody,Choices]),
%%    process_container_body(NewChoiceBody++Rest,ClassBody,ExtBody,Name,Prfx,AllLeafs,AllActs,AllChoices++[{ChName,Choices}]);

process_container_body([{{Tag,[{value,_}]},[]}=Key|Body],ClassBody,ExtBody,Name,Prfx,AllLeafs,AllActs) 
  when Tag == key;
Tag == 'min-elements';
Tag == 'max-elements'->  
    process_container_body(Body,[Key]++ClassBody,ExtBody,Name,Prfx,AllLeafs,AllActs);
%% process_container_body([{{description,[]},[{{text,[]},Desc}]}|Body],ClassBody,ExtBody,Name,Prfx,AllLeafs,AllActs)->
%%     process_container_body(Body,[{desc,list_to_binary(Desc)}]++ClassBody,ExtBody,Name,Prfx,AllLeafs,AllActs);
%%--MM--%%
process_container_body([{{config,[{value,Value=false}]},[]}|Body],ClassBody,
		       ExtBody,Name,Prfx,AllLeafs,AllActs) ->
    process_container_body(Body,[{config,Value}]++ClassBody,ExtBody,
			   Name,Prfx,AllLeafs,AllActs);
%process_container_body([{choice,ChName}|Body],ClassBody,
%		       ExtBody,Name,Prfx,AllLeafs,AllActs) ->
%    process_container_body(Body,[{config,ChNAme}]++ClassBody,ExtBody,
%			   Name,Prfx,AllLeafs,AllActs);
process_container_body([{{choice,[{name,ChName}]},ChoiceBody}=C1|Body]=C2,ClassBody,ExtBody,Name,Prfx,AllLeafs,AllActs) ->
    io:format("~nChoice resolution ~p~n",[C1]),
    %[NewChoiceBody,Choices]=process_choice_body_simple(ChName,ChoiceBody,[]),
    {EBody,ALeafs} = process_choice_body(ChName,ChoiceBody,Name,Prfx,[],[]),
    %io:format("~nNewChoiceBody ~p~n~n~p~n",[NewChoiceBody,Choices]),
    process_container_body(Body,ClassBody,ExtBody++EBody,Name,Prfx,AllLeafs++ALeafs,AllActs);
%%--MM--%%
process_container_body([Head|Body],ClassBody,ExtBody,Name,Prfx,AllLeafs,AllActs)->
    io:format("~nContainer Body Skipped ~p~n",[Head]),
    process_container_body(Body,[Head]++ClassBody,ExtBody,Name,Prfx,AllLeafs,AllActs).

%[{{'case',[{name,"protocol-operation"}]},[{{leaf,[{name,"rpc-name"}]},[{{type,[{name,"union"}]},[{{type,[{name,"matchall-string-type"}]},[]},{{type,[{name,"string"}]},[]}]},{{description,[]},[{{text,[]},"Thisleafmatchesifithasthevalue'*'orif\nitsvalueequalstherequestedprotocoloperation\nname."}]}]}]},{{'case',[{name,"notification"}]},[{{leaf,[{name,"notification-name"}]},[{{type,[{name,"union"}]},[{{type,[{name,"matchall-string-type"}]},[]},{{type,[{name,"string"}]},[]}]},{{description,[]},[{{text,[]},"Thisleafmatchesifithasthevalue'*'orifits\nvalueequalstherequestednotificationname."}]}]}]},{{'case',[{name,"data-node"}]},[{{leaf,[{name,"path"}]},[{{type,[{name,"node-instance-identifier"}]},[]},{{mandatory,[{value,"true"}]},[]},{{description,[]},[{{text,[]},"Datanodeinstance-identifierassociatedwiththe\ndatanode,action,ornotificationcontrolledby\nthisrule.\n\nConfigurationdataorstatedata\ninstance-identifiersstartwithatop-level\ndatanode.Acompleteinstance-identifieris\nrequiredforthistypeofpathvalue.\n\nThespecialvalue'/'referstoallpossible\ndatastorecontents."}]}]}]}]

%%process_choice_body_simple(ChoiceName,[{{'case',[{name,CName}]},CaseBody} = CB |ChoiceBody] = Choice,ACC)->
%%    io:format("~nchoice CaSE Body to actual class ~p ~p~n",[CName,Choice]),
%%    process_choice_body_simple(ChoiceBody,CaseBody++ACC);
%%process_choice_body_simple([CHTag|ChoiceBody]=Cmm,ACC) ->
%%    io:format("~nChoice Body added to actual class ~p~n",[Cmm]),
%%    process_choice_body_simple(ChoiceBody,[CHTag]++ACC);
%%process_choice_body_simple([],ACC) ->
%%   io:format("~nchoice acc ~p~n",[ACC]),
%%    ACC.

process_choice_body(_ChoiceName,[],_Name,_Prfx,ExtBody,AllLeafs) ->
    {ExtBody,AllLeafs};
%%Below functions are written for future use to handle mandatory and choice validations

%[{{'case',[{name,"protocol-operation"}]},[{{leaf,[{name,"rpc-name"}]},[{{type,[{name,"union"}]},[{{type,[{name,"matchall-string-type"}]},[]},{{type,[{name,"string"}]},[]}]},{{description,[]},[{{text,[]},"Thisleafmatchesifithasthevalue'*'orif\nitsvalueequalstherequestedprotocoloperation\nname."}]}]}]},{{'case',[{name,"notification"}]},[{{leaf,[{name,"notification-name"}]},[{{type,[{name,"union"}]},[{{type,[{name,"matchall-string-type"}]},[]},{{type,[{name,"string"}]},[]}]},{{description,[]},[{{text,[]},"Thisleafmatchesifithasthevalue'*'orifits\nvalueequalstherequestednotificationname."}]}]}]},{{'case',[{name,"data-node"}]},[{{leaf,[{name,"path"}]},[{{type,[{name,"node-instance-identifier"}]},[]},{{mandatory,[{value,"true"}]},[]},{{description,[]},[{{text,[]},"Datanodeinstance-identifierassociatedwiththe\ndatanode,action,ornotificationcontrolledby\nthisrule.\n\nConfigurationdataorstatedata\ninstance-identifiersstartwithatop-level\ndatanode.Acompleteinstance-identifieris\nrequiredforthistypeofpathvalue.\n\nThespecialvalue'/'referstoallpossible\ndatastorecontents."}]}]}]}]
%process_choice_body([{{'case',[{name,CName}]},CaseBody} = CB |ChoiceBody] = Choice,ACC)->


%process_choice_body(ChoiceName, [{{container,[{name,ContName}]},_}=ChContainerBody|ChoiceBody],
%			ExtBody,Name,Name,Prfx,AllLeafs,AllActs)->

%Choice Container
			     
process_choice_body(ChoiceName, [{{container,[{name,ContName}]},_}=ChContainerBody|ChoiceBody],Name,Prfx,ExtBody,AllLeafs) ->
    io:format("Choice containers ~p ~n",[ContName]),
    NewChContainerBody = process_choice_class(ChoiceName,ChContainerBody,Name,Prfx),
    process_choice_body(ChoiceName,ChoiceBody,Name,Prfx,NewChContainerBody++ExtBody,AllLeafs);
process_choice_body(ChoiceName,[{{'case',[{name,_CN}]},[{{container,[{name,ContName}]},_}=ChContainerBody]}|ChoiceBody],
			Name,Prfx,ExtBody,AllLeafs) ->
    io:format("Choice case containers ~p ~n",[ContName]),
    NewChContainerBody = process_choice_class(ChoiceName,ChContainerBody,Name,Prfx),
    process_choice_body(ChoiceName,ChoiceBody,Name,Prfx,NewChContainerBody++ExtBody,AllLeafs);
    
%process_choice_body(ChoiceName, [{{list,[{name,ListName}]},_}=ChListBody|ChoiceBody],Name,Prfx,ExtBody,AllLeafs)->
%Choice List
%process_choice_body(ChoiceName, [ChTag|ChoiceBody],Name,Prfx,ExtBody,AllLeafs) 
%			when ChTag == {{?YIN_LIST,[{name,ListName}]},_}; 
%			     ChTag == {{'case',[{name,_CN}]},[{{{?YIN_LIST,[{name,ListName}]},_}=ChListBody]}->
			     
process_choice_body(ChoiceName,[{{?YIN_LIST,[{name,ListName}]},_}=ChListBody|ChoiceBody],Name,Prfx,ExtBody,AllLeafs) ->			     
    io:format("Choice list ~p ~n",[ListName]),
    NewChListBody = process_choice_class(ChoiceName,ChListBody,Name,Prfx),
    process_choice_body(ChoiceName,ChoiceBody,Name,Prfx,NewChListBody++ExtBody,AllLeafs);
process_choice_body(ChoiceName,[{{'case',[{name,_CN}]},[{{?YIN_LIST,[{name,ListName}]},_}=ChListBody]}|ChoiceBody],
			Name,Prfx,ExtBody,AllLeafs) ->		     
    io:format("Choice list ~p ~n",[ListName]),
    NewChListBody = process_choice_class(ChoiceName,ChListBody,Name,Prfx),
    process_choice_body(ChoiceName,ChoiceBody,Name,Prfx,NewChListBody++ExtBody,AllLeafs);
    
%Choice Leaf-List
%process_choice_body(ChoiceName,[{{'leaf-list',[{name,LeafName}]},ChLListBody}|ChoiceBody],ExtBody,AllLeafs)->
process_choice_body(ChoiceName,[{{?YIN_LEAF_LIST,[{name,LLName}]},ChLListBody}|ChoiceBody],Name,Prfx,ExtBody,AllLeafs) ->		     
    io:format(" con-leaflist ~p ",[LLName]),
    FnlLLDet = process_choice_leaflist(ChoiceName,LLName,ChLListBody,Name,Prfx),
    process_choice_body(ChoiceName,ChoiceBody,Name,Prfx,ExtBody,[{list_to_atom(LLName),FnlLLDet}]++AllLeafs);
process_choice_body(ChoiceName,[{{'case',[{name,_CN}]},[{{?YIN_LEAF_LIST,[{name,LLName}]},ChLListBody}]}|ChoiceBody],
			Name,Prfx,ExtBody,AllLeafs) ->
    io:format(" con-leaflist ~p ",[LLName]),
    FnlLLDet = process_choice_leaflist(ChoiceName,LLName,ChLListBody,Name,Prfx),
    process_choice_body(ChoiceName,ChoiceBody,Name,Prfx,ExtBody,[{list_to_atom(LLName),FnlLLDet}]++AllLeafs);

%process_choice_body(ChoiceName,[{{?YIN_LEAF,[{name,LeafName}]},ChLeafBody}|ChoiceBody],    
%Choice Leaf
process_choice_body(ChoiceName,[{{?YIN_LEAF,[{name,LeafName}]},ChLeafBody}|ChoiceBody],Name,Prfx,ExtBody,AllLeafs) ->
    io:format("##### ~p leaf ~p",[ChoiceName,LeafName]),
    NewLeafDets = process_choice_leaf(ChoiceName,LeafName,ChLeafBody,Name,Prfx),
    process_choice_body(ChoiceName,ChoiceBody,Name,Prfx,ExtBody,[{list_to_atom(LeafName),NewLeafDets}]++AllLeafs);
process_choice_body(ChoiceName,[{{'case',[{name,_CN}]},[{{?YIN_LEAF,[{name,LeafName}]},ChLeafBody}]}|ChoiceBody],
			Name,Prfx,ExtBody,AllLeafs) ->
    io:format("##### ~p leaf ~p",[ChoiceName,LeafName]),
    NewLeafDets = process_choice_leaf(ChoiceName,LeafName,ChLeafBody,Name,Prfx),
    process_choice_body(ChoiceName,ChoiceBody,Name,Prfx,ExtBody,[{list_to_atom(LeafName),NewLeafDets}]++AllLeafs);
    
%%process_choice_body([{{'case',[{name,CaseName}]},[{{container,[{name,CName}]},_}]=CaseBody}|ChoiceBody],
%%			ExtBody,Name,Prfx,AllLeafs,AllActs)->
%%    io:format("Choice container ~p ~n",[CName]),
%%    process_choice_body(ChoiceBody,CaseBody++ExtBody,Name,Prfx,AllLeafs,AllActs);
%%process_choice_body([{{'case',[{name,CaseName}]},[{{list,[{name,CName}]},_}]=CaseBody}|ChoiceBody],
%%			ExtBody,Name,Prfx,AllLeafs,AllActs)->
%%    
%%    io:format("Choice container ~p ~n",[CName]),
%%    process_choice_body(ChoiceBody,CaseBody++ExtBody,Name,Prfx,AllLeafs,AllActs);
%%process_choice_body([{{'case',[{name,ChName}]},[{{?YIN_LEAF,[{name,LeafName}]},ChLeafBody}]}|ChoiceBody],ExtBody,Name,Prfx,AllLeafs,AllActs)->
%%    io:format(" choice- leaf ~p ",[LeafName]),
%%    LeafDetails= case process_leaf_typedef_body(ChLeafBody,[{flags,[]}],Prfx) of
%%		     {derived, DTBody} ->
%%			 io:format("~nDerivedddddddddddddddddddd ~p~n",[DTBody]),
%%			 DTBody;
%%		     DDTBody ->
%%			 case lists:keysearch(type,1,DDTBody) of
%%			     {value,{type,enumeration}} ->
%%				 io:format("~nenummmm ~p~n",[DDTBody]),
%%				 {value,EnumMems} = lists:keysearch(enummembers,1,DDTBody),
%%				 ets:insert(?YIN_TABLE,{{mim_extended_definitions,
%%							 {enum,A=Prfx++":"++LeafName}},[{flags,[]},EnumMems]}),
%%				 ets_key_value_append({mim_extended_definitions,enums},A),
%%				 NewEnumBody = DDTBody--[EnumMems],
%%				 simdiv:key_mergereplace([{type,{enum,A}}],NewEnumBody);
%%			     _Else ->
%%				 DDTBody
%%			 end
%%		 end,
%%    NewLeafDets = LeafDetails ++ [{choice,ChName}],
%%    case ets:lookup(?YIN_TABLE,{choices,ChName}) of
%%    	[{_Key,CAttr}] ->
%%	    ets:insert(?YIN_TABLE,{{choices,ChName},LeafName ++ Attrs});
%%	_ ->
%%	    ets:insert(?YIN_TABLE,{{choices,ChName},LeafName})
%%    end,
%%    ets:insert(?YIN_TABLE,{{mim_mimdata,{{name,Prfx++":"++Name},{attribute,list_to_atom(LeafName)}}},NewLeafDets}),	
%%    ets:insert(?YIN_TABLE,{{new_mimdata,{{name,Prfx++":"++Name},{attribute,list_to_atom(LeafName)}}},NewLeafDets}),
%%    process_choice_body(ChoiceBody,ExtBody,Name,Prfx,
%%	[{list_to_atom(LeafName),NewLeafDets}]++AllLeafs,AllActs);
%%	%AllChoices++[{CName},[{case1,[{A2,[]}]}]]);

process_choice_body(ChoiceName,[{{choice,[{name,ChName}]},IntChoiceBody}|ChoiceBody],Name,Prfx,ExtBody,AllLeafs) ->
    {EBody,ALeafs} = process_choice_body(ChName,IntChoiceBody,Name,Prfx,[],[]),
    process_choice_body(ChoiceName,ChoiceBody,Name,Prfx,ExtBody++EBody,AllLeafs++ALeafs);
process_choice_body(ChoiceName,[{{'case',[{name,_CN}]},[{{choice,[{name,ChName}]},IntChoiceBody}]}|ChoiceBody],
			Name,Prfx,ExtBody,AllLeafs) ->
    {EBody,ALeafs} = process_choice_body(ChName,IntChoiceBody,Name,Prfx,[],[]),
    process_choice_body(ChoiceName,ChoiceBody,Name,Prfx,ExtBody++EBody,AllLeafs++ALeafs);
    
process_choice_body(ChoiceName,[{{description,[]},_}|ChoiceBody],Name,Prfx,ExtBody,AllLeafs) ->
    process_choice_body(ChoiceName,ChoiceBody,Name,Prfx,ExtBody,AllLeafs);
process_choice_body(ChoiceName,[Tag|ChoiceBody],Name,Prfx,ExtBody,AllLeafs)->
    io:format("Choice SKIPPED ~p ~n",[Tag]),
    process_choice_body(ChoiceName,ChoiceBody,Name,Prfx,ExtBody,AllLeafs).
%% FIX ME MN: Check if we need to add ?YIN_ACTION handler for choice as well


process_choice_class(ChName,{{Class,[{name,ClassName}]},ClassBody},Name,Prfx) ->   
    NewChClassBody = [{{Class,[{name,ClassName}]},ClassBody ++ [{choice,ChName ++ "-" ++Prfx ++ ":" ++ Name}]}],
    %NewChClassBody = [{choice,ChName}] ++ [ChClassBody],
    case ets:lookup(?YIN_TABLE,{choice,ChName++ "-" ++Prfx++":"++ Name}) of
    	[{_Key,Classes}] ->	  
	    ets:insert(?YIN_TABLE,{{choice,ChName++"-"++Prfx++":"++ Name},[{con,Prfx ++ ":" ++ ClassName}] ++ Classes});
	_ ->
	    %ets:insert(?YIN_TABLE,{{choice,ChName++"-"++Prfx++":"++ Name},[Prfx ++ ":" ++ ClassName]})
	    ets:insert(?YIN_TABLE,{{choice,ChName++"-"++Prfx++":"++ Name},[{con,Prfx ++ ":" ++ ClassName}]})
    end,
    NewChClassBody.
    
process_choice_leaflist(ChName,LLName,ChLListBody,Name,Prfx) ->
   LeafListDetails= case process_leaf_typedef_body(ChLListBody,[{flags,[]}],Prfx) of
			 {derived, DTBody} ->
			     io:format("Impossible case"),
			     DTBody;
			 DDTBody ->
			     DDTBody
		     end,
    NewLLDet = case lists:keysearch(type,1, LeafListDetails) of
    		   {value,{type,Val}} ->
    		    %%lists:keyreplace(type,1,LeafListDetails,{type,{seq,Prfx++":"++atom_to_list(Val)}});		   
		       lists:keyreplace(type,1,LeafListDetails,{type,{seq,Val}});		   
    		   _ ->
		       io:format("Type Not Found ~p",[LLName]),
		       LeafListDetails
    		     end,  
    FnlLLDet = [{choice,ChName++ "-" ++ Prfx ++":" ++Name}] ++ NewLLDet,
    ets:insert(?YIN_TABLE,{{mim_mimdata,{{name,Prfx++":"++Name},{attribute,list_to_atom(LLName)}}},FnlLLDet}),	
    ets:insert(?YIN_TABLE,{{new_mimdata,{{name,Prfx++":"++Name},{attribute,list_to_atom(LLName)}}},FnlLLDet}),
    case ets:lookup(?YIN_TABLE,{choice,ChName++ "-" ++Prfx++":"++ Name}) of
    	[{_Key,LeafLists}] ->	    
	     ets:insert(?YIN_TABLE,{{choice,ChName++"-"++Prfx++":"++Name},[{leaf,list_to_atom(LLName)}] ++ LeafLists});
	_ ->
	    ets:insert(?YIN_TABLE,{{choice,ChName++"-"++Prfx++":"++Name},[{leaf,list_to_atom(LLName)}]})
    end,
    FnlLLDet.
    
process_choice_leaf(ChName,LeafName,ChLeafBody,Name,Prfx) ->
    LeafDetails= case process_leaf_typedef_body(ChLeafBody,[{flags,[]}],Prfx) of
		     {derived, DTBody} ->
			 io:format("~nDerivedddddddddddddddddddd ~p~n",[DTBody]),
			 DTBody;
		     DDTBody ->
			 case lists:keysearch(type,1,DDTBody) of
			     {value,{type,enumeration}} ->
				 io:format("~nenummmm ~p~n",[DDTBody]),
				 {value,EnumMems} = lists:keysearch(enummembers,1,DDTBody),
				 ets:insert(?YIN_TABLE,{{mim_extended_definitions,
							 {enum,A=Prfx++":"++LeafName}},[{flags,[]},EnumMems]}),
				 ets_key_value_append({mim_extended_definitions,enums},A),
				 NewEnumBody = DDTBody--[EnumMems],
				 simdiv:key_mergereplace([{type,{enum,A}}],NewEnumBody);
			     _Else ->
				 DDTBody
			 end
		 end,   
    NewLeafDets = LeafDetails ++ [{choice,ChName++ "-" ++ Prfx++":"++Name}],
    ets:insert(?YIN_TABLE,{{mim_mimdata,{{name,Prfx++":"++Name},{attribute,list_to_atom(LeafName)}}},NewLeafDets}),	
    ets:insert(?YIN_TABLE,{{new_mimdata,{{name,Prfx++":"++Name},{attribute,list_to_atom(LeafName)}}},NewLeafDets}),
    case ets:lookup(?YIN_TABLE,{choice,ChName++ "-" ++Prfx++":"++ Name}) of
    	[{_Key,Leafs}] ->
	    ets:insert(?YIN_TABLE,{{choice,ChName++"-"++Prfx++":"++Name},[{leaf,list_to_atom(LeafName)}] ++ Leafs});
	_ ->
	    ets:insert(?YIN_TABLE,{{choice,ChName++"-"++Prfx++":"++Name},[{leaf,list_to_atom(LeafName)}]})
    end,
    NewLeafDets.

append_or_add_attributes_actions_to_class(TrgtNode, NewAttrs,Field) ->
    case ets:lookup(?YIN_TABLE,{mim_mimdata,{name,TrgtNode}}) of
	[{_Key,ClassBody}] ->
	    {value,{Field,Atts}}=lists:keysearch(Field,1,NewAttrs),	    
	    NewBaseBody = case lists:keysearch(Field,1,ClassBody) of
			      false ->
				  io:format("SUSPECTING case: Check it ~n"),
				  ClassBody++NewAttrs;
			      {value,{Field,Attrs}} ->
				  lists:keyreplace(Field,1,ClassBody,{Field,simdiv:key_mergereplace(Atts,Attrs)})
			  end,	 
	    ets:insert(?YIN_TABLE, {{mim_mimdata,{name,TrgtNode}},NewBaseBody}),
	    ets:insert(?YIN_TABLE, {{new_mimdata,{name,TrgtNode}},NewBaseBody});
        []->
	    ets:insert(?YIN_TABLE, {{mim_mimdata,{name,TrgtNode}},NewAttrs}),
	    ets:insert(?YIN_TABLE, {{new_mimdata,{name,TrgtNode}},NewAttrs})
    end.

ets_key_value_append(Key,AppndVal) ->
    case ets:lookup(?YIN_TABLE,Key) of
	[] ->
	    ets:insert(?YIN_TABLE,{Key,[AppndVal]});
	[{Key,Val}] ->
	    case lists:member(AppndVal,Val) of
		false ->
		    ets:insert(?YIN_TABLE,{Key,Val++[AppndVal]});
		_->
		    ok
	    end
    end.
