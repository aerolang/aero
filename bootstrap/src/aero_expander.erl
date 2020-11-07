%%% Converts the Aero AST down to Core Aero.
%%%
%%% This format is made to resemble Core Erlang, with variables renamed,
%%% macros expanded, and with type information checked, giving a simple core
%%% language.
%%%
%%% For now we don't really have macros, so they're just manually expanded here.

-module(aero_expander).

-export([expand/2]).

-export_type([c_any/0, c_module/0, c_expr/0, c_block/0]).
-export_type([c_literal/0, c_integer_lit/0, c_float_lit/0, c_atom_lit/0, c_string_lit/0]).
-export_type([c_func/0, c_var/0, c_type/0]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

%% Any Core Aero syntax element.
-type c_any() :: c_module()
               | c_expr()
               | c_type().

%% Top-level of Core Aero, represents a whole module.
-type c_module() :: {c_module, meta(), c_atom_lit(), c_module_exports(), c_module_attrs(),
                                                     c_module_defs()}.

-type c_module_exports() :: [c_atom_lit()].
-type c_module_attrs()   :: [{c_atom_lit(), c_literal()}].
-type c_module_defs()    :: [{c_atom_lit(), c_func()}].

%% Any expression, excludes non-expression parts of Core Aero.
-type c_expr() :: c_block()
                | c_literal()
                | c_unit()
                | c_func()
                | c_var().

%% A group of expressions with the last giving the value of the group.
-type c_block() :: {c_block, meta(), [c_expr()]}.

%% A constant literal value at compile time.
-type c_literal() :: c_integer_lit()
                   | c_float_lit()
                   | c_atom_lit()
                   | c_string_lit().

%% Literals.
-type c_integer_lit() :: {c_integer_lit, meta(), integer()}.
-type c_float_lit()   :: {c_float_lit, meta(), float()}.
-type c_atom_lit()    :: {c_atom_lit, meta(), atom()}.
-type c_string_lit()  :: {c_string_lit, meta(), binary()}.

%% Unit type.
-type c_unit() :: {c_unit, meta()}.

%% A function expression.
-type c_func() :: {c_func, meta(), c_func_args(), c_func_ret(), c_func_where(), c_func_body()}.

-type c_func_args()   :: [{c_var(), c_type_inner()}].
-type c_func_ret()    :: c_type_inner().
-type c_func_where()  :: c_type_where().
-type c_func_body()   :: c_expr().

%% Variables.
-type c_var() :: {c_var, meta(), atom()}.

%% Any type in Core Aero.
-type c_type() :: {c_type, meta(), c_type_inner(), c_type_where()}.

-type c_type_inner() :: c_type_int
                      | c_type_float
                      | c_type_atom
                      | c_type_str
                      | c_type_bytes
                      | c_type_bits
                      | c_type_ref
                      | c_type_unit
                      | {c_type_tuple, [c_type_inner()]}
                      | {c_type_list, c_type_inner()}
                      | {c_type_dict, c_type_inner(), c_type_inner()}
                      | {c_type_func, [c_type_inner()], c_type_inner()}
                      | {c_type_uniq, c_type_inner()} 
                      | {c_type_dyn, c_type_inner()}
                      | c_type_wld
                      | c_type_never
                      | {c_type_mbox, c_type_inner()}
                      | {c_type_addr, c_type_inner()}
                      | {c_type_param, atom()}
                      | {c_type_struct, atom(), [c_type_inner()]}
                      | {c_type_proto, atom(), [c_type_inner()]}
                      | {c_type_union, [c_type_inner()]}
                      | {c_type_inter, [c_type_inner()]}.
-type c_type_where() :: [{c_type_inner(), c_type_inner()}].

-type meta() :: [term()].

-spec expand(aero_parser:ast(), aero_context:context()) -> {ok, [c_module()]} | {error, term()}.
expand(Source, Context) ->
  Filename = aero_context:filename(Context),
  ModuleName = binary_to_atom(filename:basename(Filename, ".aero"), utf8),
  try expand_source(Source, ModuleName) of
    Modules -> {ok, Modules}
  catch
    throw:{expand_error, Reason} -> {error, Reason}
  end.

%% -----------------------------------------------------------------------------
%% Definition Expanding
%% -----------------------------------------------------------------------------

expand_source({source, Meta, SourceArgs}, ModuleName) ->
  expand_mod_def([{ident, [], ModuleName}, {block, Meta, SourceArgs}], []);
expand_source(_, _) ->
  throw({expand_error, no_source}).

expand_mod_def([{ident, _IdentMeta, Ident}, {block, _BlockMeta, BlockArgs}], _ModuleMeta) ->
  Meta = [],
  Attrs = [],
  {Exports, Defs, InnerModules} =
    lists:foldl(fun(BlockArg, {Exports, Defs, InnerModules}) ->
      case BlockArg of
        % Public functions.
        {expand, FuncMeta, {ident, _, pub}, [{expand, _, {ident, _, func}, FuncArgs}]} ->
          {Name, _} = Func = expand_func_def(FuncArgs, FuncMeta),
          {[Name | Exports], [Func | Defs], InnerModules};
        
        % Private functions.
        {expand, FuncMeta, {ident, _, func}, FuncArgs} ->
          Func = expand_func_def(FuncArgs, FuncMeta),
          {Exports, [Func | Defs], InnerModules}
      end
    end, {[], [], []}, BlockArgs),

  Module = {c_module, Meta, Ident, lists:reverse(Exports), Attrs, lists:reverse(Defs)},
  [Module | lists:reverse(InnerModules)];
expand_mod_def(_, ModuleMeta) ->
  throw({expand_error, {mod_def_invalid, ModuleMeta}}).

expand_func_def([FuncHead, FuncBody], _FuncMeta) ->
  Meta = [],
  {Name, Args, Ret, Where} = expand_func_def_head(FuncHead),
  Body = expand_func_def_body(FuncBody),

  Func = {c_func, Meta, Args, Ret, Where, Body},
  {Name, Func};
expand_func_def(_, FuncMeta) ->
  throw({expand_error, {func_def_invalid, FuncMeta}}).

expand_func_def_head(FuncHead) ->
  expand_func_def_head(FuncHead, []).

expand_func_def_head({expand, _, {op, _, '_where_'}, [FuncHeadLeft, Clause]}, Where) ->
  expand_func_def_head(FuncHeadLeft, [Clause | expand_type_where(Where)]);
expand_func_def_head({expand, FuncHeadMeta, {op, _, Arrow}, [{args, _, LeftArrowArgs}, Ret]},
                     Where) when Arrow =:= '_->_'; Arrow =:= '_->>_'->
  % TODO: check when pure.
  case LeftArrowArgs of
    [{expand, _, {op, _, '_(_)'}, [{ident, _, Name}, {args, _, Args}]}] ->
      {Name, lists:map(fun expand_func_def_arg/1, Args), expand_type_inner(Ret), Where};
    _ ->
      throw({expand_error, {func_def_head_invalid, FuncHeadMeta}})
  end;
expand_func_def_head(FuncHead, _) ->
  throw({expand_error, {func_def_head_invalid, get_meta(FuncHead)}}).

expand_func_def_arg({tag, _, {ident, _, Ident}, Type}) ->
  {{c_var, [], Ident}, expand_type_inner(Type)};
expand_func_def_arg(Arg) ->
  throw({expand_error, {func_def_arg_invalid, get_meta(Arg)}}).

expand_func_def_body({block, _, _} = Block) ->
  expand_expr(Block);
expand_func_def_body(FuncBody) ->
  throw({expand_error, {func_def_body_invalid, get_meta(FuncBody)}}).

%% -----------------------------------------------------------------------------
%% Expression Expanding
%% -----------------------------------------------------------------------------

%% Literals.
expand_expr({integer_lit, _, Integer}) ->
  {c_integer_lit, [], Integer};
expand_expr({float_lit, _, Float}) ->
  {c_float_lit, [], Float};
expand_expr({atom_lit, _, Atom}) ->
  {c_atom_lit, [], Atom};
expand_expr({string_lit, _, String}) ->
  {c_string_lit, [], String};

%% Blocks.
expand_expr({block, _, []}) ->
  {c_unit, []};
expand_expr({block, _, [Expr]}) ->
  expand_expr(Expr);
expand_expr({block, _, Exprs}) ->
  lists:map(fun expand_expr/1, Exprs);

%% Anything else...
expand_expr(Expr) ->
  throw({expand_error, {expr_invalid, get_meta(Expr)}}).

%% -----------------------------------------------------------------------------
%% Type Expanding
%% -----------------------------------------------------------------------------

%% Builtins.
expand_type_inner({ident, _, int}) ->
  c_type_int;
expand_type_inner({ident, _, float}) ->
  c_type_float;
expand_type_inner({ident, _, atom}) ->
  c_type_atom;
expand_type_inner({ident, _, str}) ->
  c_type_str;
expand_type_inner({ident, _, bytes}) ->
  c_type_bytes;
expand_type_inner({ident, _, bits}) ->
  c_type_bits;
expand_type_inner({ident, _, ref}) ->
  c_type_ref;

% Unit type.
expand_type_inner({expand, _, {op, _, '(_)'}, []}) ->
  c_type_unit;

%% Anything else...
expand_type_inner(Type) ->
  throw({expand_error, {type_invalid, get_meta(Type)}}).

%% Where clauses.
%% TODO: implement.
expand_type_where(Where) ->
  throw({expand_error, {type_where_invalid, get_meta(Where)}}).

%% -----------------------------------------------------------------------------
%% Utilities
%% -----------------------------------------------------------------------------

get_meta({source, Meta, _})          -> Meta;
get_meta({integer_lit, Meta, _})     -> Meta;
get_meta({float_lit, Meta, _})       -> Meta;
get_meta({atom_lit, Meta, _})        -> Meta;
get_meta({string_lit, Meta, _})      -> Meta;
get_meta({ident, Meta, _})           -> Meta;
get_meta({type_param, Meta, _})      -> Meta;
get_meta({blank, Meta})              -> Meta;
get_meta({op, Meta, _})              -> Meta;
get_meta({block, Meta, _})           -> Meta;
get_meta({expand, Meta, _, _})       -> Meta;
get_meta({args, Meta, _})            -> Meta;
get_meta({tag, Meta, _, _})          -> Meta;
get_meta({attribute, Meta, _, _})    -> Meta;
get_meta({inner_attribute, Meta, _}) -> Meta.
