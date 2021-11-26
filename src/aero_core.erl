%%% Contains the schema for Core Aero.
%%%
%%% This format gives a simple language for Aero to compile down to, designed
%%% to make it easy to implement semantic passes over a program. Most of it also
%%% corresponds to Core Erlang, allowing for codegen to be done (relatively)
%%% easily.
%%%
%%% This module exports types for all element kinds: definitions, expressions,
%%% patterns, and types. With each type is a constructor for convenience.

-module(aero_core).

-export([c_pkg/3, c_mod/4, c_def_func/4, c_def_const/5, c_def_mod/3]).
-export([c_block/2, c_bool/2, c_int/2, c_float/2, c_sym/2, c_str/2, c_unit/1, c_tuple/2,
         c_cons/3, c_nil/1, c_dict/2, c_func/5, c_call/3, c_apply/3, c_var/2, c_path/2, c_let/4,
         c_letrec/4, c_match/3, c_args/2]).
-export([c_pat_bool/2, c_pat_int/2, c_pat_float/2, c_pat_sym/2, c_pat_str/2, c_pat_unit/1,
         c_pat_tuple/2, c_pat_cons/3, c_pat_nil/1, c_pat_dict/2, c_pat_var/2, c_pat_args/2]).
-export([c_type_bool/1, c_type_int/1, c_type_float/1, c_type_sym/1, c_type_str/1, c_type_bytes/1,
         c_type_bits/1, c_type_ref/1, c_type_unit/1, c_type_tuple/2, c_type_list/2, c_type_dict/3,
         c_type_func/3, c_type_uniq/2, c_type_dyn/2, c_type_any/1, c_type_never/1, c_type_wld/1,
         c_type_mbox/2, c_type_addr/2, c_type_var/2, c_type_path/2, c_type_tag/2, c_type_struct/3,
         c_type_proto/3, c_type_union/2, c_type_inter/2, c_type_where/3]).

-export_type([c_any/0]).
-export_type([c_pkg/0, c_mod/0, c_def/0, c_def_func/0, c_def_const/0, c_def_mod/0, c_vis/0]).
-export_type([c_expr/0, c_block/0, c_bool/0, c_int/0, c_float/0, c_sym/0, c_str/0, c_unit/0,
              c_tuple/0, c_cons/0, c_nil/0,  c_dict/0, c_func/0, c_call/0, c_apply/0, c_var/0,
              c_path/0, c_let/0, c_letrec/0, c_match/0, c_args/0]).
-export_type([c_pat/0, c_pat_bool/0, c_pat_int/0, c_pat_float/0, c_pat_sym/0, c_pat_str/0,
              c_pat_unit/0, c_pat_tuple/0, c_pat_cons/0, c_pat_nil/0, c_pat_dict/0, c_pat_var/0,
              c_pat_args/0]).
-export_type([c_type/0, c_type_bool/0, c_type_int/0, c_type_float/0, c_type_sym/0, c_type_str/0,
              c_type_bytes/0, c_type_bits/0, c_type_ref/0, c_type_unit/0, c_type_tuple/0,
              c_type_list/0, c_type_dict/0, c_type_func/0, c_type_uniq/0, c_type_dyn/0,
              c_type_any/0, c_type_never/0, c_type_wld/0, c_type_mbox/0, c_type_addr/0,
              c_type_var/0, c_type_path/0, c_type_tag/0, c_type_struct/0, c_type_proto/0,
              c_type_union/0, c_type_inter/0, c_type_where/0]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

%% Any Aero Core element.
-type c_any() :: c_pkg()
               | c_mod()
               | c_def()
               | c_expr()
               | c_pat()
               | c_type().

%% Top-level of Core Aero, represents a whole package.
-type c_pkg() :: {c_pkg, meta(), atom(), [c_mod()]}.

%% Modules.
-type c_mod()       :: {c_mod, meta(), c_path(), c_mod_attrs(), [c_def()]}.
-type c_mod_attrs() :: [{c_sym(), c_expr()}].

%% Definitions.
-type c_def() :: c_def_func()
               | c_def_const()
               | c_def_mod().

-type c_def_func()  :: {c_def_func, meta(), c_path(), c_vis(), c_func()}.
-type c_def_const() :: {c_def_const, meta(), c_path(), c_vis(), c_type(), c_expr()}.
-type c_def_mod()   :: {c_def_mod, meta(), c_path(), c_vis()}.

%% Definition visibility.
-type c_vis() :: c_vis_pub
               | c_vis_priv.

%% Any expression in Core Aero.
-type c_expr() :: c_block()
                | c_bool()
                | c_int()
                | c_float()
                | c_sym()
                | c_str()
                | c_unit()
                | c_tuple()
                | c_cons()
                | c_nil()
                | c_dict()
                | c_func()
                | c_call()
                | c_apply()
                | c_var()
                | c_path()
                | c_let()
                | c_letrec()
                | c_match()
                | c_args().

%% A group of expressions with the last giving the value of the group.
-type c_block() :: {c_block, meta(), [c_expr()]}.

%% Literals.
-type c_bool()  :: {c_bool, meta(), boolean()}.
-type c_int()   :: {c_int, meta(), integer()}.
-type c_float() :: {c_float, meta(), float()}.
-type c_sym()   :: {c_sym, meta(), atom()}.
-type c_str()   :: {c_str, meta(), binary()}.

%% Unit value.
-type c_unit() :: {c_unit, meta()}.

%% Collections.
-type c_tuple() :: {c_tuple, meta(), [c_expr()]}.
-type c_cons()  :: {c_cons, meta(), c_expr(), c_expr()}.
-type c_nil()   :: {c_nil, meta()}.
-type c_dict()  :: {c_dict, meta(), [c_dict_pair()]}.

-type c_dict_pair() :: {c_expr(), c_expr()}.

%% A function expression.
-type c_func() :: {c_func, meta(), c_func_args(), c_func_result(), c_func_where(), c_func_body()}.

-type c_func_args()   :: [{c_var(), c_type()}].
-type c_func_result() :: c_type().
-type c_func_where()  :: [{c_type(), c_type()}].
-type c_func_body()   :: c_expr().

%% A call to a named function.
-type c_call() :: {c_call, meta(), c_path(), c_call_args()}.

-type c_call_args() :: [c_expr()].

%% A call to an anonymous function.
-type c_apply() :: {c_apply, meta(), c_var(), c_apply_args()}.

-type c_apply_args() :: [c_expr()].

%% Variables.
-type c_var() :: {c_var, meta(), atom()}.

%% Paths.
-type c_path() :: {c_path, meta(), [c_var()]}.

%% Let and letrec expressions.
-type c_let()    :: {c_let, meta(), c_var(), c_type(), c_expr()}.
-type c_letrec() :: {c_letrec, meta(), c_var(), c_type(), c_func()}.

%% Match.
-type c_match() :: {c_match, meta(), c_expr(), c_match_cases()}.

-type c_match_cases() :: [{c_pat(), c_expr()}].

%% A list of expressions from function arguments.
-type c_args() :: {c_args, meta(), [c_expr()]}.

%% Any pattern in Core Aero.
-type c_pat() :: c_pat_bool()
               | c_pat_int()
               | c_pat_float()
               | c_pat_sym()
               | c_pat_str()
               | c_pat_unit()
               | c_pat_tuple()
               | c_pat_cons()
               | c_pat_nil()
               | c_pat_dict()
               | c_pat_var()
               | c_pat_args().

%% Literal patterns.
-type c_pat_bool()  :: {c_pat_bool, meta(), boolean()}.
-type c_pat_int()   :: {c_pat_int, meta(), integer()}.
-type c_pat_float() :: {c_pat_float, meta(), float()}.
-type c_pat_sym()   :: {c_pat_sym, meta(), atom()}.
-type c_pat_str()   :: {c_pat_str, meta(), binary()}.

%% Unit pattern.
-type c_pat_unit() :: {c_pat_unit, meta()}.

%% Collection patterns.
-type c_pat_cons()  :: {c_pat_cons, meta(), c_pat(), c_pat()}.
-type c_pat_nil()   :: {c_pat_nil, meta()}.
-type c_pat_tuple() :: {c_pat_tuple, meta(), [c_pat()]}.
-type c_pat_dict()  :: {c_pat_dict, meta(), [{c_pat(), c_pat()}]}.

%% Pattern variable.
-type c_pat_var() :: {c_pat_var, meta(), atom()}.

%% Args pattern.
-type c_pat_args() :: {c_pat_args, meta(), [c_pat()]}.

%% Any type in Core Aero.
-type c_type() :: c_type_bool()
                | c_type_int()
                | c_type_float()
                | c_type_sym()
                | c_type_str()
                | c_type_bytes()
                | c_type_bits()
                | c_type_ref()
                | c_type_unit()
                | c_type_tuple()
                | c_type_list()
                | c_type_dict()
                | c_type_func()
                | c_type_uniq()
                | c_type_dyn()
                | c_type_wld()
                | c_type_never()
                | c_type_mbox()
                | c_type_addr()
                | c_type_var()
                | c_type_path()
                | c_type_tag()
                | c_type_struct()
                | c_type_proto()
                | c_type_union()
                | c_type_inter()
                | c_type_where().

%% Literal types.
-type c_type_bool()  :: {c_type_bool, meta()}.
-type c_type_int()   :: {c_type_int, meta()}.
-type c_type_float() :: {c_type_float, meta()}.
-type c_type_sym()   :: {c_type_sym, meta()}.
-type c_type_str()   :: {c_type_str, meta()}.

%% Bytes and bits types.
-type c_type_bytes() :: {c_type_bytes, meta()}.
-type c_type_bits()  :: {c_type_bits, meta()}.

%% References type.
-type c_type_ref() :: {c_type_ref, meta()}.

%% Unit type.
-type c_type_unit() :: {c_type_unit, meta()}.

%% Collection types.
-type c_type_tuple() :: {c_type_tuple, meta(), [c_type()]}.
-type c_type_list()  :: {c_type_list, meta(), c_type()}.
-type c_type_dict()  :: {c_type_dict, meta(), c_type(), c_type()}.

%% Function type.
-type c_type_func() :: {c_type_func, meta(), [c_type()], c_type()}.

%% Unique and dynamic types.
-type c_type_uniq() :: {c_type_uniq, meta(), c_type()}.
-type c_type_dyn()  :: {c_type_dyn, meta(), c_type()}.

%% Top and bottom types.
-type c_type_any()   :: {c_type_any, meta()}.
-type c_type_never() :: {c_type_never, meta()}.

%% Concurrent primitives.
-type c_type_wld()  :: {c_type_wld, meta()}.
-type c_type_mbox() :: {c_type_mbox, meta(), c_type()}.
-type c_type_addr() :: {c_type_addr, meta(), c_type()}.

%% Type variables.
-type c_type_var() :: {c_type_var, meta(), atom()}.

%% Type paths.
-type c_type_path() :: {c_type_path, meta(), [c_type_var()]}.

%% Tags.
-type c_type_tag() :: {c_type_tag, meta(), atom()}.

%% Structs and protocols.
-type c_type_struct() :: {c_type_struct, meta(), c_type_path(), [c_type()]}.
-type c_type_proto()  :: {c_type_proto, meta(), c_type_path(), [c_type()]}.

%% Type unions and intersections
-type c_type_union() :: {c_type_union, meta(), [c_type()]}.
-type c_type_inter() :: {c_type_inter, meta(), [c_type()]}.

%% Type with where clauses.
-type c_type_where() :: {c_type_where, meta(), c_type(), [{c_type(), c_type()}]}.

%% Core Aero metadata.
-type meta() :: [term()].

%% Create a package.
-spec c_pkg(meta(), atom(), [c_mod()]) -> c_pkg().
c_pkg(Meta, Name, Modules) ->
  {c_pkg, Meta, Name, Modules}.

%% Create a module.
-spec c_mod(meta(), c_path(), c_mod_attrs(), [c_def()]) -> c_mod().
c_mod(Meta, Path, Attrs, Defs) ->
  {c_mod, Meta, Path, Attrs, Defs}.

%% Create a function definition.
-spec c_def_func(meta(), c_path(), c_vis(), c_func()) -> c_def_func().
c_def_func(Meta, Path, Vis, Func) ->
  {c_def_func, Meta, Path, Vis, Func}.

%% Create a constant definition.
-spec c_def_const(meta(), c_path(), c_vis(), c_type(), c_expr()) -> c_def_const().
c_def_const(Meta, Path, Vis, Type, Expr) ->
  {c_def_const, Meta, Path, Vis, Type, Expr}.

%% Create a module definition.
-spec c_def_mod(meta(), c_path(), c_vis()) -> c_def_mod().
c_def_mod(Meta, Path, Vis) ->
  {c_def_mod, Meta, Path, Vis}.

%% Create a block expression.
-spec c_block(meta(), [c_expr()]) -> c_block().
c_block(Meta, Exprs) ->
  {c_block, Meta, Exprs}.

%% Create a boolean expression.
-spec c_bool(meta(), boolean()) -> c_bool().
c_bool(Meta, Boolean) ->
  {c_bool, Meta, Boolean}.

%% Create an integer expression.
-spec c_int(meta(), integer()) -> c_int().
c_int(Meta, Integer) ->
  {c_int, Meta, Integer}.

%% Create a float expression.
-spec c_float(meta(), float()) -> c_float().
c_float(Meta, Float) ->
  {c_float, Meta, Float}.

%% Create an atom expression.
-spec c_sym(meta(), atom()) -> c_sym().
c_sym(Meta, Symbol) ->
  {c_sym, Meta, Symbol}.

%% Create a string expression.
-spec c_str(meta(), binary()) -> c_str().
c_str(Meta, String) ->
  {c_str, Meta, String}.

%% Create a unit expression.
-spec c_unit(meta()) -> c_unit().
c_unit(Meta) ->
  {c_unit, Meta}.

%% Create a tuple expression.
-spec c_tuple(meta(), [c_expr()]) -> c_tuple().
c_tuple(Meta, Exprs) ->
  {c_tuple, Meta, Exprs}.

%% Create a cons expression.
-spec c_cons(meta(), c_expr(), c_expr()) -> c_cons().
c_cons(Meta, Head, Tail) ->
  {c_cons, Meta, Head, Tail}.

%% Create a nil expression.
-spec c_nil(meta()) -> c_nil().
c_nil(Meta) ->
  {c_nil, Meta}.

%% Create a dictionary expression.
-spec c_dict(meta(), [c_dict_pair()]) -> c_dict().
c_dict(Meta, Pairs) ->
  {c_dict, Meta, Pairs}.

%% Create a function expression.
-spec c_func(meta(), c_func_args(), c_func_result(), c_func_where(), c_func_body()) -> c_func().
c_func(Meta, Args, Result, Where, Body) ->
  {c_func, Meta, Args, Result, Where, Body}.

%% Create a call expression.
-spec c_call(meta(), c_path(), c_call_args()) -> c_call().
c_call(Meta, Path, Args) ->
  {c_call, Meta, Path, Args}.

%% Create an apply expression.
-spec c_apply(meta(), c_var(), c_apply_args()) -> c_apply().
c_apply(Meta, Var, Args) ->
  {c_apply, Meta, Var, Args}.

%% Create a variable.
-spec c_var(meta(), atom()) -> c_var().
c_var(Meta, Name) ->
  {c_var, Meta, Name}.

%% Create a path to an expression.
-spec c_path(meta(), [c_var()]) -> c_path().
c_path(Meta, Vars) ->
  {c_path, Meta, Vars}.

%% Create a let expression.
-spec c_let(meta(), c_var(), c_type(), c_expr()) -> c_let().
c_let(Meta, Var, Type, Expr) ->
  {c_let, Meta, Var, Type, Expr}.

%% Create a letrec expression.
-spec c_letrec(meta(), c_var(), c_type(), c_func()) -> c_letrec().
c_letrec(Meta, Var, Type, Func) ->
  {c_letrec, Meta, Var, Type, Func}.

%% Create a match expression.
-spec c_match(meta(), c_expr(), c_match_cases()) -> c_match().
c_match(Meta, Expr, Cases) ->
  {c_match, Meta, Expr, Cases}.

%% Create an args expression.
-spec c_args(meta(), [c_expr()]) -> c_args().
c_args(Meta, Exprs) ->
  {c_args, Meta, Exprs}.

%% Create a boolean pattern.
-spec c_pat_bool(meta(), boolean()) -> c_pat_bool().
c_pat_bool(Meta, Boolean) ->
  {c_pat_bool, Meta, Boolean}.

%% Create an integer pattern.
-spec c_pat_int(meta(), integer()) -> c_pat_int().
c_pat_int(Meta, Integer) ->
  {c_pat_int, Meta, Integer}.

%% Create a float pattern.
-spec c_pat_float(meta(), float()) -> c_pat_float().
c_pat_float(Meta, Float) ->
  {c_pat_float, Meta, Float}.

%% Create an atom pattern.
-spec c_pat_sym(meta(), atom()) -> c_pat_sym().
c_pat_sym(Meta, Symbol) ->
  {c_pat_sym, Meta, Symbol}.

%% Create a string pattern.
-spec c_pat_str(meta(), binary()) -> c_pat_str().
c_pat_str(Meta, String) ->
  {c_pat_str, Meta, String}.

%% Create a unit pattern.
-spec c_pat_unit(meta()) -> c_pat_unit().
c_pat_unit(Meta) ->
  {c_pat_unit, Meta}.

%% Create a cons pattern.
-spec c_pat_cons(meta(), c_pat(), c_pat()) -> c_pat_cons().
c_pat_cons(Meta, Head, Tail) ->
  {c_pat_cons, Meta, Head, Tail}.

%% Create a nil pattern.
-spec c_pat_nil(meta()) -> c_pat_nil().
c_pat_nil(Meta) ->
  {c_pat_nil, Meta}.

%% Create a tuple pattern.
-spec c_pat_tuple(meta(), [c_pat()]) -> c_pat_tuple().
c_pat_tuple(Meta, Pats) ->
  {c_pat_tuple, Meta, Pats}.

%% Create a dictionary pattern.
-spec c_pat_dict(meta(), [{c_pat(), c_pat()}]) -> c_pat_dict().
c_pat_dict(Meta, Pairs) ->
  {c_pat_dict, Meta, Pairs}.

%% Create a pattern variable.
-spec c_pat_var(meta(), atom()) -> c_pat_var().
c_pat_var(Meta, Name) ->
  {c_pat_var, Meta, Name}.

%% Create an args pattern.
-spec c_pat_args(meta(), [c_pat()]) -> c_pat_args().
c_pat_args(Meta, Pats) ->
  {c_pat_args, Meta, Pats}.

%% Create a boolean type.
-spec c_type_bool(meta()) -> c_type_bool().
c_type_bool(Meta) ->
  {c_type_bool, Meta}.

%% Create a boolean type.
-spec c_type_int(meta()) -> c_type_int().
c_type_int(Meta) ->
  {c_type_int, Meta}.

%% Create a float type.
-spec c_type_float(meta()) -> c_type_float().
c_type_float(Meta) ->
  {c_type_float, Meta}.

%% Create an atom type.
-spec c_type_sym(meta()) -> c_type_sym().
c_type_sym(Meta) ->
  {c_type_sym, Meta}.

%% Create a string type.
-spec c_type_str(meta()) -> c_type_str().
c_type_str(Meta) ->
  {c_type_str, Meta}.

%% Create a bytes type.
-spec c_type_bytes(meta()) -> c_type_bytes().
c_type_bytes(Meta) ->
  {c_type_bytes, Meta}.

%% Create a bits type.
-spec c_type_bits(meta()) -> c_type_bits().
c_type_bits(Meta) ->
  {c_type_bits, Meta}.

%% Create a ref type.
-spec c_type_ref(meta()) -> c_type_ref().
c_type_ref(Meta) ->
  {c_type_ref, Meta}.

%% Create a unit type.
-spec c_type_unit(meta()) -> c_type_unit().
c_type_unit(Meta) ->
  {c_type_unit, Meta}.

%% Create a tuple type.
-spec c_type_tuple(meta(), [c_type()]) -> c_type_tuple().
c_type_tuple(Meta, Types) ->
  {c_type_tuple, Meta, Types}.

%% Create a list type.
-spec c_type_list(meta(), c_type()) -> c_type_list().
c_type_list(Meta, Type) ->
  {c_type_list, Meta, Type}.

%% Create a dictionary type.
-spec c_type_dict(meta(), c_type(), c_type()) -> c_type_dict().
c_type_dict(Meta, Key, Value) ->
  {c_type_dict, Meta, Key, Value}.

%% Create a function type.
-spec c_type_func(meta(), [c_type()], c_type()) -> c_type_func().
c_type_func(Meta, Args, Result) ->
  {c_type_func, Meta, Args, Result}.

%% Create a unique type.
-spec c_type_uniq(meta(), c_type()) -> c_type_uniq().
c_type_uniq(Meta, Type) ->
  {c_type_uniq, Meta, Type}.

%% Create a dynamic type.
-spec c_type_dyn(meta(), c_type()) -> c_type_dyn().
c_type_dyn(Meta, Type) ->
  {c_type_dyn, Meta, Type}.

%% Create an any type.
-spec c_type_any(meta()) -> c_type_any().
c_type_any(Meta) ->
  {c_type_any, Meta}.

%% Create a never type.
-spec c_type_never(meta()) -> c_type_never().
c_type_never(Meta) ->
  {c_type_never, Meta}.

%% Create a world type.
-spec c_type_wld(meta()) -> c_type_wld().
c_type_wld(Meta) ->
  {c_type_wld, Meta}.

%% Create a mailbox type.
-spec c_type_mbox(meta(), c_type()) -> c_type_mbox().
c_type_mbox(Meta, Type) ->
  {c_type_mbox, Meta, Type}.

%% Create an address type.
-spec c_type_addr(meta(), c_type()) -> c_type_addr().
c_type_addr(Meta, Type) ->
  {c_type_addr, Meta, Type}.

%% Create a type variable.
-spec c_type_var(meta(), atom()) -> c_type_var().
c_type_var(Meta, Name) ->
  {c_type_var, Meta, Name}.

%% Create a path to a type.
-spec c_type_path(meta(), [c_type_var()]) -> c_type_path().
c_type_path(Meta, Vars) ->
  {c_type_path, Meta, Vars}.

%% Create a type tag.
-spec c_type_tag(meta(), atom()) -> c_type_tag().
c_type_tag(Meta, Tag) ->
  {c_type_tag, Meta, Tag}.

%% Create a struct type.
-spec c_type_struct(meta(), c_type_path(), [c_type()]) -> c_type_struct().
c_type_struct(Meta, Path, Types) ->
  {c_type_struct, Meta, Path, Types}.

%% Create a protocol type.
-spec c_type_proto(meta(), c_type_path(), [c_type()]) -> c_type_proto().
c_type_proto(Meta, Path, Types) ->
  {c_type_proto, Meta, Path, Types}.

%% Create a union type.
-spec c_type_union(meta(), [c_type()]) -> c_type_union().
c_type_union(Meta, Types) ->
  {c_type_union, Meta, Types}.

%% Create an intersection type.
-spec c_type_inter(meta(), [c_type()]) -> c_type_inter().
c_type_inter(Meta, Types) ->
  {c_type_inter, Meta, Types}.

%% Create a type with where clauses.
-spec c_type_where(meta(), c_type(), [{c_type(), c_type()}]) -> c_type_where().
c_type_where(Meta, Type, Where) ->
  {c_type_where, Meta, Type, Where}.
