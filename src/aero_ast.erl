%%% Contains the Aero AST.

-module(aero_ast).

-export([meta/1, metas/1]).

-export_type([t/0, source/0]).
-export_type([int_lit/0, float_lit/0, atom_lit/0, str_lit/0]).
-export_type([ident/0, type_param/0, blank/0, op/0]).
-export_type([block/0, expand/0, args/0, tag/0, attr/0, inner_attr/0]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

%% Any AST element.
-type t() :: source()
           | int_lit()
           | float_lit() 
           | atom_lit()
           | str_lit()
           | ident()
           | type_param()
           | blank()
           | op()
           | block()
           | expand()
           | args()
           | tag()
           | attr()
           | inner_attr().

%% Top AST element.
-type source() :: {source, meta(), [t()]}.

%% Literals.
-type int_lit()   :: {int_lit, meta(), integer()}.
-type float_lit() :: {float_lit, meta(), float()}.
-type atom_lit()  :: {atom_lit, meta(), atom()}.
-type str_lit()   :: {str_lit, meta(), binary()}.

%% Names and operators.
-type ident()      :: {ident, meta(), atom()}.
-type type_param() :: {type_param, meta(), atom()}.
-type blank()      :: {blank, meta()}.
-type op()         :: {op, meta(), atom()}.

%% Group of expressions.
-type block() :: {block, meta(), [t()]}.

%% Expand element and arguments.
-type expand() :: {expand, meta(), t(), [t()]}.
-type args()   :: {args, meta(), [t()]}.

%% Tags and attributes.
-type tag()        :: {tag, meta(), t(), t()}.
-type attr()       :: {attr, meta(), t(), t()}.
-type inner_attr() :: {inner_attr, meta(), t()}.

%% AST metadata.
-type meta() :: [term()].

%% Get metadata from an AST element.
-spec meta(t()) -> meta().
meta({source, Meta, _})     -> Meta;
meta({int_lit, Meta, _})    -> Meta;
meta({float_lit, Meta, _})  -> Meta;
meta({atom_lit, Meta, _})   -> Meta;
meta({str_lit, Meta, _})    -> Meta;
meta({ident, Meta, _})      -> Meta;
meta({type_param, Meta, _}) -> Meta;
meta({blank, Meta})         -> Meta;
meta({op, Meta, _})         -> Meta;
meta({block, Meta, _})      -> Meta;
meta({expand, Meta, _, _})  -> Meta;
meta({args, Meta, _})       -> Meta;
meta({tag, Meta, _, _})     -> Meta;
meta({attr, Meta, _, _})    -> Meta;
meta({inner_attr, Meta, _}) -> Meta.

%% Get metadata from multiple AST elements at once.
-spec metas([t()]) -> [meta()].
metas(Asts) when is_list(Asts) -> [meta(Ast) || Ast <- Asts].
