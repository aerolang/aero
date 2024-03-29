%%% Contains Aero tokens.

-module(aero_token).

-export([meta/1, metas/1]).

-export_type([t/0]).
-export_type([int_lit/0, float_lit/0, sym_lit/0, str_lit/0]).
-export_type([ident/0, type_param/0, blank/0, op/0]).
-export_type([space/0, newline/0, eof/0]).

%% -----------------------------------------------------------------------------
%% Public API
%% -----------------------------------------------------------------------------

%% Any token.
-type t() :: int_lit()
           | float_lit()
           | sym_lit()
           | str_lit()
           | ident()
           | type_param()
           | blank()
           | op()
           | space()
           | newline()
           | eof().

%% Literals.
-type int_lit()   :: {int_lit, meta(), integer()}.
-type float_lit() :: {float_lit, meta(), float()}.
-type sym_lit()   :: {sym_lit, meta(), atom()}.
-type str_lit()   :: {str_lit, meta(), binary()}.

%% Names and operators.
-type ident()      :: {ident, meta(), atom()}.
-type type_param() :: {type_param, meta(), atom()}.
-type blank()      :: {blank, meta()}.
-type op()         :: {op, meta(), atom()}.

%% Whitespace.
-type space()   :: {space, meta()}.
-type newline() :: {newline, meta()}.
-type eof()     :: {eof, meta()}.

%% Token metadata.
-type meta() :: [term()].

%% Get metadata from a token.
-spec meta(t()) -> meta().
meta({int_lit, Meta, _})    -> Meta;
meta({float_lit, Meta, _})  -> Meta;
meta({sym_lit, Meta, _})    -> Meta;
meta({str_lit, Meta, _})    -> Meta;
meta({ident, Meta, _})      -> Meta;
meta({type_param, Meta, _}) -> Meta;
meta({blank, Meta})         -> Meta;
meta({op, Meta, _})         -> Meta;
meta({space, Meta})         -> Meta;
meta({newline, Meta})       -> Meta;
meta({eof, Meta})           -> Meta.

%% Get metadata from multiple tokens at once.
metas(Tokens) when is_list(Tokens) -> [meta(Token) || Token <- Tokens].
