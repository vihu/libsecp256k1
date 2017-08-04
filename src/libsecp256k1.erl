%% Copyright 2015 Matthew Branton. All Rights Reserved.
%% Use of this source code is governed by the MIT
%% license that can be found in the LICENSE file.
%%
%% @doc Erlang NIF bindings
%% <a href="https://github.com/bitcoin/secp256k1">libsec256k1</a> elliptic curve library

-module(libsecp256k1).
-author("mbranton@emberfinancial.com").

-export([dsha256/1,
		 sha256/1,
		 hmac_sha256/2,
		 rand32/0,
		 rand256/0,
		 ec_seckey_verify/1,
		 ec_pubkey_create/2,
		 ec_pubkey_decompress/1,
		 ec_pubkey_verify/1,
		 ec_privkey_export/2,
		 ec_privkey_import/1,
		 ec_privkey_tweak_add/2,
		 ec_pubkey_tweak_add/2,
		 ec_privkey_tweak_mul/2,
		 ec_pubkey_tweak_mul/2,
		 ecdsa_sign/4,
		 ecdsa_verify/3,
		 ecdsa_sign_compact/4,
		 ecdsa_recover_compact/4]).

-on_load(init/0).

-define(APPNAME, libsecp256k1).
-define(LIBNAME, libsecp256k1_nif).

-type hash() :: binary().
-type public_key() :: binary().
-type private_key() :: binary().
-type compression() :: compressed | uncompressed.
-type signature() :: binary().
-type recovery_id() :: integer().

%% API
-spec dsha256(binary()) -> hash().
dsha256(_) ->
	not_loaded(?LINE).

-spec sha256(binary()) -> hash().
sha256(_) ->
    not_loaded(?LINE).

-spec hmac_sha256(binary(), binary()) -> hash().
hmac_sha256(_, _) ->
	not_loaded(?LINE).

%% testing PRNG
-spec rand32() -> binary().
rand32() ->
	not_loaded(?LINE).

-spec rand256() -> binary().
rand256() ->
	not_loaded(?LINE).

%% Ecdsa functions
-spec ec_seckey_verify(private_key()) -> ok | error.
ec_seckey_verify(_) ->
	not_loaded(?LINE).

-spec ec_pubkey_create(private_key(), compression()) -> {ok, public_key()} | {error, string()}.
ec_pubkey_create(_, _) ->
	not_loaded(?LINE).

-spec ec_pubkey_decompress(public_key()) -> {ok, public_key()} | {error, string()}.
ec_pubkey_decompress(_) ->
	not_loaded(?LINE).

-spec ec_pubkey_verify(public_key()) -> ok | error.
ec_pubkey_verify(_) ->
	not_loaded(?LINE).

-spec ec_privkey_export(private_key(), compression()) -> {ok, binary()} | {error, string()}.
ec_privkey_export(_, _) ->
	not_loaded(?LINE).

-spec ec_privkey_import(binary()) -> {ok, private_key()} | {error, string()}.
ec_privkey_import(_) ->
	not_loaded(?LINE).

-spec ec_privkey_tweak_add(private_key(), binary()) -> {ok, private_key()} | {error, string()}.
ec_privkey_tweak_add(_, _) ->
	not_loaded(?LINE).

-spec ec_pubkey_tweak_add(public_key(), binary()) -> {ok, public_key()} | {error, string()}.
ec_pubkey_tweak_add(_, _) ->
	not_loaded(?LINE).

-spec ec_privkey_tweak_mul(private_key(), binary()) -> {ok, private_key()} | {error, string()}.
ec_privkey_tweak_mul(_, _) ->
	not_loaded(?LINE).

-spec ec_pubkey_tweak_mul(public_key(), binary()) -> {ok, public_key()} | {error, string()}.
ec_pubkey_tweak_mul(_, _) -> 
	not_loaded(?LINE).

-spec ecdsa_sign(binary(), private_key(), atom(), binary()) -> {ok, signature()} | {error, string()}.
ecdsa_sign(_, _, _, _) ->
	not_loaded(?LINE).

-spec ecdsa_verify(binary(), signature(), public_key()) -> ok | error.
ecdsa_verify(_, _, _) ->
	not_loaded(?LINE).

-spec ecdsa_sign_compact(binary(), private_key(), atom(), binary()) -> {ok, signature(), recovery_id()} | {error, string()}.
ecdsa_sign_compact(_, _, _, _) ->
	not_loaded(?LINE).

-spec ecdsa_recover_compact(binary(), signature(), compression(), recovery_id()) -> {ok, public_key()}.
ecdsa_recover_compact(_, _, _, _) ->
	not_loaded(?LINE).

%% Iternal functions

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

% This is just a simple place holder. It mostly shouldn't ever be called
% unless there was an unexpected error loading the NIF shared library.

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
