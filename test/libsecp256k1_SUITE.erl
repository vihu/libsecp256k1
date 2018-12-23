-module(libsecp256k1_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([
         create_keys_test/1,
         invalid_keys_test/1,
         import_export_test/1,
         signing_test/1,
         tweaks_test/1,
         blank_msg_test/1,
         compact_signing_test/1,
         sha256_test/1
        ]).

all() ->
    [
     create_keys_test,
     invalid_keys_test,
     import_export_test,
     signing_test,
     tweaks_test,
     blank_msg_test,
     compact_signing_test,
     sha256_test
    ].

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

create_keys_test(_Config) ->
    A = crypto:strong_rand_bytes(32),
    {ok, B} = libsecp256k1:ec_pubkey_create(A, compressed),
    {ok, B2} = libsecp256k1:ec_pubkey_create(A, uncompressed),
    {ok, C} = libsecp256k1:ec_pubkey_decompress(B),
    ?assertEqual(B2, C),
    ?assertEqual(ok, libsecp256k1:ec_pubkey_verify(B)),
    ?assertEqual(ok, libsecp256k1:ec_pubkey_verify(C)).

invalid_keys_test(_Config) ->
    A = crypto:strong_rand_bytes(16),
    ?assertMatch({error, _Msg}, libsecp256k1:ec_pubkey_create(A, compressed)),
    ?assertMatch({error, _Msg}, libsecp256k1:ec_pubkey_create(A, invalidflag)).

import_export_test(_Config) ->
    A = crypto:strong_rand_bytes(32),
    {ok, B} = libsecp256k1:ec_privkey_export(A, compressed),
    {ok, C} = libsecp256k1:ec_privkey_import(B),
    ?assertEqual(A, C).

tweaks_test(_Config) ->
    <<A:256/bitstring, Tweak:256/bitstring>> = crypto:strong_rand_bytes(64),
    {ok, Pubkey} = libsecp256k1:ec_pubkey_create(A, compressed),
    {ok, A2} = libsecp256k1:ec_privkey_tweak_add(A, Tweak),
    {ok, A3} = libsecp256k1:ec_privkey_tweak_mul(A, Tweak),
    {ok, Pubkey2} = libsecp256k1:ec_pubkey_tweak_add(Pubkey, Tweak),
    {ok, Pubkey3} = libsecp256k1:ec_pubkey_tweak_mul(Pubkey, Tweak),
    {ok, PubkeyA2} = libsecp256k1:ec_pubkey_create(A2, compressed),
    {ok, PubkeyA3} = libsecp256k1:ec_pubkey_create(A3, compressed),
    ?assertEqual(Pubkey2, PubkeyA2),
    ?assertEqual(Pubkey3, PubkeyA3).

signing_test(_Config) ->
    Msg = <<"This is a secret message...">>,
    A = crypto:strong_rand_bytes(32),
    {ok, Pubkey} = libsecp256k1:ec_pubkey_create(A, compressed),
    {ok, Signature} = libsecp256k1:ecdsa_sign(Msg, A, default, <<>>),
    ?assertEqual(ok, libsecp256k1:ecdsa_verify(Msg, Signature, Pubkey)).

blank_msg_test(_Config) ->
    Msg = <<>>,
    A = crypto:strong_rand_bytes(32),
    {ok, Pubkey} = libsecp256k1:ec_pubkey_create(A, compressed),
    {ok, Signature} = libsecp256k1:ecdsa_sign(Msg, A, default, <<>>),
    ?assertEqual(ok, libsecp256k1:ecdsa_verify(Msg, Signature, Pubkey)).

compact_signing_test(_Config) ->
    Msg = <<"This is a very secret compact message...">>,
    A = crypto:strong_rand_bytes(32),
    {ok, Pubkey} = libsecp256k1:ec_pubkey_create(A, uncompressed),
    {ok, Signature, RecoveryID} = libsecp256k1:ecdsa_sign_compact(Msg, A, default, <<>>),
    {ok, RecoveredKey} = libsecp256k1:ecdsa_recover_compact(Msg, Signature, uncompressed, RecoveryID),
    ?assertEqual(Pubkey, RecoveredKey),
    ?assertEqual(ok, libsecp256k1:ecdsa_verify_compact(Msg, Signature, Pubkey)).

sha256_test(_Config) ->
    A = crypto:strong_rand_bytes(64),
    DoubleHashed = crypto:hash(sha256, crypto:hash(sha256, A)),
    ?assertEqual(DoubleHashed, libsecp256k1:sha256(libsecp256k1:sha256(A))),
    ?assertEqual(DoubleHashed, libsecp256k1:dsha256(A)).

