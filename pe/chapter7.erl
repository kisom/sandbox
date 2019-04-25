-module(chapter7).
-export([breverse/1, term_to_packet/1, packet_to_term/1, reverse_byte/1,
	 reverse_bits/1, reverse_bitlist/1, test/0]).

%% Exercises
%% 1. Write a function that reverses the order of bytes in a binary.
breverse(Bin) ->
    binary:list_to_bin(lists:reverse(binary:bin_to_list(Bin))).

%% 2. Write a function term_to_packet(Term) -> Packet that returns a
%%    binary consisting of a 4-byte length header N followed by N
%%    bytes of data produced by calling term_to_binary(Term).

%% Need to packetize
term_to_packet(Term) ->
    BinaryTerm = erlang:term_to_binary(Term),
    Length = erlang:byte_size(BinaryTerm),
    <<Length:32, BinaryTerm:Length/bytes>>.
    
%% 3. Write the inverse function packet_to_term(Packet) -> Term that
%%    is the inverse of the previous function.
packet_to_term(<<Length:32, BinaryTerm:Length/bytes>>) ->
    erlang:binary_to_term(BinaryTerm).

packet_test(Term) ->
    Term = packet_to_term(term_to_packet(Term)).

%% 4. Write some tests in the style of Adding Tests to Your Code, on
%%    page 46, to test that the previous two functions can correctly
%%    encode terms into packets and recover the original terms by
%%    decoding the packets.
test() ->
    Squared = fun (X) -> X * X end,
    packet_test(Squared),
    packet_test(32),
    packet_test(<<"Goodbye, Joe.">>),
    <<0:7, 1:1>> = reverse_byte(<<1:1, 0:7>>).

%% 5. Write a function to reverse the bits in a binary.
reverse_byte(<<A:1, B:1, C:1, D:1, E:1, F:1, G:1, H:1>>) ->
    <<H:1,G:1,F:1,E:1,D:1,C:1,B:1,A:1>>;
reverse_byte(<<>>) -> <<>>.

reverse_bitlist([]) -> [];
reverse_bitlist([H|T]) ->
    [reverse_byte(H) || reverse_bitlist(T)].

reverse_bits(Binary) ->
    erlang:list_to_binary(reverse_bitlist(erlang:binary_to_list(Binary))).



